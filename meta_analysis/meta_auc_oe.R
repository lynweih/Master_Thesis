library(data.table)
library(stringr)
library(future.apply)
library(metamisc)
library(ggplot2)
library(pROC)
library(dplyr)


# List all dataset file paths
file_list <- list.files(path = "~/master_thesis/Multilevel/mls/probs/", pattern = "*.csv.gz", full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  # Extracts filename without extension

process_dataset <- function(df, dataset_name) {
  metrics_list <- list()
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  
  for (i in 1:100) {  
    model <- paste0("model_", i)
    df$probs <- df[[model]]
    
    # Group by center and compute metrics
    center_metrics <- df %>%
      group_by(center) %>%
      summarise(
        Dataset = dataset_name,
        Model = model,
        expect_prob = sum(probs),
        pos = sum(outcome1 == 1),
        AUC = tryCatch({
          as.numeric(auc(outcome1, probs))
        }, error = function(e) NA)
      )
    
    metrics_list[[i]] <- center_metrics
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  return(rbindlist(metrics_list, fill = TRUE))
}


# Set up parallel processing
num_cores <- parallel::detectCores() - 1
plan(multisession, workers = num_cores)

# Read and process all datasets in parallel
all_metrics <- future_lapply(seq_along(file_list), function(i) {
  df <- fread(file_list[i])  # Read dataset
  process_dataset(df, dataset_params[i])  # Process it with dataset name
})

# Combine all datasets' results
combined_metrics <- rbindlist(all_metrics)
combined_metrics

write.csv(combined_metrics,"~/master_thesis/Multilevel/mls/probs/metrics_ml_withouthgroupby.csv",row.names = FALSE)

combined_metrics<-read.csv("~/master_thesis/Multilevel/mls/probs/metrics_ml_withouthgroupby.csv")
# Assume sample size per center is known
sample_size <- 10000
rm(meta_auc,meta_results,dataset_data,tau2_df,tau2_values,model_data,model,sample_size,dataset)
rm(meta_oe,tau2_auc)

# Initialize lists for storing meta-analysis results
meta_results <- list()
tau2_values <- list()

# Loop over datasets
for (dataset in unique(combined_metrics$Dataset)) {
  
  # Subset data for the current dataset
  dataset_data <- combined_metrics[combined_metrics$Dataset == dataset,]
  
  # Loop over models within this dataset
  for (model in unique(dataset_data$Model)) {
    
    # Subset data for the current model within this dataset
    model_data <- dataset_data[dataset_data$Model == model,]
    
    # Ensure we have data for this model in this dataset
    if (nrow(model_data) > 0) {
      # Compute standard error for AUC
      #model_data$AUC_se <- ccalc(model_data$AUC, N = sample_size, O = model_data$pos)$theta.se
      
      # Run random-effects meta-analysis for AUC
      meta_auc <- valmeta(
        measure = "OE",
        cstat=model_data$AUC,
        O = model_data$pos, 
        E = model_data$expect_prob,
        N = sample_size,
        slab=model_data$center,
        data=model_data,
        method = "REML"
      )
      
      # Store results
      meta_results[[paste(dataset, model, sep = "_")]] <- meta_auc
      tau2_values[[paste(dataset, model, sep = "_")]] <- meta_auc$fit$tau2
      
      # Print summary
      cat("\nDataset:", dataset, " | Model:", model, "\n")
      print(meta_auc$fit)
    } else {
      cat("\nSkipping Dataset:", dataset, " | Model:", model, " (No Data)\n")
    }
  }
}

# Convert tau2_values list into a data frame
# Extract the dataset name by removing "_prob_" and anything after it
tau2_df <- data.frame(
  Dataset = sub("_model_.*","", names(tau2_values)),  # Extract dataset name
  Tau2 = unlist(tau2_values)  # Extract numeric tau² values
)

write.csv(tau2_df ,"~/master_thesis/results/tau2_oe_ml.csv",row.names = FALSE)

library(dplyr)

tau2_auc<-tau2_df%>%group_by(Dataset)%>%summarise(auc_mean=mean(Tau2))



ggplot(tau2_df, aes(x = Dataset, y = Tau2, fill = Dataset)) +
  geom_boxplot() +
  labs(title = "Boxplot of Tau² Values Across Datasets", 
       x = "Dataset", 
       y = "Tau² (Heterogeneity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
