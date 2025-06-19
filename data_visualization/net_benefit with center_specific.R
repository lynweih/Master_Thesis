### net benefit--- 5 vs 5 1000 

# function to calculate the net benefit per center
calc_net_benefit <- function(df, dataset_name){
  threshold <- 0.1
  models <- paste0("model_", 1:100)
  results <- data.table()
  
  setDT(df)
  
  # Step 1: Treat-all NB per center
  treat_all_lookup <- df[, {
    valid_outcomes <- !is.na(outcome1)
    p <- sum(outcome1[valid_outcomes]) / sum(valid_outcomes)
    list(
      prevalence = p,
      nb_treat_all = p - (1 - p) * (threshold / (1 - threshold))
    )
  }, by = center]
  
  
  # Step 2: Loop over models and centers
  for (j in 1:100) {
    prob_col <- paste0("model_", j)
    
    for (c in unique(df$center)) {
      subdata <- df[center == c]
      probs <- subdata[[prob_col]]
      outcome <- subdata$outcome1
      
      N <- length(outcome)
      TP <- sum(probs >= threshold & outcome == 1)
      FP <- sum(probs >= threshold & outcome == 0)
      NB_model <- (TP/N) - (FP/N) * (threshold / (1 - threshold))
      
      NB_treat_all <- treat_all_lookup[center == c]$nb_treat_all
      
      results <- rbind(results, data.table(
        Dataset = dataset_name,
        Model = models[j],
        center = c,
        NB_model = NB_model,
        NB_treat_all = NB_treat_all,
        outperform = (NB_model > 0) & (NB_model > NB_treat_all)
      ))
    }
  }
  
  return(results)
}

library(data.table)
library(dplyr)
library(readr)

### this is the data with the probabilites like model_1, model_2
df_center <- read_csv("~/master_thesis/Multilevel/mls/probs/test_prob_8_2_1000_center.csv.gz") %>%
  mutate(model_type = "Type of center")

df_nocenter <- read_csv("~/master_thesis/Multilevel/mls/probs/test_prob_8_2_1000_nocenter.csv.gz") %>%
  mutate(model_type = "No higher-order variable")

df_small_prev <- read_csv("~/master_thesis/Multilevel/mls/probs/test_prob_8_2_1000_small_prevalence.csv.gz") %>%
  mutate(model_type = "Prevalence small training set")

df_full_prev <- read_csv("~/master_thesis/Multilevel/mls/probs/test_prob_8_2_1000_prevalence.csv.gz") %>%
  mutate(model_type = "Prevalence full training set")

df_list <- list(
  "Type of center" = df_center,
  "No higher-order variable" = df_nocenter,
  "Prevalence small training set" = df_small_prev,
  "Prevalence full training set" = df_full_prev
)

net_benefit_results <- lapply(names(df_list), function(name) {
  df <- df_list[[name]]
  calc_net_benefit(df, name)
})

all_net_benefit <- rbindlist(net_benefit_results)

oncocenter_df <- tibble(
  center = c("AGR", "BCH", "BFR", "BI0", "BI1", "CIT", "GBE", "KUK", "LBE", "LPO", 
             "LSW", "MFR", "MIT", "MSW", "NCI", "NIT", "OIT", "PCR", "RIT", "SIT", "SSW"),
  oncocenter = c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1)
)


summary_df_nb <- all_net_benefit %>%
  group_by(center, Dataset) %>%
  summarise(
    mean_nb = mean(NB_model),
    sd_nb = sd(NB_model),
    n = n(),
    se = sd_nb / sqrt(n),
    lower = mean_nb - 1.96 * se,
    upper = mean_nb + 1.96 * se,
    nb_treat_all=mean(NB_treat_all),
    .groups = "drop"
  )%>%
  left_join(oncocenter_df, by = "center")


# Get one nb_treat_all value per center for reference lines
nb_lines <- summary_df_nb %>%
  group_by(center) %>%
  summarise(nb_treat_all = unique(nb_treat_all)) %>%
  mutate(x = factor(center, levels = sort(unique(center))))


# Create a consistent ordering of center levels
center_levels <- sort(unique(c(summary_df_nb$center, nb_lines$center)))
summary_df_nb$center <- factor(summary_df_nb$center, levels = center_levels)
nb_lines$x <- factor(nb_lines$center, levels = center_levels)


# Main plot
ggplot(summary_df_nb, aes(x = factor(center, levels = sort(unique(center))), y = mean_nb, color = Dataset)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  # Add reference points for each center's treat_all value
  geom_point(
    data = nb_lines,
    aes(x = x, y = nb_treat_all,shape="Treat All"),
    inherit.aes = FALSE,
    shape = 18,  # diamond shape
    size = 3,
    color = "#D75455"
  ) +
  scale_color_manual(
    values = c(
      "No higher-order variable" = "lightsteelblue1",    
      "Type of center" = "thistle",       
      "Prevalence full training set" = "palegreen3", 
      "Prevalence small training set" = "wheat1"
    )
  ) +
  coord_flip() +
  ylim(0,0.7) +  # adjust as needed
  labs(
    x = "Center",
    y = "Mean Net Benefit (95% CI)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),
    plot.margin = margin(5, 5, 5, 5),
    panel.spacing = unit(1, "lines"),
    aspect.ratio = 1.2,
    legend.position = "none"
  )



