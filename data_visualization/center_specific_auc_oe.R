library(tidyverse)

### only for 5_5_1000
## my metrics dataframe has auc and cases(pos), expect_prob group by center
df_center <- read_csv("~/master_thesis/Multilevel/mls/metrics/group_metrics_5_5_1000_center.csv") %>%
  mutate(model_type = "Type of center")

df_nocenter <- read_csv("~/master_thesis/Multilevel/mls/metrics/group_metrics_5_5_1000_nocenter.csv") %>%
  mutate(model_type = "No higher-order variable")

df_small_prev <- read_csv("~/master_thesis/Multilevel/mls/metrics/group_metrics_5_5_1000_small_prevalence.csv") %>%
  mutate(model_type = "Prevalence small training set")

df_full_prev <- read_csv("~/master_thesis/Multilevel/mls/metrics/group_metrics_5_5_1000_prevalence.csv") %>%
  mutate(model_type = "Prevalence full training set")

# Step 2: Combine all into one dataframe
all_df <- bind_rows(df_center, df_nocenter, df_small_prev, df_full_prev)

oncocenter_df <- tibble(
  center = c("AGR", "BCH", "BFR", "BI0", "BI1", "CIT", "GBE", "KUK", "LBE", "LPO", 
             "LSW", "MFR", "MIT", "MSW", "NCI", "NIT", "OIT", "PCR", "RIT", "SIT", "SSW"),
  oncocenter = c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1)
)


# Step 3: Calculate mean and 95% CI for each center and model_type
summary_df_auc <- all_df %>%
  group_by(center, model_type) %>%
  summarise(
    mean_auc = mean(AUC),
    sd_auc = sd(AUC),
    n = n(),
    se = sd_auc / sqrt(n),
    lower = mean_auc - 1.96 * se,
    upper = mean_auc + 1.96 * se,
    .groups = "drop"
  )%>%
  left_join(oncocenter_df, by = "center")

summary_df_oe <- all_df %>%
  group_by(center, model_type) %>%
  summarise(
    mean_oe = mean(pos/expect_prob),
    sd_oe = sd(pos/expect_prob),
    n = n(),
    se = sd_oe / sqrt(n),
    lower = mean_oe - 1.96 * se,
    upper = mean_oe + 1.96 * se,
    .groups = "drop"
  )%>%
  left_join(oncocenter_df, by = "center")



library(ggplot2)
library(ggbeeswarm)  # for better spread than jitter

ggplot(summary_df_auc, aes(x = factor(center, levels = sort(unique(center))), y = mean_auc, color = model_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  scale_color_manual(
    values = c(
      "No higher-order variable" = "lightsteelblue1",    
      "Type of center" = "thistle",       
      "Prevalence full training set" = "palegreen3", 
      "Prevalence small training set" = "wheat1"
    )) +
  coord_flip() +
  ylim(0.5, 1.1) +  # set x-axis (flipped) range from 0 to 2
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    x = "Center",
    y = "Mean AUC (95% CI)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),           # slightly smaller
    plot.margin = margin(5, 5, 5, 5),               # reduce margin around plot
    panel.spacing = unit(1, "lines"),             # tighter spacing
    aspect.ratio = 1.2,                             # narrow the plot horizontally
    legend.position = "none"
  )


ggplot(summary_df_oe, aes(x = factor(center, levels = sort(unique(center))), y = mean_oe, color = model_type)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), width = 0.15) +
  scale_color_manual(
    values = c(
      "No higher-order variable" = "lightsteelblue1",    
      "Type of center" = "thistle",       
      "Prevalence full training set" = "palegreen3", 
      "Prevalence small training set" = "wheat1"
    )) +
  coord_flip() +
  ylim(0.25, 1.75) +  # set x-axis (flipped) range from 0 to 2
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    x = "Center",
    y = "Mean O:E Ratio (95% CI)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),           # slightly smaller
    plot.margin = margin(5, 5, 5, 5),               # reduce margin around plot
    panel.spacing = unit(1, "lines"),             # tighter spacing
    aspect.ratio = 1.2,                             # narrow the plot horizontally
    legend.position = "none"
  )


###










