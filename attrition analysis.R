# ATTRITION ANALYSIS — THESIS KEY VARIABLES----
# For each thesis variable, compares outcome means (age, LS21, LS24,
# BMI21, BMI24) between observed vs missing groups across all 4 samples.

library(dplyr)
library(tidyr)
library(ggplot2)


# SECTION 1: CORE FUNCTION----
# Same structure as summarise_by_var, but groups by observed vs missing only.

attrition_by_var <- function(data, var_name, sample_name) {
  total_n <- nrow(data)
  data %>%
    mutate(.group = if_else(is.na(.data[[var_name]]), "NA", "Observed")) %>%
    group_by(.group) %>%
    summarise(
      n          = n(),
      pct        = round(n() / total_n * 100, 1),
      age_mean   = round(mean(age_2021_imputed, na.rm = TRUE), 1),
      age_sd     = round(sd(age_2021_imputed,   na.rm = TRUE), 1),
      LS21_mean  = round(mean(LS21,  na.rm = TRUE), 2),
      LS21_sd    = round(sd(LS21,    na.rm = TRUE), 2),
      LS24_mean  = round(mean(LS24,  na.rm = TRUE), 2),
      LS24_sd    = round(sd(LS24,    na.rm = TRUE), 2),
      BMI21_mean = round(mean(BMI_21, na.rm = TRUE), 1),
      BMI21_sd   = round(sd(BMI_21,   na.rm = TRUE), 1),
      BMI24_mean = round(mean(BMI_24, na.rm = TRUE), 1),
      BMI24_sd   = round(sd(BMI_24,   na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    rename(group = .group) %>%
    mutate(variable = var_name, sample = sample_name, .before = 1)
}



# SECTION 2: RUN ATTRITION FOR ALL VARIABLES x ALL SAMPLES----

# Explicit calls — one per variable per sample, then stacked.
# Add/remove rows if your samples or variables change.

## --- BMI_21_label ------------------------------------------------------------
attr_BMI_cat_ds      <- attrition_by_var(ds, "BMI_21_label", "Full cleaned")
attr_BMI_cat_crude   <- attrition_by_var(crude, "BMI_21_label", "Crude")
attr_BMI_cat_res     <- attrition_by_var(restrictive, "BMI_21_label", "Restrictive")
attr_BMI_cat_rawres  <- attrition_by_var(raw_res, "BMI_21_label", "Raw restrictive")

## --- obePersist --------------------------------------------------------------
attr_obePersist_ds      <- attrition_by_var(ds, "obePersist", "Full cleaned")
attr_obePersist_crude   <- attrition_by_var(crude, "obePersist", "Crude")
attr_obePersist_res     <- attrition_by_var(restrictive, "obePersist", "Restrictive")
attr_obePersist_rawres  <- attrition_by_var(raw_res, "obePersist", "Raw restrictive")

## --- CWP_21 ------------------------------------------------------------------
attr_CWP_ds      <- attrition_by_var(ds,  "CWP_21", "Full cleaned")
attr_CWP_crude   <- attrition_by_var(crude,           "CWP_21", "Crude")
attr_CWP_res     <- attrition_by_var(restrictive,     "CWP_21", "Restrictive")
attr_CWP_rawres  <- attrition_by_var(raw_res, "CWP_21", "Raw restrictive")

## --- AWP_21 ------------------------------------------------------------------
attr_AWP_ds      <- attrition_by_var(ds,              "AWP_21", "Full cleaned")
attr_AWP_crude   <- attrition_by_var(crude,           "AWP_21", "Crude")
attr_AWP_res     <- attrition_by_var(restrictive,     "AWP_21", "Restrictive")
attr_AWP_rawres  <- attrition_by_var(raw_res, "AWP_21", "Raw restrictive")

## --- typology_child ----------------------------------------------------------
attr_typChild_ds      <- attrition_by_var(ds,              "typology_child", "Full cleaned")
attr_typChild_crude   <- attrition_by_var(crude,           "typology_child", "Crude")
attr_typChild_res     <- attrition_by_var(restrictive,     "typology_child", "Restrictive")
attr_typChild_rawres  <- attrition_by_var(raw_res, "typology_child", "Raw restrictive")

## --- typology_adult ----------------------------------------------------------
attr_typAdult_ds      <- attrition_by_var(ds,              "typology_adult", "Full cleaned")
attr_typAdult_crude   <- attrition_by_var(crude,           "typology_adult", "Crude")
attr_typAdult_res     <- attrition_by_var(restrictive,     "typology_adult", "Restrictive")
attr_typAdult_rawres  <- attrition_by_var(raw_res, "typology_adult", "Raw restrictive")

## --- parentPhys_cat ----------------------------------------------------------
attr_parentPhys_ds      <- attrition_by_var(ds,              "parentPhys_cat", "Full cleaned")
attr_parentPhys_crude   <- attrition_by_var(crude,           "parentPhys_cat", "Crude")
attr_parentPhys_res     <- attrition_by_var(restrictive,     "parentPhys_cat", "Restrictive")
attr_parentPhys_rawres  <- attrition_by_var(raw_res, "parentPhys_cat", "Raw restrictive")

## --- diplUd_21_bin ----------------------------------------------------------
attr_diplUd_21_ds      <- attrition_by_var(ds, "diplUd_21", "Full cleaned")
attr_diplUd_21_crude   <- attrition_by_var(crude,           "diplUd_21", "Crude")
attr_diplUd_21_res     <- attrition_by_var(restrictive,     "diplUd_21", "Restrictive")
attr_diplUd_21_rawres  <- attrition_by_var(raw_res, "diplUd_21", "Raw restrictive")






# SECTION 3: COMBINED TABLE----
attrition_all <- bind_rows(
  attr_BMI_cat_ds,     attr_BMI_cat_crude,     attr_BMI_cat_res,     attr_BMI_cat_rawres,
  attr_obePersist_ds,  attr_obePersist_crude,  attr_obePersist_res,  attr_obePersist_rawres,
  attr_CWP_ds,         attr_CWP_crude,         attr_CWP_res,         attr_CWP_rawres,
  attr_AWP_ds,         attr_AWP_crude,         attr_AWP_res,         attr_AWP_rawres,
  attr_typChild_ds,    attr_typChild_crude,     attr_typChild_res,    attr_typChild_rawres,
  attr_typAdult_ds,    attr_typAdult_crude,     attr_typAdult_res,    attr_typAdult_rawres,
  attr_parentPhys_ds,  attr_parentPhys_crude,   attr_parentPhys_res,  attr_parentPhys_rawres,
  attr_diplUd_21_ds, attr_diplUd_21_crude, attr_diplUd_21_res, attr_diplUd_21_rawres
)

View(attrition_all)
# write.csv(attrition_all, "attrition_analysis.csv", row.names = FALSE)



# SECTION 4: MISSINGNESS OVERVIEW TABLE----
# n missing and % missing per variable per sample — good for thesis appendix.
missing_overview <- attrition_all %>%
  filter(group == "Missing") %>%
  dplyr::select(variable, sample, n, pct) %>%
  rename(n_missing = n, pct_missing = pct) %>%
  pivot_wider(
    names_from  = sample,
    values_from = c(n_missing, pct_missing)
  )

cat("\n", strrep("=", 70), "\n")
cat(" MISSINGNESS OVERVIEW — Thesis Variables by Sample\n")
cat(strrep("=", 70), "\n")
print(as.data.frame(missing_overview), row.names = FALSE)



# SECTION 5: CONSOLE PRINT — ONE BLOCK PER VARIABLE----


print_attrition <- function(tbl, title) {
  cat("\n", strrep("=", 70), "\n")
  cat(" ATTRITION:", title, "\n")
  cat(strrep("=", 70), "\n")
  print(as.data.frame(tbl), row.names = FALSE)
}

print_attrition(
  bind_rows(attr_BMI_cat_ds, attr_BMI_cat_crude, attr_BMI_cat_res, attr_BMI_cat_rawres),
  "BMI category 2021 (BMI_21_label)"
)
print_attrition(
  bind_rows(attr_obePersist_ds, attr_obePersist_crude, attr_obePersist_res, attr_obePersist_rawres),
  "Obesity persistence (obePersist)"
)
print_attrition(
  bind_rows(attr_CWP_ds, attr_CWP_crude, attr_CWP_res, attr_CWP_rawres),
  "Childhood weight perception (CWP_21)"
)
print_attrition(
  bind_rows(attr_AWP_ds, attr_AWP_crude, attr_AWP_res, attr_AWP_rawres),
  "Adulthood weight perception (AWP_21)"
)
print_attrition(
  bind_rows(attr_typChild_ds, attr_typChild_crude, attr_typChild_res, attr_typChild_rawres),
  "Typology child (typology_child)"
)
print_attrition(
  bind_rows(attr_typAdult_ds, attr_typAdult_crude, attr_typAdult_res, attr_typAdult_rawres),
  "Typology adult (typology_adult)"
)
print_attrition(
  bind_rows(attr_parentPhys_ds, attr_parentPhys_crude, attr_parentPhys_res, attr_parentPhys_rawres),
  "Parental body size category (parentPhys_cat)"
)

print_attrition(
  bind_rows(attr_diplUd_21_ds, attr_diplUd_21_crude, attr_diplUd_21_res, attr_diplUd_21_rawres),
  "Nursing diploma education (diplUd_21)"
)



# SECTION 6: VISUALISATION — extends your existing plot_na_comparison()----
# Same logic as before but now:
#   - lines split by Observed vs Missing (not just NA group)
#   - one plot per sample

# Overall sample means (reference dotted line)
make_sample_means <- function(data, sample_name) {
  data %>%
    summarise(
      age_mean   = mean(age_2021_imputed, na.rm = TRUE),
      LS21_mean  = mean(LS21,  na.rm = TRUE),
      LS24_mean  = mean(LS24,  na.rm = TRUE),
      BMI21_mean = mean(BMI_21, na.rm = TRUE),
      BMI24_mean = mean(BMI_24, na.rm = TRUE)
    ) %>%
    mutate(sample = sample_name)
}

sample_means <- bind_rows(
  make_sample_means(ds, "Full cleaned"),
  make_sample_means(crude, "Crude"),
  make_sample_means(restrictive, "Restrictive"),
  make_sample_means(raw_res, "Raw restrictive")
) %>%
  pivot_longer(cols = ends_with("_mean"),
               names_to  = "outcome_raw",
               values_to = "overall_mean") %>%
  mutate(outcome = recode(outcome_raw,
                          "age_mean"   = "Age",
                          "LS21_mean"  = "LS 2021",
                          "LS24_mean"  = "LS 2024",
                          "BMI21_mean" = "BMI 2021",
                          "BMI24_mean" = "BMI 2024")) %>%
  dplyr::select(sample, outcome, overall_mean)

# Long format for plotting
plot_data_attr <- attrition_all %>%
  dplyr::select(sample, variable, group,
                age_mean, LS21_mean, LS24_mean, BMI21_mean, BMI24_mean) %>%
  pivot_longer(cols = ends_with("_mean"),
               names_to  = "outcome_raw",
               values_to = "mean_val") %>%
  mutate(outcome = recode(outcome_raw,
                          "age_mean"   = "Age",
                          "LS21_mean"  = "LS 2021",
                          "LS24_mean"  = "LS 2024",
                          "BMI21_mean" = "BMI 2021",
                          "BMI24_mean" = "BMI 2024"),
         outcome = factor(outcome,
                          levels = c("Age", "LS 2021", "LS 2024", "BMI 2021", "BMI 2024")),
         sample  = factor(sample,
                          levels = c("Full cleaned", "Crude", "Restrictive", "Raw restrictive")))

plot_attrition <- function(selected_sample) {
  
  pd  <- plot_data_attr %>% filter(sample == selected_sample)
  ref <- sample_means   %>% filter(sample == selected_sample)
  
  ggplot(pd, aes(x = variable, y = mean_val,
                 group = group, color = group, linetype = group)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_hline(data = ref,
               aes(yintercept = overall_mean),
               color = "grey50", linetype = "dotted", linewidth = 0.5,
               inherit.aes = FALSE) +
    facet_wrap(~outcome, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c("Observed" = "#2166ac", "NA" = "#d73027")) +
    scale_linetype_manual(values = c("Observed" = "solid", "NA" = "dashed")) +
    labs(
      title    = paste0("Attrition effect analysis: observed vs NA responses — ", selected_sample),
      subtitle = "Dotted line = overall sample mean | Blue = observed responses, Red = NA responses",
      x        = "Variables",
      y        = "Mean value",
      color    = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
      strip.text       = element_text(face = "bold"),
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}

plot_attrition("Full cleaned")
plot_attrition("Crude")
plot_attrition("Restrictive")
plot_attrition("Raw restrictive")


# SECTION 6: COMBINED PLOT — ALL SAMPLES SIDE BY SIDE----

plot_attrition_combined <- function(selected_samples = c("Crude", "Restrictive", "Raw restrictive")) {
  
  pd <- plot_data_attr %>%
    filter(sample %in% selected_samples) %>%
    mutate(sample = factor(sample, levels = selected_samples))
  
  ref <- sample_means %>%
    filter(sample %in% selected_samples) %>%
    mutate(
      sample  = factor(sample, levels = selected_samples),
      outcome = factor(outcome, levels = c("Age", "LS 2021", "LS 2024", "BMI 2021", "BMI 2024"))
    )
  
  ggplot(pd, aes(x = variable, y = mean_val,
                 group = group, color = group, linetype = group)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    geom_hline(data = ref,
               aes(yintercept = overall_mean),
               color = "grey50", linetype = "dotted", linewidth = 0.5,
               inherit.aes = FALSE) +
    facet_grid(outcome ~ sample, scales = "free_y") +
    scale_color_manual(values = c("Observed" = "#2166ac", "NA" = "#d73027")) +
    scale_linetype_manual(values = c("Observed" = "solid", "NA" = "dashed")) +
    labs(
      title    = "Attrition effect analysis: observed vs NA responses",
      subtitle = "Dotted line = overall sample mean | Blue = observed responses, Red = NA responses",
      x        = NULL,
      y        = "Mean value",
      color    = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      strip.text       = element_text(face = "bold"),
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}

p_attrition <- plot_attrition_combined(
  selected_samples = c("Crude", "Restrictive", "Raw restrictive")
)

p_attrition

ggsave("attrition_combined.png", plot = p_attrition,
       width = 14, height = 12, dpi = 300)
