library(dplyr)

# ds <- read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v5.csv")
ds <- read.csv("/Users/siruizhang/Thesis/Data_Lexi_v6 - Copy.csv")
View(ds)
# crude <- read.csv("C:/Users/SZHA0012/Documents/crude sample.csv")
# restrictive <- read.csv("C:/Users/SZHA0012/Documents/crude sample.csv")

# test <-  read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v5.csv")
test <- read.csv("/Users/siruizhang/Thesis/Data_Lexi_v6 - Copy.csv")


#PREP----
ds <- ds %>%
  rename(LS21 = quality_of_life_a_k,
         H21 = height_k,
         W21 = weight_k,
         waist21 = waist_k,
         edu = work_k, #edu beyond nursing edu
         CWP_21 = weight_statements_a_k,
         adolWP21 = weight_statements_b_k,
         youngAWP21 = weight_statements_c_k,
         AWP_21 = weight_statements_d_k,
         momPhys_21 = physique_mom_k,
         dadPhys_21 = physique_dad_k,
         age_2021 = cpr_alder,
         #work: shift type
         daySche_21 = work_schedule_a_k,
         eveSche_21 = work_schedule_b_k,
         nightSche_21 = work_schedule_c_k,
         mixedSche_21 = work_schedule_d_k,
         age_2024 = age,
         LS24 = qol,
         phyHealth_24 = phy_health_v2,
         mentHealth_24 = men_health_v2,
         W24 = weight_k_v2,
         H24 = height_k_v2,
         CWP_24 = weight_statements_a_k_v2,
         AWP_24 = weight_statements_d_k_v2,
         momPhys_24 = physique_mom_k_v2,
         dadPhys_24 = physique_dad_k_v2,
         famInh_24 = inheritage_icd_v2,
         obeInh_24 = inheritage_icd_v3___5
  )


ds <- ds %>%
  mutate(age_2021 = trunc(age_2021),
         age_2024 = trunc(age_2024))
ds <- ds %>%
  mutate(
    age_2021_imputed = if_else(
      is.na(age_2021) & !is.na(age_2024),
      trunc(age_2024)-3,
      trunc(age_2021)
    )
  )
ds <- ds %>%
  mutate(
    age_flag = if_else(
      is.na(age_2021) & !is.na(age_2024),
      "missing 2021",
      if_else(
        !is.na(age_2021) & is.na(age_2024),
        "missing 2024",
        NA_character_
      )
    )
  )



ds <- ds %>%
  filter(age_2021_imputed >= 25,
         cpr_sex==1,
         !is.na(LS21))


#WEIGHT----
##weight rescue----
library(dplyr)
library(gtsummary)

rescue_W21 <- function(w_input, w_anchor) {
  x <- if_else(is.na(w_input) | w_input == 0, w_anchor, w_input)
  x <- if_else(!is.na(x) & x < 0, abs(x), x)
  case_when(
    x > 0  & x < 1   ~ x * 100,
    x >= 2 & x < 10  ~ x * 10,
    x >= 10 & x < 40 ~ w_anchor,
    x >= 40 & x <= 190 ~ x,
    x > 190 & x < 200 ~ NA_real_,
    x >= 200  & x < 1500  ~ x / 10,
    x >= 1500 & x < 10000 ~ x / 100,
    x >= 10000             ~ x / 1000,
    TRUE ~ NA_real_
  )
}

rescue_W24 <- function(w_input, w_anchor) {
  x <- if_else(is.na(w_input) | w_input == 0, w_anchor, w_input)
  case_when(
    x > 0  & x <= 1   ~ x * 100,
    x >= 2 & x <= 10  ~ x * 10,
    x > 10 & x < 40   ~ w_anchor,
    x >= 40 & x <= 190 ~ x,
    x > 190 & x <= 1000  ~ x / 10,
    x > 1000 & x <= 10000 ~ x / 100,
    x > 10000              ~ x / 1000,
    TRUE ~ NA_real_
  )
}

ds <- ds %>%
  mutate(
    TW21_flag = case_when(
      is.na(W21) & is.na(W24)         ~ NA_character_,
      is.na(W21) | W21 == 0           ~ "from_W24",
      W21 < 0                         ~ "negative_flipped",
      W21 > 0  & W21 < 1              ~ "decimal_x100",
      W21 >= 2 & W21 < 10             ~ "single_digit_x10",
      W21 >= 10 & W21 < 40            ~ "from_W24",
      W21 >= 40 & W21 <= 190          ~ "kept",
      W21 > 190 & W21 < 200           ~ "gap_NA",
      W21 >= 200 & W21 < 1500         ~ "div10",
      W21 >= 1500 & W21 < 10000       ~ "div100",
      W21 >= 10000                    ~ "div1000",
      TRUE                            ~ NA_character_
    ),
    
    TW24_flag = case_when(
      is.na(W24) & is.na(W21)         ~ NA_character_,
      is.na(W24) & !is.na(W21)        ~ "from_TW21",
      W24 == 0                        ~ "from_TW21",
      W24 > 0  & W24 <= 1            ~ "decimal_x100",
      W24 >= 2 & W24 <= 10           ~ "single_digit_x10",
      W24 > 10 & W24 < 40            ~ "from_TW21",
      W24 >= 40 & W24 <= 190         ~ "kept",
      W24 > 190 & W24 <= 1000        ~ "div10",
      W24 > 1000 & W24 <= 10000      ~ "div100",
      W24 > 10000                    ~ "div1000",
      TRUE                           ~ NA_character_
    ),
    
    treatedW21 = trunc(rescue_W21(w_input = W21, w_anchor = W24)),
    treatedW24 = trunc(rescue_W24(w_input = W24, w_anchor = treatedW21)),
    
    TW21_flag = if_else(!is.na(treatedW21) & (treatedW21 < 40 | treatedW21 > 190), "out_of_range", TW21_flag),
    TW24_flag = if_else(!is.na(treatedW24) & (treatedW24 < 40 | treatedW24 > 190), "out_of_range", TW24_flag)
  )

# range(ds$treatedW21, na.rm = TRUE)


###SMD analysis, W21----
library(smd)

smd_w21 <- smd(
  x = c(ds$W21, ds$treatedW21),
  g = c(rep("Raw", nrow(ds)), rep("Treated", nrow(ds))),
  na.rm = TRUE
)

print(smd_w21)

data.frame(
  Raw     = unclass(summary(ds$H21)),
  Treated = unclass(summary(ds$treatedH21))
)

###SMD analysis, W24----
smd_w24 <- smd(
  x = c(ds$W24, ds$treatedW24),
  g = c(rep("Raw", nrow(ds)), rep("Treated", nrow(ds))),
  na.rm = TRUE
)

print(smd_w24)








#HEIGHT----
rescue_H21 <- function(h_input, h_anchor) {
  x <- if_else(is.na(h_input) | h_input == 0, h_anchor, h_input)
  case_when(
    x <= 1 ~ h_anchor,
    x > 1 & x < 2  ~ x * 100,
    x >= 2 & x <= 13 ~ h_anchor,
    x > 13 & x <= 20  ~ x * 10,
    x >= 20 & x < 60 ~ h_anchor,
    x >= 60 & x < 100 ~ x + 100,
    x >= 100 & x < 140 ~ h_anchor,
    x >= 140 & x <= 210 ~ x,
    x > 210  & x <= 1000 ~ (x %% 100) + 100,
    x > 1000 ~ as.numeric(substr(as.character(x), 1, 3)),
    TRUE ~ NA_real_
  )
}

ds <- ds %>%
  mutate(
    TH21_flag = case_when(
      is.na(H21) & is.na(H24) ~ NA_character_,
      is.na(H21) | H21 == 0 ~ "from_H24",
      H21 == 1 ~ "from_H24",
      H21 > 1   & H21 < 2 ~ "decimal_x100",
      H21 >= 2  & H21 <= 13 ~ "from_H24",
      H21 > 13  & H21 <= 20 ~ "teens_x10",
      H21 >= 20 & H21 < 60 ~ "from_H24",
      H21 >= 60 & H21 < 100 ~ "missing_leading1",
      H21 >= 100 & H21 < 140 ~ "from_H24",
      H21 >= 140 & H21 <= 210 ~ "kept",
      H21 > 210  & H21 <= 1000 ~ "leading_digit_swap",
      H21 > 1000 ~ "leftmost3",
      TRUE ~ "NA"
    ),
    treatedH21 = trunc(rescue_H21(h_input = H21, h_anchor = H24)),
    TH21_flag = if_else(
      !is.na(treatedH21) & (treatedH21 < 140 | treatedH21 > 210),
      "out_of_range",
      TH21_flag
    )
  )

###SMD analysis----
library(smd)

smd_h21 <- smd(
  x = c(ds$H21, ds$treatedH21),
  g = c(rep("Raw", nrow(ds)), rep("Treated", nrow(ds))),
  na.rm = TRUE
)

print(smd_h21)

data.frame(
  Raw     = unclass(summary(ds$H21)),
  Treated = unclass(summary(ds$treatedH21))
)







#BMI----
ds <- ds %>%
  mutate(
    BMI_21 = if_else(
      treatedW21 >= 40 & treatedW21 <= 190 & treatedH21 >= 140 & treatedH21 <= 210,
      treatedW21 / ((treatedH21 / 100)^2),
      NA_real_
    ),
    BMI_24 = if_else(
      treatedW24 >= 40 & treatedW24 <= 190 & treatedH21 >= 140 & treatedH21 <= 210,
      treatedW24 / ((treatedH21 / 100)^2),
      NA_real_
    )
  )


##labels----
ds <- ds %>%
  mutate(
    BMI_21_label = factor(case_when(
      BMI_21 < 18.5 ~ "Underweight",
      BMI_21 >= 18.5 & BMI_21 < 25 ~ "Healthy",
      BMI_21 >= 25 & BMI_21 < 30 ~ "Overweight",
      BMI_21 >= 30 & BMI_21 < 35 ~ "Obesity I",
      BMI_21 >= 35 & BMI_21 < 40 ~ "Obesity II",
      BMI_21 >= 40 ~ "Obesity III"
    ), levels = c("Healthy", "Underweight", "Overweight", 
                  "Obesity I", "Obesity II", "Obesity III")),
    
    BMI_24_label = factor(case_when(
      BMI_24 < 18.5 ~ "Underweight",
      BMI_24 >= 18.5 & BMI_24 < 25 ~ "Healthy",
      BMI_24 >= 25 & BMI_24 < 30 ~ "Overweight",
      BMI_24 >= 30 & BMI_24 < 35 ~ "Obesity I",
      BMI_24 >= 35 & BMI_24 < 40 ~ "Obesity II",
      BMI_24 >= 40 ~ "Obesity III"
    ), levels = c("Healthy", "Underweight", "Overweight",
                  "Obesity I", "Obesity II", "Obesity III"))
  )


##bin & persistence----
ds <- ds %>%
  mutate(
    obe21_bin = factor(case_when(
      BMI_21 >= 30 ~ "obese",
      BMI_21 < 30  ~ "non-obese"
    ), levels = c("non-obese", "obese")),
    
    obe24_bin = factor(case_when(
      BMI_24 >= 30 ~ "obese",
      BMI_24 < 30  ~ "non-obese"
    ), levels = c("non-obese", "obese")),
    
    ## obesity persistence
    obePersist = factor(case_when(
      obe21_bin == "non-obese" & obe24_bin == "non-obese" ~ "never",
      obe21_bin == "obese" & obe24_bin == "non-obese" ~ "2021 only",
      obe21_bin == "non-obese" & obe24_bin == "obese"     ~ "2024 only",
      obe21_bin == "obese" & obe24_bin == "obese"     ~ "both waves"
    ), levels = c("never", "2021 only", "2024 only", "both waves"))
  )




#parental body size 2021----
ds <- ds %>%
  mutate(
    momPhys_21_large = case_when(
      momPhys_21 >= 1 & momPhys_21 <= 3 ~ 1,
      momPhys_21 >= 4 & momPhys_21 <= 9 ~ 0
    ),
    dadPhys_21_large = case_when(
      dadPhys_21 >= 1 & dadPhys_21 <= 3 ~ 1,
      dadPhys_21 >= 4 & dadPhys_21 <= 9 ~ 0
    ),
    parentPhys_cat = factor(case_when(
      momPhys_21_large == 1 & dadPhys_21_large == 1 ~ "both",
      momPhys_21_large == 1 | dadPhys_21_large == 1 ~ "one parent",
      momPhys_21_large == 0 & dadPhys_21_large == 0 ~ "neither"
    ), levels = c("neither", "one parent", "both"))
  )


ds <- ds %>%
  mutate(
    momPhys_21_large = factor(momPhys_21_large, 
                              levels = c(0, 1),
                              labels = c("not large", "large")),
    dadPhys_21_large = factor(dadPhys_21_large,
                              levels = c(0, 1),
                              labels = c("not large", "large"))
  )

#edu----
ds <- ds %>%
  mutate(
    edu_21 = factor(edu,
                    levels = c(0, 1),
                    labels = c("no", "yes")
    )
  )


#work schedules----
ds <- ds %>%
  mutate(daySche_21_grp = factor(case_when(
    daySche_21 == 1 ~ "yes",
    daySche_21 == 0 ~ "no"), 
    levels = c("no", "yes"))
  )

ds <- ds %>%
  mutate(nightSche_21_grp = factor(case_when(
    nightSche_21 == 1 ~ "yes",
    nightSche_21 == 0 ~ "no"), 
    levels = c("no", "yes"))
  )

ds <- ds %>%
  mutate(eveSche_21_grp = factor(case_when(
    eveSche_21 == 1 ~ "yes",
    eveSche_21 == 2 ~ "no"),
    levels = c("no", "yes"))
  )

ds <- ds %>%
  mutate(mixedSche_21_grp = factor(case_when(
    mixedSche_21 == 1 ~ "yes",
    mixedSche_21 == 0 ~ "no"), 
    levels = c("no", "yes")
  )
  )

#work schedule categories----
ds <- ds %>%
  mutate(
    workSche_cat = case_when(
      mixedSche_21_grp == "yes" ~ "rotating",
      nightSche_21_grp == "yes" ~ "regular night",
      daySche_21_grp == "yes" | eveSche_21_grp == 1 ~ "regular day-or-eve",
      TRUE ~ NA_character_
    ),
    workSche_cat = factor(
      workSche_cat,
      levels = c("regular day-or-eve", "regular night", "rotating")
    )
  )

# table(ds$workSche_cat, useNA = "ifany")
# sum(ds$mixedSche_21_grp=="yes", na.rm = TRUE)

#family history of overweight----
ds <- ds %>%
  mutate(
    obeInh_24 = factor(case_when(
      obeInh_24 == 0 ~ "no",
      obeInh_24 == 1 ~ "yes"
    ), levels = c("no", "yes"))
  )

#weight perceptions----
ds <- ds %>%
  mutate(
    CWP_21 = factor(case_when(
      CWP_21 == 1 ~ "heavier",
      CWP_21 == 2 ~ "thinner",
      CWP_21 == 3 ~ "no difference"
    ),
    levels = c("no difference", "heavier", "thinner"))
  )

ds <- ds %>%
  mutate(
    adolWP21 = factor(case_when(
      adolWP21 == 1 ~ "heavier",
      adolWP21 == 2 ~ "thinner",
      adolWP21 == 3 ~ "no difference"
    ), levels = c("no difference", "heavier", "thinner"))
  )

ds <- ds %>%
  mutate(
    youngAWP21  = factor(case_when(
      youngAWP21 == 1 ~ "heavier",
      youngAWP21 == 2 ~ "thinner",
      youngAWP21 == 3 ~ "no difference"
    ), levels = c("no difference", "heavier", "thinner"))
  )

ds <- ds %>%
  mutate(
    AWP_21 = factor(case_when(
      AWP_21 == 1 ~ "heavier",
      AWP_21 == 2 ~ "thinner",
      AWP_21 == 3 ~ "no difference"
    ),
    levels = c("no difference", "heavier", "thinner"))
  )


#AT----
ds <- ds %>%
  mutate(
    typology_adult = factor(case_when(
      obe21_bin == "non-obese" & AWP_21 %in% c("no difference", "thinner") ~ "concordant healthy",
      obe21_bin == "obese" & AWP_21 == "heavier"  ~ "concordant heavy",
      obe21_bin == "non-obese" & AWP_21 == "heavier" ~ "over-perceiver",
      obe21_bin == "obese" & AWP_21 == "no difference" ~ "under-perceiver",
      obe21_bin == "obese" & AWP_21 == "thinner" ~ "under-perceiver"
    ), levels = c("concordant healthy", "concordant heavy",
                  "over-perceiver", "under-perceiver"))
  )

#obesity trajectory----
ds <- ds %>%
  mutate(
    earlyLife_heavier = rowSums(
      dplyr::select(., CWP_21, adolWP21) == "heavier", na.rm = TRUE
    ) > 0,
    ob_trajectory = case_when(
      !earlyLife_heavier & obe21_bin == "non-obese" ~ "never obese",
      earlyLife_heavier & obe21_bin == "non-obese" ~ "early-life obesity only",
      !earlyLife_heavier & obe21_bin == "obese" ~ "adult-onset by 2021",
      earlyLife_heavier & obe21_bin == "obese" ~ "persistent obesity through 2021",
      TRUE ~ NA_character_
    ),
    ob_trajectory = factor(
      ob_trajectory,
      levels = c("never obese", "early-life obesity only",
                 "adult-onset by 2021", "persistent obesity through 2021")
    )
  )

#LS----
##labels----
ds <- ds %>%
  mutate(across(c(LS21, LS24), 
                ~case_when(
                  . <= 4 ~ "dissatisfied",
                  . == 5 ~ "neutral",
                  . >= 6 ~ "satisfied"
                ), 
                .names = "{.col}_label"))

##make factors----
ds <- ds %>%
  mutate(
    LS21_cat = factor(LS21_label, 
                      levels = c("dissatisfied", "neutral", "satisfied"),  ordered = TRUE),
    LS24_cat = factor(LS24_label, 
                      levels = c("dissatisfied", "neutral", "satisfied"), ordered = TRUE)
  )


#sample data set updates----
##crude ----
crude <- ds %>%
  filter(
    !is.na(BMI_21),
    !is.na(LS21),
    !is.na(LS24)
  )
nrow(crude)
View(ds)filt

## Restrictive----
restrictive <- ds %>%
  filter(
    !is.na(BMI_21),
    !is.na(BMI_24),
    !is.na(LS21),
    !is.na(LS24),
    !is.na(CWP_21),
    !is.na(adolWP21),
    !is.na(AWP_21),
    !is.na(momPhys_21),
    !is.na(dadPhys_21),
    !is.na(age_2021_imputed)
  )
nrow(restrictive)

# raw_res <- ds %>%
#   filter(
#     !is.na(BMI_21),
#     !is.na(BMI_24),
#     !is.na(LS21),
#     !is.na(LS24),
#     !is.na(CWP_21),
#     !is.na(AWP_21),
#     !is.na(momPhys_21),
#     !is.na(dadPhys_21),
#     !is.na(diplUd_21),
#     !is.na(obeInh_24),
#     !is.na(age_2021_imputed)
#   )
# nrow(raw_res)



library(tidyverse)
# install.packages("MASS")
library(MASS)


# table(ds$lgbt_binary, useNA = "always")
# table(ds$eveSche_21, useNA = "always")





#***----
#FUNCTIONS----
run_polr <- function(data, model_name = "", formula) {
  
  library(MASS)
  library(brant)
  library(DescTools)
  library(tidyverse)
  
  model <- polr(formula, data = data, Hess = TRUE)
  
  cat("\n====", model_name, "====\n")
  print(summary(model))
  
  cat("\nOdds Ratios:\n")
  print(exp(cbind(OR = coef(model), confint(model))))
  
  cat("\nBrant Test:\n")
  print(brant(model))
  
  cat("\nPseudo R-squared:\n")
  print(PseudoR2(model, which = c("McFadden", "Nagelkerke")))
  
  return(invisible(model))
}

run_margins <- function(model, variable) {
  
  library(marginaleffects)
  
  pred <- avg_predictions(model,
                          variables = variable,
                          type = "probs") %>%
    as.data.frame() %>%
    mutate(
      group = factor(group,
                     levels = c("dissatisfied", "neutral", "satisfied"))
    )
  
  print(pred)
  return(invisible(pred))
}

plot_margins <- function(margins_data, x_var, x_label = x_var, title = "") {
  
  margins_data %>%
    mutate(
      group = factor(group, levels = c("dissatisfied", "neutral", "satisfied")),
      x = factor(.data[[x_var]])
    ) %>%
    ggplot(aes(x = x, y = estimate, fill = group)) +
    geom_bar(stat = "identity", position = "stack", width = 0.5) +
    geom_text(aes(label = scales::percent(estimate, accuracy = 0.1)),
              position = position_stack(vjust = 0.5),
              size = 3.5, color = "white", fontface = "bold") +
    scale_fill_manual(values = c(
      "dissatisfied" = "#C0504D",
      "neutral"      = "#9BB8D4",
      "satisfied"    = "#366092"
    )) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = title,
      subtitle = "Adjusted for baseline life satisfaction (2021)",
      x = x_label,
      y = "Predicted probability",
      fill = "Life satisfaction (2024)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


# library(ggplot2)
# 
# ggplot(crude, aes(x = LS24)) +
#   geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#   scale_x_continuous(breaks = 0:10) +
#   labs(title = "Distribution of Life Satisfaction at Baseline (2021)",
#        x = "Life Satisfaction (0-10)",
#        y = "Count") +
#   theme_minimal()



#H1----
# library(ggplot2)
# 
# ggplot(crude, aes(x = BMI_21, y = LS24)) +
#   geom_point(alpha = 0.1, size = 0.7, color = "grey40") +
#   geom_smooth(method = "lm", formula = y ~ x, 
#               color = "#4C6B8A", linewidth = 1, se = TRUE) +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
#               color = "#C9772E", linewidth = 1, se = TRUE, linetype = "dashed") +
#   geom_vline(xintercept = 30, color = "red", linetype = "dotted", linewidth = 0.6) +
#   labs(x = "BMI at baseline", y = "Life satisfaction (0–10 scale)",
#        caption = "Solid line: linear fit. Dashed line: quadratic fit. Red line marks obesity threshold (BMI = 30).") +
#   theme_minimal(base_size = 12)


#crude ----
H1_crude <- crude %>% run_polr(
  "H1_crude",
  LS24_cat ~ obe21_bin
)
nobs(H1_crude)
# install.packages("marginaleffects")

margPre_H1_crude <- run_margins(H1_crude, "obe21_bin")

plot_margins(margPre_H1_crude, "obe21_bin",
             x_label = "Obesity Status (2021)",
             title = "Predicted life satisfaction category (2024) by obesity status, crude sample"
)
summary(H1_crude)


#restrictive----
H1_res <- restrictive %>% run_polr(
  "H1_res",
  LS24_cat ~ obe21_bin
)
nobs(H1_res)
margPre_H1_res <- run_margins(H1_res, "obe21_bin")

plot_margins(margPre_H1_res, "obe21_bin",
             x_label = "Obesity Status (2021)",
             title = "Predicted probability of life satisfaction (2024) by obesity status, restrictive sample")


# H1_raw_res <- raw_restrictive %>% run_polr(
#   "H1_raw_res",
#   LS24_cat ~ obe21_bin
# )
# nobs(H1_raw_res)

# ###adjusted
# H1_resAdj <- restrictive %>% run_polr(
#   "H1_resAdj",
#   LS24_cat ~ obe21_bin + LS21_cat
# )
# nobs(H1_resAdj)
# margPred_H1_resAdj <- run_margins(H1_resAdj, "obe21_bin")


# ##raw restrictive
# ###unadjusted
# H1_raw_res <- raw_res %>% run_polr(
#   "H1_raw_res",
#   LS24_cat ~ obe21_bin
# )
# nobs(H1_raw_res)
# margPre_H1_raw <- run_margins(H1_raw_res, "obe21_bin")
# 
# 
# ###adjusted
# H1_raw_resAdj <- raw_res %>% run_polr(
#   "H1_raw_resAdj",
#   LS24_cat ~ obe21_bin + LS21_cat
# )
# nobs(H1_raw_resAdj)
# margPre_H1_raw_resAdj <- run_margins(H1_raw_resAdj, "obe21_bin")
# 
# plot_margins(margPre_H1_raw_resAdj, "obe21_bin",
#              x_label = "Obesity Status (2021)",
#              title = "Predicted probability of life satisfaction (2024) by obesity status, raw restrictive sample")


#H2----
##obesity trajectory----
###crude----
H2_obTraj_crude <- crude %>%
  run_polr(
    "H2_obTraj_crude",
    LS24_cat ~ ob_trajectory + age_2021_imputed
  )
nobs(H2_obTraj_crude)
margPre_H2_obTraj_crude <- run_margins(H2_obTraj_crude, "ob_trajectory")
###restrictive----
H2_obTraj_res <- restrictive %>%
  run_polr(
    "H2_obTraj_res",
    LS24_cat ~ ob_trajectory + age_2021_imputed
  )
nobs(H2_obTraj_res)
margPre_H2_obTraj_res <- run_margins(H2_obTraj_res, "ob_trajectory")




##severity----
###crude----
H2_severity_crude <- crude %>% run_polr(
  "H2_severity_crude",
  LS24_cat ~ BMI_21_label + age_2021_imputed
)
nobs(H2_severity_crude)
margPre_H2_severity_crude <- run_margins(H2_severity_crude, "BMI_21_label")

###restrictive----
H2_severity_res <- restrictive %>% run_polr(
  "H2_severity_res",
  LS24_cat ~ BMI_21_label + age_2021_imputed
)
nobs(H2_severity_res)
margPre_H2_severity_res <- run_margins(H2_severity_res, "BMI_21_label")




##persistence----
###crude----
H2_obePersist_crude <- crude %>% run_polr(
  "H2_obePersist_crude",
  LS24_cat ~ obePersist + age_2021_imputed
)
nobs(H2_obePersist_crude)
margPre_H2_obePersist_crude <- run_margins(H2_obePersist_crude, "obePersist")

plot_margins(margPre_H2_obePersist_crude, "obePersist",
             x_label = "Obesity persistence",
             title = "Predicted probability of life satisfaction (2024) by obesity persistence, crude sample")


###restrictive----
H2_obePersist_res <- restrictive %>% run_polr(
  "H2_obePersist_res",
  LS24_cat ~ obePersist + age_2021_imputed
)
nobs(H2_obePersist_res)
margPre_H2_obePersist_res <- run_margins(H2_obePersist_res, "obePersist")
nobs(H2_obePersist_res)



##AT----
###crude----
H2_AT_crude <- crude %>% run_polr(
  "H2_AT_crude",
  LS24_cat ~ typology_adult + age_2021_imputed
)
nobs(H2_AT_crude)
margPre_H2_AT_crude <- run_margins(H2_AT_crude, "typology_adult")

plot_margins(margPre_typology_AWP, "typology_adult",
             x_label = "Adulthood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by adulthood weight status-perception typology, crude sample")

crude %>%
  count(typology_adult)

###restrictive----
H2_AT_res <- restrictive %>% run_polr(
  "H2_AT_res",
  LS24_cat ~ typology_adult + age_2021_imputed
)
nobs(H2_AT_res)
margPre_H2_AT_res <- run_margins(H2_AT_res, "typology_adult")



#H3----
H3_CWP_21 <- crude %>% run_polr("H3_CWP", LS24_cat ~ obe21_bin * CWP_21)
H3_AWP_21 <- crude %>% run_polr("H3_AWP", LS24_cat ~ obe21_bin * AWP_21)
H3_mom_21 <- crude %>% run_polr("H3_mom", LS24_cat ~ obe21_bin * momPhys_21_large)
H3_dad_21 <- crude %>% run_polr("H3_dad", LS24_cat ~ obe21_bin * dadPhys_21_large)



#H4----
H4 <- crude %>% run_polr(
  "H4",
  LS24_cat ~ obe21_bin + age_2021_imputed + momPhys_21_large + dadPhys_21_large
)

ds %>% filter(!is.na(LS21) & !is.na(LS24)) %>% nrow()
nrow(ds)

#non-participation analysis----
library(dplyr)
library(tableone)

## excluded = the eligibility sample (ds), restricted to those who FAIL
## crude's defining criteria (missing BMI21 or missing LS24)
excluded <- ds %>%
  filter(is.na(BMI_21) | is.na(LS24))

## sanity check: these two groups should partition ds exactly, with no overlap and no gaps
nrow(crude) + nrow(excluded) == nrow(ds)

###excluded 16210----
build_schedule <- function(df) {
  df %>%
    mutate(
      workSchedule_3cat = case_when(
        mixedSche_21 == 1 ~ "rotating",
        nightSche_21 == 1 ~ "regular night",
        daySche_21 == 1 | eveSche_21 == 1 ~ "regular day-or-evening",
        TRUE ~ NA_character_
      )
    )
}

crude_tagged    <- build_schedule(crude)    %>% mutate(group = "Crude sample")
excluded_tagged <- build_schedule(excluded) %>% mutate(group = "Excluded")

compare_df <- bind_rows(crude_tagged, excluded_tagged)

smd_vars <- c("BMI_21", "LS21", "age_2021_imputed", "diplUd_21", "workSchedule_3cat")

tab1 <- CreateTableOne(vars = smd_vars, strata = "group", data = compare_df,
                       test = FALSE, includeNA = TRUE)
print(tab1, smd = TRUE)

###reasons of exclusion----
excluded_reasons <- ds %>%
  filter(is.na(BMI_21) | is.na(LS24)) %>%
  mutate(
    exclusion_reason = case_when(
      is.na(LS24) & is.na(BMI_21) ~ "Both missing (LS24 and BMI21)",
      is.na(LS24) & !is.na(BMI_21) ~ "Lost to follow-up only (missing LS24)",
      !is.na(LS24) & is.na(BMI_21) ~ "Missing/implausible baseline BMI only",
      TRUE ~ NA_character_
    )
  )

table(excluded_reasons$exclusion_reason)

###SMD----
library(tableone)

excluded_reasons <- ds %>%
  filter(is.na(BMI_21) | is.na(LS24)) %>%
  mutate(
    exclusion_reason = case_when(
      is.na(LS24) & is.na(BMI_21) ~ "Both missing (LS24 and BMI21)",
      is.na(LS24) & !is.na(BMI_21) ~ "Lost to follow-up only (missing LS24)",
      !is.na(LS24) & is.na(BMI_21) ~ "Missing/implausible baseline BMI only",
      TRUE ~ NA_character_
    )
  )

lost_to_followup <- excluded_reasons %>% filter(exclusion_reason == "Lost to follow-up only (missing LS24)")
missing_exposure  <- excluded_reasons %>% filter(exclusion_reason == "Missing/implausible baseline BMI only")

two_way <- bind_rows(
  crude %>% mutate(group = "Crude sample"),
  lost_to_followup %>% mutate(group = "Lost to follow-up")
)

tab_two <- CreateTableOne(vars = c("age_2021_imputed", "LS21"),
                          strata = "group", data = two_way, test = FALSE)
print(tab_two, smd = TRUE)

#※※※※※※※※※※DIVIDER※※※※※※※※※-------------------------------------------------












H2_CWP <- crude %>% run_polr(
  "H2_CWP",
  LS24_cat ~ obe21_bin * CWP_21 + LS21_cat
)
nobs(H2_CWP)

margPre_H2_CWP <- run_margins(H2_CWP, "CWP_21")

plot_margins(
  margPre_H2_CWP, "CWP_21",
  x_label = "Childhood Weight Perception (2021)",
  title = "Predicted probability of life satisfaction (2024) by childhood weight perception, crude sample"
)


2 * pnorm(abs(6.2323), lower.tail = FALSE)   # H3a bin_obese
2 * pnorm(abs(-2.4372), lower.tail = FALSE)   # H3a CWP_heavier
2 * pnorm(abs(-1.6205), lower.tail = FALSE)   # H3a CWP_thinner
2 * pnorm(abs(1.7497), lower.tail = FALSE)   # H3a obese × heavier
2 * pnorm(abs(0.2991), lower.tail = FALSE)   # H3a obese × thinner
2 * pnorm(abs(2.2078), lower.tail = FALSE)   # restrictive, H3a obese × thinner




###restrictive----
H2_CWP_res <- restrictive %>% run_polr(
  "H2_CWP_res",
  LS24_cat ~ obe21_bin * CWP_21 + LS21_cat
)
nobs(H2_CWP_res)

margPre_H2_CWP_res <- run_margins(H2_CWP_res, "CWP_21")

plot_margins(
  margPre_H2_CWP, "CWP_21",
  x_label = "Childhood Weight Perception (2021)",
  title = "Predicted probability of life satisfaction (2024) by childhood weight perception"
)



###raw restrictive----
H2_CWP_raw_res <- raw_res %>% run_polr(
  "H2_CWP_raw_res",
  LS24_cat ~ obe21_bin * CWP_21 + LS21_cat
)
nobs(H2_CWP_raw_res)

margPre_H2_CWP_raw_res <- run_margins(H2_CWP_raw_res, "CWP_21")

plot_margins(
  margPre_H2_CWP_raw_res, "CWP_21",
  x_label = "Childhood Weight Perception (2021)",
  title = "Predicted probability of life satisfaction (2024) by childhood weight perception, raw restrictive sample"
)






# Distribution of CWP by obesity status
table(crude$CWP_21, crude$obe21_bin, useNA = "always")

# Proportion in each CWP category
crude %>%
  count(CWP_21) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Mean BMI and LS by CWP category
crude %>%
  group_by(CWP_21) %>%
  summarise(
    n = n(),
    mean_BMI = mean(BMI_21, na.rm = TRUE),
    sd_BMI = sd(BMI_21, na.rm = TRUE),
    mean_LS21 = mean(LS21, na.rm = TRUE),
    mean_LS24 = mean(LS24, na.rm = TRUE)
  )

# margPre_H2_CWP_interaction <- avg_predictions(H2_CWP,
#                                   variables = list(
#                                   CWP_21 = c("no difference", "heavier", "thinner"),
#                                   obe21_bin = c("non-obese", "obese")),
#                                   type = "probs") %>% as.data.frame()
# 
# print(margPre_H2_CWP_interaction)
# 
# margPre_H2_CWP_interaction %>%
#   mutate(
#     group = factor(group, levels = c("dissatisfied", "neutral", "satisfied")),
#     CWP_21 = factor(CWP_21, levels = c("no difference", "heavier", "thinner")),
#     obe21_bin = factor(obe21_bin, levels = c("non-obese", "obese"))
#   ) %>%
#   ggplot(aes(x = CWP_21, y = estimate, fill = group)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.5) +
#   geom_text(aes(label = scales::percent(estimate, accuracy = 0.1)),
#             position = position_stack(vjust = 0.5),
#             size = 3, color = "white", fontface = "bold") +
#   scale_fill_manual(values = c(
#     "dissatisfied" = "#C0504D",
#     "neutral"      = "#9BB8D4",
#     "satisfied"    = "#366092"
#   )) +
#   scale_y_continuous(labels = scales::percent) +
#   facet_wrap(~obe21_bin) +
#   labs(
#     title = "Predicted probability of life satisfaction by childhood weight perception and obesity status",
#     subtitle = "Adjusted for baseline life satisfaction (2021)",
#     x = "Childhood weight perception (before age 13)",
#     y = "Predicted probability",
#     fill = "Life satisfaction (2024)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")


##adulthood weight perception----
###crude----
H2_AWP <- crude %>% run_polr(
  "H2_AWP",
  LS24_cat ~ obe21_bin * AWP_21 + LS21_cat
)
nobs(H2_AWP)

margPre_H2_AWP <- run_margins(H2_AWP, "AWP_21")

crude %>%
  filter(obe21_bin == "obese", AWP_21 == "no difference") %>%
  nrow()

2 * pnorm(abs(-1.1147), lower.tail = FALSE)   # H3b obese
2 * pnorm(abs(-5.9117), lower.tail = FALSE)   # H3b heavier
2 * pnorm(abs(-0.9247), lower.tail = FALSE)   # H3b thinner
2 * pnorm(abs(0.5085), lower.tail = FALSE)   # H3b obese:heavier
2 * pnorm(abs(-1.6223), lower.tail = FALSE)   # H3b obese:thinner




###restrictive----
H2_AWP_res <- restrictive %>% run_polr(
  "H2_AWP_res",
  LS24_cat ~ AWP_21 * obe21_bin + LS21_cat
)
nobs(H2_AWP_res)

margPre_H2_AWP_res <- run_margins(H2_AWP_res, "AWP_21")

###raw restrictive----
H2_AWP_raw_res <- raw_res %>% run_polr(
  "H2_AWP_res",
  LS24_cat ~ AWP_21 * obe21_bin + LS21_cat
)
nobs(H2_AWP_raw_res)

margPre_H2_AWP_raw_res <- run_margins(H2_AWP_raw_res, "AWP_21")

2 * pnorm(abs(-1.1107), lower.tail = FALSE)   # H3b obese
2 * pnorm(abs(-5.6703), lower.tail = FALSE)   # H3b heavier
2 * pnorm(abs(-0.5562), lower.tail = FALSE)   # H3b thinner
2 * pnorm(abs(0.5559), lower.tail = FALSE)   # H3b obese:heavier
2 * pnorm(abs(-1.5811), lower.tail = FALSE)   # H3b obese:thinner

table(crude$momPhys_21_large, crude$obe21_bin)
table(crude$dadPhys_21_large, crude$obe21_bin)

sum(!is.na(crude$momPhys_21_large))
sum(!is.na(crude$dadPhys_21_large))
sum(!is.na(crude$parentPhys_cat))

chisq.test(table(crude$momPhys_21_large, crude$obe21_bin))
chisq.test(table(crude$dadPhys_21_large, crude$obe21_bin))

library(DescTools)
CramerV(table(crude$momPhys_21_large, crude$obe21_bin))
CramerV(table(crude$dadPhys_21_large, crude$obe21_bin))

# Check whether the discrepancy is driven by cases with one parent's data present, the other missing
table(mom_missing = is.na(crude$momPhys_21_large), dad_missing = is.na(crude$dadPhys_21_large))
##parental body size A-C :(( ----
H2_mom <- crude %>% run_polr(
  "H2_mom",
  LS24_cat ~ obe21_bin + momPhys_21_large + LS21_cat
)

H2_dad <- crude %>% run_polr(
  "H2_dad",
  LS24_cat ~ obe21_bin + dadPhys_21_large + LS21_cat
)

table(crude$parentPhys_cat, crude$obe21_bin)
chisq.test(crude$parentPhys_cat, crude$obe21_bin)

###crude----
H2_parents <- crude %>% run_polr(
  "H2_parents",
  LS24_cat ~ obe21_bin*parentPhys_cat + LS21_cat
)
nobs(H2_parents)

margPre_H2_parents <- run_margins(H2_parents, "parentPhys_cat")
broom::tidy(H2_parents, exponentiate = TRUE, conf.int = TRUE)

library(DescTools)

H2_mom <- crude %>% run_polr(
  "H2_mom",
  LS24_cat ~ momPhys_21_large * obe21_bin + LS21_cat
)

H2_dad <- crude %>% run_polr(
  "H2_dad",
  LS24_cat ~ dadPhys_21_large * obe21_bin + LS21_cat
)

broom::tidy(H2_mom, exponentiate = TRUE, conf.int = TRUE)
broom::tidy(H2_dad, exponentiate = TRUE, conf.int = TRUE)

# Ensure correct factor ordering
crude <- crude %>%
  mutate(parentPhys_cat = factor(parentPhys_cat,
                                 levels = c("neither", "one parent", "both")), obe21_bin = factor(obe21_bin,
                                                                                                  levels = c("non-obese", "obese")))

tab_parent_obe <- table(crude$parentPhys_cat, crude$obe21_bin)
print(tab_parent_obe)

prop.table(tab_parent_obe, margin = 1) %>% round(3)
chisq.test(tab_parent_obe)
CochranArmitageTest(tab_parent_obe)

# library(ggplot2)
# library(dplyr)
# 
# # Build summary data from the table output
# plot_data <- crude %>%
#   filter(!is.na(parentPhys_cat) & !is.na(obe21_bin)) %>%
#   mutate(parentPhys_cat = factor(parentPhys_cat,
#                                  levels = c("neither", "one parent", "both"),
#                                  labels = c("Neither parent", "One parent", "Both parents"))) %>%
#   group_by(parentPhys_cat, obe21_bin) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   group_by(parentPhys_cat) %>%
#   mutate(pct = n / sum(n) * 100) %>%
#   ungroup()
# 
# # Stacked bar chart
# ggplot(plot_data, aes(x = parentPhys_cat, y = pct, fill = obe21_bin)) +
#   geom_bar(stat = "identity", width = 0.6) +
#   geom_text(
#     data = filter(plot_data, obe21_bin == "obese"),
#     aes(label = paste0(round(pct, 1), "%")),
#     position = position_stack(vjust = 0.5),
#     color = "white",
#     fontface = "bold",
#     size = 4.2
#   ) +
#   scale_fill_manual(
#     values = c("non-obese" = "#4575b4", "obese" = "#d73027"),
#     labels = c("non-obese" = "Non-obese", "obese" = "Obese"),
#     name   = "Obesity status\nat baseline"
#   ) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),
#                      expand = c(0, 0),
#                      limits = c(0, 105)) +
#   labs(
#     title    = "Obesity prevalence at baseline by parental body size",
#     subtitle = paste0("Crude sample (N = ", 
#                       sum(!is.na(crude$parentPhys_cat) & !is.na(crude$obe21_bin)),
#                       "); Cochran-Armitage trend test: Z = −24.94, p < 0.001"),
#     x        = "Parental body size (at age 40)",
#     y        = "Proportion of participants (%)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title      = element_text(face = "bold", size = 13),
#     plot.subtitle   = element_text(size = 9, color = "grey50"),
#     legend.position = "right",
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor   = element_blank(),
#     axis.line.x        = element_line(color = "grey70")
#   )

# library(ggplot2)
# library(dplyr)
# 
# # Step 1: compute observed prevalence per category for plotting
# prev_data <- crude %>%
#   filter(!is.na(parentPhys_cat) & !is.na(obe21_bin)) %>%
#   mutate(
#     parentPhys_num = case_when(
#       parentPhys_cat == "neither"    ~ 0,
#       parentPhys_cat == "one parent" ~ 1,
#       parentPhys_cat == "both"       ~ 2
#     ),
#     obese_bin = as.integer(obe21_bin == "obese")
#   )
# 
# prevalence_summary <- prev_data %>%
#   group_by(parentPhys_cat, parentPhys_num) %>%
#   summarise(
#     n         = n(),
#     n_obese   = sum(obese_bin),
#     prevalence = mean(obese_bin) * 100,
#     .groups   = "drop"
#   )
# 
# print(prevalence_summary)
# 
# # Step 2: logistic regression with numeric ordered predictor
# model_linear_trend <- glm(
#   obese_bin ~ parentPhys_num,
#   data   = prev_data,
#   family = binomial(link = "logit")
# )
# 
# summary(model_linear_trend)
# 
# # Odds ratio and 95% CI per one-category increase
# exp(coef(model_linear_trend))
# exp(confint(model_linear_trend))
# 
# # Step 3: generate predicted probability curve from the model
# pred_curve <- data.frame(parentPhys_num = seq(0, 2, by = 0.01))
# pred_curve$predicted_pct <- predict(model_linear_trend,
#                                     newdata = pred_curve,
#                                     type = "response") * 100
# 
# # Step 4: plot observed prevalence points with fitted logistic curve
# ggplot() +
#   geom_line(data  = pred_curve,
#             aes(x = parentPhys_num, y = predicted_pct),
#             color = "#d73027",
#             linewidth = 0.9) +
#   geom_point(data = prevalence_summary,
#              aes(x = parentPhys_num, y = prevalence, size = n),
#              color = "#d73027",
#              fill  = "white",
#              shape = 21,
#              stroke = 1.8) +
#   geom_text(data = prevalence_summary,
#             aes(x = parentPhys_num,
#                 y = prevalence + 2.5,
#                 label = paste0(round(prevalence, 1), "%\n(n = ", n, ")")),
#             size = 3.5,
#             color = "grey30") +
#   scale_x_continuous(
#     breaks = c(0, 1, 2),
#     labels = c("Neither parent", "One parent", "Both parents")
#   ) +
#   scale_y_continuous(
#     labels = function(x) paste0(x, "%"),
#     limits = c(0, 55),
#     expand = c(0, 0)
#   ) +
#   scale_size_continuous(range = c(4, 10), guide = "none") +
#   labs(
#     title    = "Linear trend in obesity prevalence by parental body size",
#     subtitle = "Points sized by group n | Fitted logistic regression line",
#     x        = "Parental body size (at age 40)",
#     y        = "Obesity prevalence (%)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title      = element_text(face = "bold", size = 13),
#     plot.subtitle   = element_text(size = 9, color = "grey50"),
#     panel.grid.minor       = element_blank(),
#     panel.grid.major.x     = element_blank()
#   )

library(ggplot2)
library(dplyr)

# Step 1: compute observed mean BMI per category
bmi_summary <- crude %>%
  filter(!is.na(parentPhys_cat) & !is.na(BMI_21)) %>%
  mutate(
    parentPhys_num = case_when(
      parentPhys_cat == "neither"    ~ 0,
      parentPhys_cat == "one parent" ~ 1,
      parentPhys_cat == "both"       ~ 2
    )
  ) %>%
  group_by(parentPhys_cat, parentPhys_num) %>%
  summarise(
    n        = n(),
    mean_BMI = mean(BMI_21, na.rm = TRUE),
    sd_BMI   = sd(BMI_21,   na.rm = TRUE),
    se_BMI   = sd_BMI / sqrt(n),
    .groups  = "drop"
  )

print(bmi_summary)

# Step 2: linear regression with numeric ordered predictor
prev_data <- crude %>%
  filter(!is.na(parentPhys_cat) & !is.na(BMI_21)) %>%
  mutate(
    parentPhys_num = case_when(
      parentPhys_cat == "neither"    ~ 0,
      parentPhys_cat == "one parent" ~ 1,
      parentPhys_cat == "both"       ~ 2
    )
  )

model_bmi_trend <- lm(BMI_21 ~ parentPhys_num + age_2021_imputed, data = prev_data)
summary(model_bmi_trend)
confint(model_bmi_trend)

# Step 3: generate fitted line from model
pred_curve <- data.frame(parentPhys_num = seq(0, 2, by = 0.01))
pred_curve$predicted_BMI <- predict(model_bmi_trend, newdata = pred_curve)

# Step 4: plot
ggplot() +
  geom_line(data = pred_curve,
            aes(x = parentPhys_num, y = predicted_BMI),
            color = "#4575b4",
            linewidth = 0.9) +
  geom_errorbar(data = bmi_summary,
                aes(x    = parentPhys_num,
                    ymin = mean_BMI - 1.96 * se_BMI,
                    ymax = mean_BMI + 1.96 * se_BMI),
                width = 0.05,
                color = "grey40",
                linewidth = 0.7) +
  geom_point(data = bmi_summary,
             aes(x = parentPhys_num, y = mean_BMI, size = n),
             color = "#4575b4",
             fill  = "white",
             shape = 21,
             stroke = 1.8) +
  geom_text(data = bmi_summary,
            aes(x     = parentPhys_num,
                y     = mean_BMI + 1.96 * se_BMI + 0.4,
                label = paste0("Mean = ", round(mean_BMI, 1),
                               "\n(n = ", n, ")")),
            size  = 3.5,
            color = "grey30") +
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c("Neither parent", "One parent", "Both parents"),
    expand = c(0.15, 0.15)        # padding so "both" point is not clipped
  ) +
  scale_y_continuous(
    limits = c(24, 32),           # raised ceiling from 30 to 32
    expand = c(0, 0)
  ) +
  scale_size_continuous(range = c(4, 10), guide = "none") +
  labs(
    title    = "Mean BMI at baseline by parental body size",
    subtitle = "Points sized by group n | Error bars = 95% CI | Fitted linear regression line",
    x        = "Parental body size (at age 40)",
    y        = "Mean BMI (kg/m²)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9, color = "grey50"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

###restrictive----
H2_parents_res <- restrictive %>% run_polr(
  "H2_parents_res",
  LS24_cat ~ parentPhys_cat * obe21_bin + LS21_cat
)
nobs(H2_parents_res)

margPre_H2_parents_res <- run_margins(H2_parents_res, "parentPhys_cat")


2 * pnorm(abs(-5.73450), lower.tail = FALSE)   # H3b obese
2 * pnorm(abs(-0.84885), lower.tail = FALSE)   # H3b one parent
2 * pnorm(abs(-1.2362), lower.tail = FALSE)   # H3b both parents
2 * pnorm(abs(0.51891), lower.tail = FALSE)   # H3b obese:one parent
2 * pnorm(abs(-0.07788), lower.tail = FALSE)   # H3b obese:both parents

###raw restrictive----
###restrictive----
H2_parents_raw_res <- raw_res %>% run_polr(
  "H2_parents_raw_res",
  LS24_cat ~ parentPhys_cat * obe21_bin + LS21_cat
)
nobs(H2_parents_raw_res)

margPre_H2_parents_raw_res <- run_margins(H2_parents_raw_res, "parentPhys_cat")

2 * pnorm(abs(-5.53761), lower.tail = FALSE)   # H3b obese
2 * pnorm(abs(-0.70791), lower.tail = FALSE)   # H3b one parent
2 * pnorm(abs(-1.32404), lower.tail = FALSE)   # H3b both parents
2 * pnorm(abs(0.33290), lower.tail = FALSE)   # H3b obese:one parent
2 * pnorm(abs(-0.05571), lower.tail = FALSE)   # H3b obese:both parents

# H2_momPhys <- crude %>% run_polr(
#   "H2_momPhys",
#   LS24_cat ~ obe21_bin * momPhys_21_large + LS21_cat
# )
# 
# H2_dadPhys <- crude %>% run_polr(
#   "H2_dadPhys",
#   LS24_cat ~ obe21_bin * dadPhys_21_large + LS21_cat
# )

##parental body size A-B :((----
###crude----
parentalSize_AB <- crude %>% run_polr(
  "stricter parental body size",
  LS24_cat ~ obe21_bin * parentPhys_AB + LS21_cat
  
)
nobs(parentalSize_AB)
margPre_H2_parentalSize_AB <- run_margins(parentalSize_AB, "parentPhys_AB")
plot_margins(margPre_H2_parentalSize_AB, "parentPhys_AB",
             x_label = "parental body size: only silhouettes A and B",
             title = "Predicted probability of life satisfaction (2024) by a stricter classification of large parental body size, crude sample")

2 * pnorm(abs(-1.05701), lower.tail = FALSE)   # H3b one parent
2 * pnorm(abs(0.03301), lower.tail = FALSE)   # H3b both parents
2 * pnorm(abs(0.44617), lower.tail = FALSE)   # H3b obese:one parent
2 * pnorm(abs(-0.6828), lower.tail = FALSE)   # H3b obese:both parents

###restrictive----
parentalSize_AB_res <- restrictive %>% run_polr(
  "parentalSize_AB_res",
  LS24_cat ~ obe21_bin * parentPhys_AB + LS21_cat
)
nobs(parentalSize_AB_res)


##AWP typology:))----
###no product term----
# table(ds$WS_d21, useNA = "always")
# table(H2_sample$typology, useNA = "always")
###crude----
H2_typology_AWP <- crude %>% run_polr(
  "H2_typology_AWP",
  LS24_cat ~ typology_adult + LS21_cat
)
nobs(H2_typology_AWP)
margPre_typology_AWP <- run_margins(H2_typology_AWP, "typology_adult")

plot_margins(margPre_typology_AWP, "typology_adult",
             x_label = "Adulthood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by adulthood weight status-perception typology, crude sample")

crude %>%
  count(typology_adult)

###restrictive----
H2_typology_AWP_res <- restrictive %>% run_polr(
  "H2_typology_AWP_res",
  LS24_cat ~ typology_adult + LS21_cat
)
nobs(H2_typology_AWP_res)
margPre_H2_typology_AWP_res <- run_margins(H2_typology_AWP_res, "typology_adult")


###raw restrictive----
H2_typology_AWP_raw_res <- raw_res %>% run_polr(
  "H2_typology_AWP_raw_res",
  LS24_cat ~ typology_adult + LS21_cat
)
nobs(H2_typology_AWP_raw_res)
margPre_H2_typology_AWP_raw_res <- run_margins(H2_typology_AWP_raw_res, "typology_adult")


plot_margins(margPre_typology_AWP, "typology_adult",
             x_label = "Adulthood weight status-perception typology (all data from 2021)",
             title = "Predicted probability of life satisfaction (2024) by adulthood weight perception typology")

###adding continuous BMI----
####crude----
H2_typology_AWP_BMIcont <- crude %>% run_polr(
  "H2_typology_AWP_BMIcont",
  LS24_cat ~ typology_adult + BMI_21 + LS21_cat
)
nobs(H2_typology_AWP_BMIcont)

margPre_H2_typology_AWP_BMIcont <- run_margins(H2_typology_AWP_BMIcont, "typology_adult")

####restrictive----
H2_typology_AWP_BMIcont_res <- restrictive %>% run_polr(
  "H2_typology_AWP_BMIcont_res",
  LS24_cat ~ typology_adult + BMI_21 + LS21_cat
)
nobs(H2_typology_AWP_BMIcont_res)

margPre_H2_typology_AWP_BMIcont_res <- run_margins(H2_typology_AWP_BMIcont_res, "typology_adult")



plot_margins(margPre_typlogy_CWP_BMIadj, "typology_adult",
             x_label = "Adulthood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by adulthood weight perception typology, attenuated by baseline BMI (continuous)")

##kappa coefficient----
install.packages("irr")
library(irr)
citation("irr")

# First create a binary or categorical weight status variable
# that matches the categories of your perception variable
# For childhood typology: CWP_21 vs BMI category

# Step 1: create comparable categorical variables
# Your perception variable CWP_21 has: "heavier", "thinner", "no difference"
# You need to map BMI into the same conceptual categories
crude <- crude %>%
  mutate(
    BMI_21_perc_cat = factor(case_when(
      BMI_21 >= 30 ~ "heavier",        # obese = objectively heavier
      BMI_21 >= 18.5 & BMI_21 < 30 ~ "no difference",  # healthy/overweight = reference
      BMI_21 < 18.5 ~ "thinner"        # underweight = objectively thinner
    ), levels = c("no difference", "heavier", "thinner"))
  )

# Step 2: run kappa
kappa_AWP <- kappa2(
  cbind(as.character(crude$BMI_21_perc_cat),
        as.character(crude$AWP_21)),
  weight = "unweighted"
)

print(kappa_AWP)

##barchart, not included (yet?)----
# Create the cross-tabulation for AWP
awp_bmi_plot <- crude %>%
  filter(!is.na(AWP_21)) %>%
  mutate(
    BMI_group = factor(case_when(
      BMI_21 < 18.5 ~ "Underweight",
      BMI_21 < 25   ~ "Healthy weight",
      BMI_21 < 30   ~ "Overweight",
      BMI_21 >= 30  ~ "Obese"
    ), levels = c("Underweight", "Healthy weight", 
                  "Overweight", "Obese")),
    AWP_21 = factor(AWP_21,
                    levels = c("thinner", "no difference", "heavier"),
                    labels = c("Thinner than most",
                               "Similar to most",
                               "Heavier than most"))
  ) %>%
  group_by(AWP_21, BMI_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prevalence = n / nrow(crude) * 100)

ggplot(awp_bmi_plot,
       aes(x = AWP_21, y = prevalence, fill = BMI_group)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  scale_fill_manual(values = c(
    "Underweight"   = "#A8C5DA",
    "Healthy weight" = "#366092",
    "Overweight"    = "#C0504D",
    "Obese"         = "#7F0000"
  )) +
  geom_text(aes(label = round(prevalence, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 50),
                     labels = scales::label_number(suffix = "%")) +
  labs(
    x = "Perceived weight in adulthood (AT)",
    y = "Prevalence (%)",
    fill = "BMI (baseline)",
    title = "Perceived vs actual weight status",
    subtitle = "Crude sample (n = 17,174)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10)
  )



##CWP typology ----
###crude----
H2_typology_CWP <- crude %>% run_polr(
  "H2_typology_CWP",
  LS24_cat ~ typology_child + LS21_cat
)
nobs(H2_typology_CWP)
margPre_H2_typology_CWP <- run_margins(H2_typology_CWP, "typology_child")

plot_margins(margPre_H2_typology_CWP, "typology_child",
             x_label = "Childhood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by childhood weight perception typology, crude sample")


##forest plot?----
library(ggplot2)
library(dplyr)
library(tibble)

ct_data <- tibble(
  group = factor(
    c("Concordant healthy (reference)",
      "Concordant heavy",
      "Over-perceiver",
      "Under-perceiver"),
    levels = c("Under-perceiver",
               "Over-perceiver",
               "Concordant heavy",
               "Concordant healthy (reference)")
  ),
  OR    = c(1.000, 0.682, 0.839, 0.643),
  lower = c(NA,    0.559, 0.714, 0.567),
  upper = c(NA,    0.837, 0.989, 0.730),
  is_ref = c(TRUE, FALSE, FALSE, FALSE)
) %>%
  mutate(
    label = ifelse(is_ref,
                   "1.00",
                   paste0(round(OR, 2), " (",
                          round(lower, 2), "\u2013",
                          round(upper, 2), ")"))
  )

ggplot(ct_data, aes(x = OR, y = group)) +
  # CI bars for non-reference rows only
  geom_errorbarh(
    data = filter(ct_data, !is_ref),
    aes(xmin = lower, xmax = upper),
    height = 0.2, linewidth = 0.7,
    color = "#366092"
  ) +
  # Points — diamond for reference, circle for others
  geom_point(
    data = filter(ct_data, is_ref),
    shape = 18, size = 4, color = "#366092"
  ) +
  geom_point(
    data = filter(ct_data, !is_ref),
    shape = 16, size = 3, color = "#366092"
  ) +
  # Null line at OR = 1
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.5
  ) +
  # Labels fixed at x = 1.06
  geom_text(
    aes(x = 1.06, label = label),
    hjust = 0, size = 3.2, color = "grey20"
  ) +
  scale_x_continuous(
    limits = c(0.5, 1.6),
    breaks = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3)
  ) +
  labs(
    x = "Odds ratio (95% CI)",
    y = NULL,
    title = "Childhood weight status-perception typology",
    subtitle = "Association with life satisfaction at follow-up (n = 16,981)"
  ) +
  theme_classic() +
  theme(
    axis.text.y  = element_text(size = 10),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey92",
                                      linewidth = 0.4),
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

###restrictive----
H2_typology_CWP_res <- restrictive %>% run_polr(
  "H2_typology_CWP_res",
  LS24_cat ~ typology_child + LS21_cat
)
nobs(H2_typology_CWP_res)
margPre_H2_typology_CWP_res <- run_margins(H2_typology_CWP_res, "typology_child")


###raw restrictive----
H2_typology_CWP_raw_res <- raw_res %>% run_polr(
  "H2_typology_CWP_raw_res",
  LS24_cat ~ typology_child + LS21_cat
)
nobs(H2_typology_CWP_raw_res)
margPre_H2_typology_CWP_raw_res <- run_margins(H2_typology_CWP_raw_res, "typology_child")


###adding continuous BMI---- 
####crude----
H2_typology_CWP_BMIcont <- crude %>% run_polr(
  "H2_typology_CWP_BMIcont",
  LS24_cat ~ typology_child + BMI_21 + LS21_cat
)
nobs(H2_typology_CWP_BMIcont)

margPre_H2_typology_CWP_BMIcont <- run_margins(H2_typology_CWP_BMIcont, "typology_child")

####restrictive----
H2_typology_CWP_BMIcont_res <- restrictive %>% run_polr(
  "H2_typology_CWP_BMIcont_res",
  LS24_cat ~ typology_child + BMI_21 + LS21_cat
)
nobs(H2_typology_CWP_BMIcont_res)

margPre_H2_typology_CWP_BMIcont_res <- run_margins(H2_typology_CWP_BMIcont_res, "typology_child")




plot_margins(margPre_typlogy_CWP_BMIadj, "typology_child",
             x_label = "Childhood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by childhood weight perception typology, attenuated by baseline BMI (continuous)")

margPre_typology_CWP <- run_margins(H2_typology_CWP, "typology_child")
margPre_typology_CWP_cruSmpl <- run_margins(H2_typology_CWP_cruSmpl, "typology_child")

plot_margins(margPre_typology_CWP, "typology_child",
             x_label = "Childhood weight status-perception typology",
             title = "Predicted probability of life satisfaction (2024) by childhood weight perception typology")



#TABLE 1----
##crude----
library(gtsummary)
library(flextable)

table1_gt <- crude %>%
  dplyr::select(BMI_21_label, age_2021_imputed, BMI_21,
                LS21, LS24,
                obePersist, parentPhys_cat, CWP_21, AWP_21,
                diplUd_21_bin, obeInh_24,
                typology_child, typology_adult) %>%
  tbl_summary(
    by = BMI_21_label,
    missing = "ifany",
    missing_text = "Missing",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age (years)",
      BMI_21          ~ "BMI (kg/m²)",
      LS21            ~ "Life satisfaction at baseline (2021)",
      LS24            ~ "Life satisfaction at follow-up (2024)",
      obePersist      ~ "Obesity persistence between baseline and follow-up",
      CWP_21          ~ "Childhood weight perception",
      AWP_21          ~ "Adulthood weight perception",
      parentPhys_cat  ~ "Parental body size at age 40",
      diplUd_21_bin   ~ "Attainment of nursing diploma education",
      obeInh_24       ~ "Family history of overweight",
      typology_child  ~ "Childhood weight status-perception typology (CT)",
      typology_adult  ~ "Adulthood weight status-perception typology (AT)"
    ),
    percent = "column",
    type = list(
      diplUd_21_bin ~ "categorical",
      obeInh_24     ~ "categorical"
    ),
    value = list(
      diplUd_21_bin ~ "yes",
      obeInh_24     ~ "yes"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      parentPhys_cat    ~ "fisher.test",
      typology_adult    ~ "fisher.test",
      typology_child    ~ "fisher.test",
      all_categorical() ~ "chisq.test"
    ),
    test.args = list(
      parentPhys_cat = list(simulate.p.value = TRUE),
      typology_adult = list(simulate.p.value = TRUE),
      typology_child = list(simulate.p.value = TRUE)
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_spanning_header(
    all_stat_cols() ~ "**BMI categories, crude sample**"
  ) %>%
  bold_labels()

table1_gt

getwd()

# Save to Word
table1_gt %>%
  modify_column_unhide(columns = p.value) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "table1.docx")

##restrictive----
restrictive %>%
  dplyr::select(BMI_21_label, age_2021_imputed, BMI_21, 
                obePersist, CWP_21, parentPhys_cat, obeInh_24, diplUd_21) %>%
  tbl_summary(
    by = BMI_21_label,
    missing_text = "Missing",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age",
      BMI_21 ~ "BMI (kg/m²)",
      obePersist ~ "Obesity persistence",
      CWP_21 ~ "Childhood weight perception",
      parentPhys_cat ~ "Parental body size",
      obeInh_24 ~ "Family history of overweight (heredity)",
      diplUd_21 ~ "Attainment of diplomuddannelse"
    )
  ) %>%
  add_overall() %>%
  modify_spanning_header(all_stat_cols() ~ "**2021, restrictive**") %>%
  bold_labels()
nrow(crude)

##raw restrictive----
raw_res %>%
  dplyr::select(BMI_21_label, age_2021_imputed, BMI_21, 
                obePersist, CWP_21, parentPhys_cat, obeInh_24, diplUd_21) %>%
  tbl_summary(
    by = BMI_21_label,
    missing_text = "Missing",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age",
      BMI_21 ~ "BMI (kg/m²)",
      obePersist ~ "Obesity persistence",
      CWP_21 ~ "Childhood weight perception",
      parentPhys_cat ~ "Parental body size",
      obeInh_24 ~ "Family history of overweight (heredity)",
      diplUd_21 ~ "Attainment of diplomuddannelse"
    )
  ) %>%
  add_overall() %>%
  modify_spanning_header(all_stat_cols() ~ "**2021, raw restrictive sample**") %>%
  bold_labels()
nrow(raw_res)


#CITATION, version, & session info----
citation()
citation("MASS")
citation("dplyr")
citation("gtsummary")
citation("tidyverse")
citation("brant")
citation("marginaleffects")
citation("ggplot2")
citation("flextable")
citation("tidyr")
citation("smd")


packageVersion("")
# sessionInfo()

#descriptive stat by covariates----

# VARIABLE AUDIT: Descriptive Statistics by Response Category
# PURPOSE:
#   For each key analytic variable, show who answered what —
#   and how they look on age, LS21, LS24, and BMI21/BMI24.
#   Use this to decide how to treat each variable (recode,
#   collapse categories, handle NAs, flag for exclusion, etc.)

# OUTPUTS:
#   - One printed table per variable
#   - One combined CSV with all tables stacked


library(dplyr)
library(tidyr)

## ---- OUTCOME / EXPOSURE SUMMARY FUNCTION
# For a given grouping variable, compute n, %, and means of
# age, LS21, LS24, BMI21, BMI24 within each response category.

summarise_by_var <- function(data, var_name, label_map = NULL) {
  
  total_n <- nrow(data)
  
  data <- data %>%
    mutate(
      .grp = as.character(.data[[var_name]]),
      .grp = if_else(is.na(.grp), "NA (missing)", .grp)
    )
  
  if (!is.null(label_map)) {
    data <- data %>%
      mutate(.grp = recode(.grp, !!!label_map))
  }
  
  data %>%
    group_by(.grp) %>%
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
    rename(response_category = .grp) %>%
    mutate(variable = var_name, .before = 1)
}

print_audit <- function(tbl, title) {
  cat("\n", strrep("=", 70), "\n")
  cat(" VARIABLE:", title, "\n")
  cat(strrep("=", 70), "\n")
  print(as.data.frame(tbl), row.names = FALSE)
}

yn_map    <- c("0" = "No", "1" = "Yes")
lgbt_map  <- c("1" = "Yes (LGBT+)",
               "2" = "No",
               "3" = "Don't know",
               "4" = "Prefer not to say")
###binary var----

#1. diplomuddannelse
audit_diplUd <- summarise_by_var(crude, "diplUd_21",
                                 label_map = yn_map)
print_audit(audit_diplUd, "Diploma education (diplUd_21)")

# 2. Specialist education
audit_speUd <- summarise_by_var(crude, "speUd_21",
                                label_map = yn_map)
print_audit(audit_speUd, "Specialist education (speUd_21)")

# 3. Masters education
audit_mastUd <- summarise_by_var(crude, "mastUd_21",
                                 label_map = yn_map)
print_audit(audit_mastUd, "Masters education (mastUd_21)")

# 4. Kandidat education
audit_kandiUd <- summarise_by_var(crude, "kandiUd_21",
                                  label_map = yn_map)
print_audit(audit_kandiUd, "Kandidat education (kandiUd_21)")

# 5. PhD
audit_PhD <- summarise_by_var(crude, "PhD",
                              label_map = yn_map)
print_audit(audit_PhD, "PhD (PhD)")

# 6. LGBT identity (4-category: 1=Yes, 2=No, 3=Don't know, 4=Prefer not to say)
lgbt_map <- c("1" = "Yes (LGBT+)", "2" = "No", "3" = "Don't know", "4" = "Prefer not to say")
audit_lgbt <- summarise_by_var(crude, "lgbtID",
                               label_map = lgbt_map)
print_audit(audit_lgbt, "LGBT identity (lgbtID)")

# 7. Childhood weight perception (CWP_21)
audit_CWP <- summarise_by_var(crude, "CWP_21")
print_audit(audit_CWP, "Childhood weight perception (CWP_21)")

# 8. Mother physique (momPhys_21)
audit_momPhys <- summarise_by_var(crude, "momPhys_21_large")
print_audit(audit_momPhys, "Mother physique (momPhys_21_large)")

# 9. Father physique (dadPhys_21)
audit_dadPhys <- summarise_by_var(crude, "dadPhys_21_large")
print_audit(audit_dadPhys, "Father physique (dadPhys_21_large)")

# 10. Obesity inheritance flag - 2024 (obeInh_24)
obeInh_map <- c("0" = "Not selected", "1" = "Selected (hereditary obesity)")
audit_obeInh <- summarise_by_var(crude, "obeInh_24",
                                 label_map = obeInh_map)
print_audit(audit_obeInh, "Hereditary obesity (obeInh_24)")


# 11. Night shift schedule (nightSche_21)
audit_nightSche <- summarise_by_var(crude, "nightSche_21",
                                    label_map = yn_map)
print_audit(audit_nightSche, "Night shift work (nightSche_21)")

# 12. Evening shift schedule (eveSche_21)
audit_eveSche <- summarise_by_var(crude, "eveSche_21",
                                  label_map = yn_map)
print_audit(audit_eveSche, "Evening shift work (eveSche_21)")

# 13. Day shift schedule (daySche_21)
audit_daySche <- summarise_by_var(crude, "daySche_21",
                                  label_map = yn_map)
print_audit(audit_daySche, "Day shift work (daySche_21)")

# 14. Weight change thoughts (WCT_21)
# Typical: 1=Lost weight, 2=Same, 3=Gained weight
audit_WCT <- summarise_by_var(crude, "WCT_21")
print_audit(audit_WCT, "Thoughts about changing weight (WCT_21)")

# 15. Weight statements b, c, d (body image components)
audit_WSb <- summarise_by_var(crude, "WS_b21")
print_audit(audit_WSb, "Weight statement B - 2021 (WS_b21)")

audit_WSc <- summarise_by_var(crude, "WS_c21")
print_audit(audit_WSc, "Weight statement C - 2021 (WS_c21)")

audit_AWP <- summarise_by_var(crude, "AWP_21")
print_audit(audit_AWP, "Adulthood weight perception - 2021 (AWP_21)")

audit_parentPhys_cat <- summarise_by_var(crude, "parentPhys_cat")
print_audit(audit_parentPhys_cat, "parental body size - 2021 (parentPhys_cat)")

#16. momPhys_21_large
audit_momPhys_21_large <- summarise_by_var(crude, "momPhys_21_large")
print_audit(audit_momPhys_21_large, "biological mother's body size at age 40 - 2021 (momPhys_21_large")

#17. dadPhys_21_large
audit_dadPhys_21_large <- summarise_by_var(crude, "dadPhys_21_large")
print_audit(audit_dadPhys_21_large, "biological father's body size at age 40 - 2021 (dadPhys_21_large")

audit_BMI_cat

##continuous var---------
# For continuous vars, we don't stratify by group.
# Instead: quartile split + NA flag, to see if NAs cluster.

cat("\n", strrep("=", 70), "\n")
cat(" CONTINUOUS VARIABLE OVERVIEW\n")
cat(strrep("=", 70), "\n")
library(tidyr)
conti_summary <- crude %>%
  summarise(
    across(
      c(age_2021_imputed, LS21, LS24, BMI_21, BMI_24),
      list(
        n_valid = ~sum(!is.na(.)),
        n_miss  = ~sum(is.na(.)),
        pct_miss = ~round(mean(is.na(.)) * 100, 1),
        mean    = ~round(mean(., na.rm = TRUE), 2),
        sd      = ~round(sd(.,   na.rm = TRUE), 2),
        p25     = ~round(quantile(., 0.25, na.rm = TRUE), 2),
        median  = ~round(median(., na.rm = TRUE), 2),
        p75     = ~round(quantile(., 0.75, na.rm = TRUE), 2),
        min     = ~round(min(., na.rm = TRUE), 2),
        max     = ~round(max(., na.rm = TRUE), 2)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(as.data.frame(conti_summary), row.names = FALSE)

if ("BMI_21_label" %in% names(crude)) {
  audit_BMI_cat <- summarise_by_var(crude, "BMI_21_label")
  print_audit(audit_BMI_cat, "BMI category 2021 (BMI_21_label)")
}

if ("BMI_24_label" %in% names(crude)) {
  audit_BMI24_cat <- summarise_by_var(crude, "BMI_24_label")
  print_audit(audit_BMI24_cat, "BMI category 2024 (BMI_24_label)")
}

# Obesity persistence variable
if ("obePersist" %in% names(crude)) {
  audit_obePersist <- summarise_by_var(crude, "obePersist")
  print_audit(audit_obePersist, "Obesity persistence (obePersist)")
}

if ("LS21_cat" %in% names(crude)) {
  audit_LS21cat <- summarise_by_var(crude, "LS21_cat")
  print_audit(audit_LS21cat, "LS 2021 category (LS21_cat)")
}

if ("LS24_cat" %in% names(crude)) {
  audit_LS24cat <- summarise_by_var(crude, "LS24_cat")
  print_audit(audit_LS24cat, "LS 2024 category (LS24_cat)")
}

##combined results as df----
all_audits <- bind_rows(
  audit_diplUd,
  audit_speUd,
  audit_mastUd,
  audit_kandiUd,
  audit_PhD,
  audit_lgbt,
  audit_CWP,
  audit_momPhys,
  audit_dadPhys,
  audit_obeInh,
  audit_nightSche,
  audit_eveSche,
  audit_daySche,
  audit_WCT,
  audit_WSb,
  audit_WSc,
  audit_WSd,
  audit_momPhys_21_large,
  audit_dadPhys_21_large
)
View(all_audits)
# write.csv(all_audits, "all audits1.csv", row.names = FALSE)

# write.csv(all_audits, "variable_audit_table.csv", row.names = FALSE)
# cat("\n\nSaved: variable_audit_table.csv\n")

##quick summary----
print_var_table <- function(audit_obj, title) {
  cat("\n", strrep("=", 70), "\n")
  cat(" ", title, "\n")
  cat(strrep("=", 70), "\n")
  
  tbl <- dplyr::select(audit_obj, -variable)
  
  tbl %>%
    dplyr::bind_rows(
      tbl %>%
        dplyr::summarise(response_category = "Total",
                         n = sum(n), pct = sum(pct),
                         dplyr::across(dplyr::where(is.numeric) & !c(n, pct), ~NA))
    ) %>%
    print(width = Inf)
}

print_cont_table <- function(cont_obj) {
  cat("\n", strrep("=", 70), "\n")
  cat("  Continuous Variables\n")
  cat(strrep("=", 70), "\n")
  
  cont_obj %>%
    print()
}

cat("\n", strrep("=", 70), "\n")
cat(" MISSINGNESS OVERVIEW — All Key Variables\n")
cat(strrep("=", 70), "\n")

print_var_table(
  audit_obeInh,
  "family hereditary overweight"
)


#heatmap----
# Two plot types:
#   1. plot_var_heatmap()  — one heatmap per variable showing
#      mean age, LS21, LS24, BMI21, BMI24 by response category
#   2. plot_na_comparison() — one plot showing NA group means
#      vs overall ds means across all variables

library(ggplot2)
library(dplyr)
library(tidyr)

#crude sample means ---------
crude_stat <- crude %>%
  summarise(
    age_mean   = mean(age_2021_imputed, na.rm = TRUE),
    age_sd     = sd(age_2021_imputed,   na.rm = TRUE),
    LS21_mean  = mean(LS21,  na.rm = TRUE),
    LS21_sd    = sd(LS21,    na.rm = TRUE),
    LS24_mean  = mean(LS24,  na.rm = TRUE),
    LS24_sd    = sd(LS24,    na.rm = TRUE),
    BMI21_mean = mean(BMI_21, na.rm = TRUE),
    BMI21_sd   = sd(BMI_21,   na.rm = TRUE),
    BMI24_mean = mean(BMI_24, na.rm = TRUE),
    BMI24_sd   = sd(BMI_24,   na.rm = TRUE)
  )

plot_var_heatmap <- function(audit_obj, title) {
  
  plot_data <- audit_obj %>%
    filter(response_category != "Total") %>%
    dplyr::select(response_category,
                  age_mean, LS21_mean, LS24_mean, BMI21_mean, BMI24_mean) %>%
    pivot_longer(
      cols = -response_category,
      names_to = "outcome",
      values_to = "mean_val"
    ) %>%
    mutate(
      # Z-score each value against the overall ds mean and SD
      scaled = case_when(
        outcome == "age_mean"   ~ (mean_val - crude_stat$age_mean)   / crude_stat$age_sd,
        outcome == "LS21_mean"  ~ (mean_val - crude_stat$LS21_mean)  / crude_stat$LS21_sd,
        outcome == "LS24_mean"  ~ (mean_val - crude_stat$LS24_mean)  / crude_stat$LS24_sd,
        outcome == "BMI21_mean" ~ (mean_val - crude_stat$BMI21_mean) / crude_stat$BMI21_sd,
        outcome == "BMI24_mean" ~ (mean_val - crude_stat$BMI24_mean) / crude_stat$BMI24_sd
      ),
      label = round(mean_val, 1),
      outcome = recode(outcome,
                       "age_mean"   = "Age",
                       "LS21_mean"  = "LS 2021",
                       "LS24_mean"  = "LS 2024",
                       "BMI21_mean" = "BMI 2021",
                       "BMI24_mean" = "BMI 2024"
      )
    ) %>%
    mutate(
      outcome = factor(outcome,
                       levels = c("Age", "LS 2021", "LS 2024", "BMI 2021", "BMI 2024")),
      response_category = factor(response_category,
                                 levels = rev(unique(response_category)))
    )
  
  ggplot(plot_data, aes(x = outcome, y = response_category, fill = scaled)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label), size = 3.5, fontface = "bold") +
    scale_fill_gradient2(
      low      = "#4575b4",
      mid      = "#ffffbf",
      high     = "#d73027",
      midpoint = 0,
      na.value = "grey90",
      name     = "Z-score\n(vs. overall\nsample)"
    ) +
    labs(
      title    = title,
      subtitle = "Cell values = raw means | Color = z-score distance from overall sample mean",
      x        = NULL,
      y        = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 9, color = "grey40"),
      axis.text.x     = element_text(face = "bold"),
      axis.text.y     = element_text(face = "bold"),
      legend.position = "right",
      panel.grid      = element_blank()
    )
}


##variable heatmaps----
plot_var_heatmap(audit_diplUd,    "Diploma education (diplUd_21)")
plot_var_heatmap(audit_speUd,     "Specialist education (speUd_21)")
plot_var_heatmap(audit_mastUd,    "Masters education (mastUd_21)")
plot_var_heatmap(audit_kandiUd,   "Kandidat education (kandiUd_21)")
plot_var_heatmap(audit_PhD,       "PhD (PhD)")
plot_var_heatmap(audit_nightSche, "Night shift (nightSche_21)")
plot_var_heatmap(audit_eveSche,   "Evening shift (eveSche_21)")
plot_var_heatmap(audit_daySche,   "Day shift (daySche_21)")
plot_var_heatmap(audit_lgbt,      "LGBT identity (lgbtID)")
plot_var_heatmap(audit_obeInh,    "Hereditary obesity (obeInh_24)")
plot_var_heatmap(audit_CWP,       "Childhood weight perception (CWP_21)")
plot_var_heatmap(audit_WCT,       "Weight change (WCT_21)")
plot_var_heatmap(audit_WSb,       "Weight statement B (WS_b21)")
plot_var_heatmap(audit_WSc,       "Weight statement C (WS_c21)")
plot_var_heatmap(audit_WSd,       "Weight statement D (WS_d21)")
plot_var_heatmap(audit_BMI_cat,   "BMI category 2021")
plot_var_heatmap(audit_obePersist,"Obesity persistence (obePersist)")
plot_var_heatmap(audit_LS21cat,   "LS category 2021 (LS21_cat)")
plot_var_heatmap(audit_LS24cat,   "LS category 2024 (LS24_cat)")

# #NA comparison plot -----
# audit_list <- list(
#   audit_BMI_cat, 
#   audit_obePersist,
#   audit_diplUd, 
#   audit_obeInh,
#   audit_CWP, 
#   audit_AWP,
#   audit_parentPhys_cat,
#   audit_LS21cat, 
#   audit_LS24cat
# )

audit_list_attrition <- list(
  audit_CWP,
  audit_WSd,          # this is actually AWP_21 based on your code
  audit_obeInh,
  audit_diplUd,
  audit_momPhys_21_large,
  audit_dadPhys_21_large
)

label_map_attrition <- c(
  "CWP_21"             = "Childhood weight perception",
  "AWP_21"             = "Adulthood weight perception",
  "obeInh_24"          = "Family history of overweight",
  "diplUd_21"          = "Diploma education",
  "momPhys_21_large"   = "Mother body size",
  "dadPhys_21_large"   = "Father body size"
)

# Takes a named list of audit objects, extracts the NA row from each,
# and plots NA group mean vs overall ds mean for each outcome
crude_stat_long <- crude_stat %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "outcome",
    values_to = "overall_mean"
  ) %>%
  dplyr::select(outcome, overall_mean) %>%
  mutate(outcome = recode(outcome,
                          "age_mean"   = "Age",
                          "LS21_mean"  = "LS 2021",
                          "LS24_mean"  = "LS 2024",
                          "BMI21_mean" = "BMI 2021",
                          "BMI24_mean" = "BMI 2024"
  ))

plot_na_comparison <- function(audit_list, label_map_attrition) {
  
  na_data <- bind_rows(audit_list) %>%
    dplyr::filter(response_category == "NA (missing)") %>%
    dplyr::select(variable, age_mean, LS21_mean, LS24_mean, BMI21_mean, BMI24_mean) %>%
    pivot_longer(
      cols = -variable,
      names_to = "outcome",
      values_to = "na_mean"
    ) %>%
    mutate(outcome = recode(outcome,
                            "age_mean"   = "Age",
                            "LS21_mean"  = "LS 2021",
                            "LS24_mean"  = "LS 2024",
                            "BMI21_mean" = "BMI 2021",
                            "BMI24_mean" = "BMI 2024"
    ))
  
  plot_data <- na_data %>%
    left_join(crude_stat_long, by = "outcome") %>%
    mutate(
      diff = na_mean - overall_mean,
      outcome = factor(outcome,
                       levels = c("Age", "LS 2021", "LS 2024", "BMI 2021", "BMI 2024"))
    )
  
  ggplot(plot_data, aes(x = variable, y = na_mean, group = outcome, color = outcome)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_hline(data = crude_stat_long,
               aes(yintercept = overall_mean, color = outcome),
               linetype = "dashed", linewidth = 0.6, alpha = 0.5) +
    facet_wrap(~outcome, scales = "free_y", ncol = 1) +
    labs(
      title    = "NA group means vs crude sample means",
      subtitle = "Solid line = NA group | Dashed line = crude sample mean",
      x        = "Variable",
      y        = "Mean value",
      color    = "Outcome"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      legend.position  = "none",
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

plot_na_comparison(audit_list)

library(gtsummary)

crude %>%
  mutate(diplUd_missing = is.na(diplUd_21)) %>%
  dplyr::select(diplUd_missing, age_2021_imputed, BMI_21, 
                LS21, LS24, obe21_bin, CWP_21) %>%
  tbl_summary(
    by = diplUd_missing,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age",
      BMI_21 ~ "BMI (kg/m²)",
      LS21 ~ "Life satisfaction 2021",
      LS24 ~ "Life satisfaction 2024",
      obe21_bin ~ "Obesity status"
    )
  ) %>%
  bold_labels()





#ADULTHOOD TYPOLOGY: STEP-WISE ANALYSIS----
##create binary variable: concordant heavy vs everyone else----
###crude----
ds <- ds %>%
  mutate(
    typAdult_bin = factor(case_when(
      typology_adult == "concordant heavy" ~ "concordant heavy",
      typology_adult != "concordant heavy" ~ "everyone else"
    ), levels = c("everyone else", "concordant heavy"))
  )

##interaction model: BMI * typAdult_bin----
typAdult_bin_crude <- crude %>% run_polr(
  "typAdult_bin",
  LS24_cat ~ BMI_21 * typAdult_bin + LS21_cat
)
nobs(typAdult_bin_crude)
margPre_typAdult_bin_crude <- run_margins(typAdult_bin_crude, "typAdult_bin")

##Main effectmode
typAdult_bin_crude_main <- crude %>% run_polr(
  "typAdult_bin",
  LS24_cat ~ typAdult_bin + LS21_cat + BMI_21
)
nobs(typAdult_bin_crude_main)
margPre_typAdult_bin_crude_main <- run_margins(typAdult_bin_crude_main, "typAdult_bin")


2 * pnorm(abs(-1.473), lower.tail = FALSE)   # H3a bin_obese



plot_margins(
  margPre_typAdult_bin_crude, "typAdult_bin",
  x_label = "Adulthood weight perception-status typology",
  title = "Predicted probability of life satisfaction (2024) by adulthood weight perception-status typology"
)
###restrictive----
####interaction----
typAdult_bin_res <- restrictive %>% run_polr(
  "typAdult_bin_res",
  LS24_cat ~ BMI_21 * typAdult_bin + LS21_cat
)
nobs(typAdult_bin_res)
margPre_typAdult_bin_res <- run_margins(typAdult_bin_res, "typAdult_bin")


####main effect----
typAdult_bin_res_mainEffect <- restrictive %>% run_polr(
  "typAdult_bin_res_mainEffect",
  LS24_cat ~ typAdult_bin + LS21_cat + BMI_21
)
nobs(typAdult_bin_res_mainEffect)


2 * pnorm(abs(-1.507), lower.tail = FALSE)







##predicted probability across BMI range by group----
### NOTE: concordant heavy only exists at BMI >= 30 by definition,
### !so predictions below BMI 30 for that group are extrapolation!



###concordant heavy V everyone else----
pred_grid <- expand.grid(
  BMI_21 = seq(min(crude$BMI_21, na.rm = TRUE),
               max(crude$BMI_21, na.rm = TRUE),
               length.out = 200),
  typAdult_bin = factor(c("everyone else", "concordant heavy"),
                        levels = levels(crude$typAdult_bin)),
  LS21_cat = factor("satisfied", levels = levels(crude$LS21_cat))
)

pred_probs <- predict(typAdult_bin_crude,
                      newdata = pred_grid,
                      type = "probs")

pred_grid$satisfied <- pred_probs[, "satisfied"]

ggplot(pred_grid, aes(x = BMI_21, y = satisfied, color = typAdult_bin)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0.6, 1), labels = scales::percent) +
  scale_color_manual(values = c("everyone else" = "#2166AC",
                                "concordant heavy" = "#D6604D")) +
  labs(
    x = "BMI (2021)",
    y = "Predicted probability of satisfied",
    color = "Group",
    title = "Predicted probability of life satisfaction by BMI and weight perception typology",
    subtitle = "Adjusted for baseline life satisfaction (2021)"
  ) +
  theme_minimal()


###with BMI = 30 line----
ggplot(pred_grid, aes(x = BMI_21, y = satisfied, color = typAdult_bin)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 30, linetype = "dashed", 
             color = "grey40", linewidth = 0.6) +
  annotate("text", x = 31, y = 0.62, label = "BMI 30",
           hjust = 0, size = 3, color = "grey40") +
  scale_y_continuous(limits = c(0.6, 1), labels = scales::percent) +
  scale_color_manual(values = c("everyone else" = "#2166AC",
                                "concordant heavy" = "#D6604D")) +
  labs(
    x = "BMI (2021)",
    y = "Predicted probability of satisfied",
    color = "Group",
    title = "Predicted probability of life satisfaction by BMI and weight perception typology",
    subtitle = "Adjusted for baseline life satisfaction (2021)"
  ) +
  theme_minimal()



# library(marginaleffects)
# 
# comparisons(typAdult_bin_crude,
#             variables = "typAdult_bin",
#             by = "LS21_cat",
#             type = "probs")


##3-line visual: add whole-sample baseline from H1----
###???----
bmi_seq <- seq(min(crude$BMI_21, na.rm = TRUE),
               max(crude$BMI_21, na.rm = TRUE),
               length.out = 200)

# baseline model prediction grid
pred_grid_baseline <- data.frame(
  BMI_21 = bmi_seq,
  LS21_cat = factor("satisfied", levels = levels(crude$LS21_cat))
)

H1_continuous <- crude %>% run_polr(
  "H1_continuous",
  LS24_cat ~ BMI_21
)

pred_probs_baseline <- predict(H1_continuous,
                               newdata = pred_grid_baseline,
                               type = "probs")
pred_grid_baseline$satisfied <- pred_probs_baseline[, "satisfied"]
pred_grid_baseline$typAdult_bin <- "whole sample"

# make sure pred_grid also uses bmi_seq
pred_grid <- expand.grid(
  BMI_21 = bmi_seq,
  typAdult_bin = factor(c("everyone else", "concordant heavy"),
                        levels = levels(crude$typAdult_bin)),
  LS21_cat = factor("satisfied", levels = levels(crude$LS21_cat))
)

pred_probs <- predict(typAdult_bin_crude,
                      newdata = pred_grid,
                      type = "probs")
pred_grid$satisfied <- pred_probs[, "satisfied"]

# combine
pred_grid$typAdult_bin <- as.character(pred_grid$typAdult_bin)

pred_all <- bind_rows(
  pred_grid[, c("BMI_21", "typAdult_bin", "satisfied")],
  pred_grid_baseline[, c("BMI_21", "typAdult_bin", "satisfied")]
)

pred_all_plot <- pred_all %>%
  filter(!(typAdult_bin == "concordant heavy" & BMI_21 < 30))


pred_all$typAdult_bin <- factor(pred_all$typAdult_bin,
                                levels = c("whole sample", "everyone else",  "concordant heavy"))

# plot
ggplot(pred_all_plot, #can use pred_all to keep extrapolation before BMI = 30
       aes(x = BMI_21, y = satisfied, color = typAdult_bin)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 30, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  annotate("text", x = 31, y = 0.62, label = "BMI 30",
           hjust = 0, size = 3, color = "grey40") +
  scale_y_continuous(limits = c(0.6, 1), labels = scales::percent) +
  scale_color_manual(values = c("whole sample" = "grey50",
                                "everyone else" = "#2166AC",
                                "concordant heavy" = "#D6604D")) +
  labs(
    x = "BMI (2021)",
    y = "Predicted probability of satisfied",
    color = "Group",
    title = "Predicted probability of life satisfaction by BMI and weight perception typology",
    subtitle = "Adjusted for baseline life satisfaction (2021)"
  ) +
  theme_minimal()





##confusion model----
pred_class <- predict(typAdult_bin_crude, type = "class")

conf_matrix <- table(
  Predicted = pred_class,
  Actual = crude$LS24_cat[!is.na(crude$typAdult_bin)]
)

print(conf_matrix)

# overall accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy * 100, 1), "%\n")























#H3----
H3 <- crude %>% run_polr(
  "H3",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed)

margPre_H3 <- run_margins(H3, "obe21_bin")

##sequential models----
H3_m1 <- crude %>% run_polr(
  "H3_m1",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed
)
nobs(H3_m1)
margPre_H3_m1 <- run_margins(H3_m1, "obe21_bin")

H3_m1_rawRes <- raw_res %>% run_polr(
  "H3_m1_rawRes",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed
)
nobs(H3_m1_rawRes)
margPre_H3_m1_rawRes <- run_margins(H3_m1_rawRes, "obe21_bin")


H3_m2 <- crude %>% run_polr(
  "H3_m2",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin
)
nobs(H3_m2)
margPre_H3_m2 <- run_margins(H3_m2, "obe21_bin")

H3_m2_rawRes <- raw_res %>% run_polr(
  "H3_m2_rawRes",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin
)
nobs(H3_m2_rawRes)
margPre_H3_m2_rawRes <- run_margins(H3_m2_rawRes, "obe21_bin")


H3_m3 <- crude %>% run_polr(
  "H3_m3",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin + obeInh_24
)
nobs(H3_m3)
margPre_H3_m3 <- run_margins(H3_m3, "obe21_bin")

H3_m3_rawRes <- raw_res %>% run_polr(
  "H3_m3_rawRes",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin + obeInh_24
)
nobs(H3_m3_rawRes)
margPre_H3_m3_rawRes <- run_margins(H3_m3_rawRes, "obe21_bin")



H3_m4 <- crude %>% run_polr(
  "H3_m4",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin + obeInh_24 + parentPhys_cat
)
nobs(H3_m4)

h4_vif_proxy <- glm(
  as.numeric(LS21_cat) ~ obe21_bin + age_2021_imputed + diplUd_21_bin + obeInh_24 + LS21_cat,
  data = crude,
  family = gaussian
)

library(car)
vif(h4_vif_proxy)





##comparing crude adjusted and model 3----
library(dplyr)
library(ggplot2)

# Build combined data frame from your two marginal prediction objects
combined <- bind_rows(
  margPre_H1_crudeAdj %>% 
    mutate(model = "H1: Baseline LS adjusted"),
  margPre_H3_m3 %>% 
    mutate(model = "H4 Model 3: Fully adjusted")
) %>%
  mutate(
    group = factor(group, 
                   levels = c("dissatisfied", "neutral", "satisfied")),
    obe21_bin = factor(obe21_bin, 
                       levels = c("non-obese", "obese")),
    model = factor(model, 
                   levels = c("H1: Baseline LS adjusted",
                              "H4 Model 3: Fully adjusted"))
  )

# Faceted line plot by LS outcome category
ggplot(combined, 
       aes(x = obe21_bin, 
           y = estimate, 
           color = model, 
           group = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~group, scales = "free_y",
             labeller = labeller(group = c(
               dissatisfied = "Dissatisfied",
               neutral = "Neutral", 
               satisfied = "Satisfied"
             ))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "H1: Baseline LS adjusted" = "#366092",
    "H4 Model 3: Fully adjusted" = "#C0504D"
  )) +
  labs(
    x = "Obesity status (2021)",
    y = "Predicted probability",
    color = "Model",
    title = "Marginal predicted probability of life satisfaction at follow-up by obesity status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))




crude %>%
  group_by(parentPhys_cat) %>%
  summarise(
    n = n(),
    wishes_to_change_n = sum(WCT_21_bin == "wishes to change", na.rm = TRUE),
    wishes_to_change_pct = round(wishes_to_change_n / n * 100, 1)
  )

chisq.test(table(crude$WCT_21_bin, crude$parentPhys_cat))

crude %>%
  group_by(parentPhys_cat, obe21_bin) %>%
  summarise(
    n = n(),
    wishes_to_change_pct = round(
      sum(WCT_21_bin == "wishes to change", na.rm = TRUE) / n * 100, 1)
  )

##forest plot----
library(ggplot2)
library(dplyr)

# Build the H4 sequential model data frame
h4_data <- tibble(
  model = factor(c("Model 0\n(H1 baseline)",
                   "Model 1\n(+ age)",
                   "Model 2\n(+ diploma)",
                   "Model 3\n(+ family history)"),
                 levels = c("Model 3\n(+ family history)",
                            "Model 2\n(+ diploma)",
                            "Model 1\n(+ age)",
                            "Model 0\n(H1 baseline)")),
  OR    = c(0.664, 0.670, 0.670, 0.701),
  lower = c(0.595, 0.601, 0.601, 0.626),
  upper = c(0.743, 0.749, 0.749, 0.787)
)

ggplot(h4_data, aes(x = OR, y = model)) +
  geom_point(size = 3, color = "#366092") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2, linewidth = 0.7,
                 color = "#366092") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "grey50",
             linewidth = 0.5) +
  geom_vline(xintercept = 0.664,
             linetype = "dotted",
             color = "#366092",
             alpha = 0.4,
             linewidth = 0.5) +
  scale_x_continuous(limits = c(0.5, 1.1),
                     breaks = seq(0.5, 1.1, 0.1)) +
  labs(
    x = "Odds ratio (95% CI)",
    y = NULL,
    title = "Sequential adjustment of the obesity-LS association",
    subtitle = "Obesity OR across H4 models, crude sample"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  )



#playground----
##weight perception during teen :((----
teenPerc <- crude %>% run_polr(
  "teenPerc",
  LS24_cat ~ obe21_bin * WS_b21 + LS21_cat
)

##Weight perception life course pattern frequency table ----
wp_patterns <- wp_patterns %>%
  mutate(
    lc_wp_typology = case_when(
      
      # Reference: never heavier at any stage
      wp_pattern == "0000" ~ "never",
      
      # Adult onset only: heavier at 25+ only
      wp_pattern == "0001" ~ "adult onset only",
      
      # Early/mid onset, not persisting into adulthood (25+ = 0)
      wp_pattern %in% c("1000", "0100", "0010",
                        "1100", "0110", "1010") ~ "onset before adulthood, not persisting",
      
      # Onset before adulthood, persisting into adulthood (25+ = 1)
      wp_pattern %in% c("0011", "0111", "1111",
                        "1001", "0101", "1011",
                        "1101", "1110") ~ "onset before adulthood, persisting",
      
      # Missing: any item missing
      is.na(wp_pattern) ~ NA_character_
    ),
    lc_wp_typology = factor(
      lc_wp_typology,
      levels = c("never",
                 "adult onset only",
                 "onset before adulthood, not persisting",
                 "onset before adulthood, persisting")
    )
  )

##CWP and parental obesity----
# Association between CWP and parental body size
# Purpose: examine whether perceived childhood weight was
# associated with parental body size — two effect modifiers
# that may share variance and inform each other's interpretation

library(tidyverse)

# --- 1. Contingency table: counts ---
cwp_parent_sample <- crude %>%
  filter(!is.na(CWP_21) & !is.na(parentPhys_cat))

cwp_parent_tab <- table (
  CWP = cwp_parent_sample$CWP_21,
  Parental_size = cwp_parent_sample$parentPhys_cat
)
print(cwp_parent_tab)

# --- 2. Column proportions:
# "Among those with neither/one/both large parents,
#  what % perceived themselves as heavier/thinner/no difference?"
round(prop.table(cwp_parent_tab, margin = 2) * 100, 1)

# --- 3. Chi-squared test ---
chisq_result <- chisq.test(cwp_parent_tab)
print(chisq_result)

# Check expected cell counts — all should be >= 5
# If not, interpret chi-squared with caution
chisq_result$expected

# --- 4. Cramér's V (effect size for chi-squared) ---
# Benchmarks: < 0.10 negligible | 0.10–0.30 small |
#             0.30–0.50 moderate | > 0.50 large
n   <- sum(cwp_parent_tab)
k   <- min(dim(cwp_parent_tab))   # smaller of rows/columns
V   <- sqrt(chisq_result$statistic / (n * (k - 1)))
cat("Cramér's V:", round(V, 3), "\n")

# --- 5. Visualisation ---
as.data.frame(cwp_parent_tab) %>%
  group_by(Parental_size) %>%
  mutate(prop = Freq / sum(Freq)) %>%
  ggplot(aes(x = Parental_size, y = prop, fill = CWP)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.4, size = 3
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "no difference" = "#366092",
    "heavier"       = "#C0504D",
    "thinner"       = "#9BB8D4"
  )) +
  labs(
    title    = "Childhood weight perception by parental body size",
    subtitle = "Column proportions within each parental body size group",
    x        = "Parental body size",
    y        = "Proportion",
    fill     = "Childhood weight\nperception"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

as.data.frame(
  wp_patterns %>%
    count(lc_wp_typology, name = "n") %>%
    mutate(pct = round(n / sum(n) * 100, 2))
)

###age cut off at 65?----

table(cut(crude$age_2021_imputed, 
          breaks = c(0, 50, 65, Inf), 
          labels = c("under 50", "50 to 64", "65 and above")))

class(crude$BMI_21)
summary(crude$BMI_21)

# ── Step 1: compute BMI z-score within the analytical sample ──────────────

crude <- crude %>%
  mutate(
    BMI_zscore = (BMI_21 - mean(BMI_21, na.rm = TRUE)) / sd(BMI_21, na.rm = TRUE)
  )

# sanity check
mean(crude$BMI_zscore, na.rm = TRUE)   # should be ~0
sd(crude$BMI_zscore, na.rm = TRUE)     # should be ~1

# ── Step 2: run the model ─────────────────────────────────────────────────

H_BMIz_age_simple <- crude %>% run_polr(
  "H_BMIz_age_simple",
  LS24_cat ~ obe21_bin + BMI_zscore * age_2021_imputed + LS21_cat
)

library(MASS)

# Refit the model directly with polr
H_BMIz_age_polr <- polr(
  LS24_cat ~ obe21_bin + BMI_zscore * age_2021_imputed + LS21_cat,
  data   = crude,
  Hess   = TRUE,
  method = "logistic"
)

# Confirm it is the right class
class(H_BMIz_age_polr)

summary(crude$age_2021_imputed)
max(crude$age_2021_imputed, na.rm = TRUE)

crude %>%
  filter(age_2021_imputed > 80) %>%
  count()


####visualization----
library(ggplot2)
library(tidyr)

# Step 1: create prediction grid
age_seq <- seq(min(crude$age_2021_imputed, na.rm = TRUE),
               max(crude$age_2021_imputed, na.rm = TRUE),
               length.out = 100)

pred_grid <- expand.grid(
  age_2021_imputed = age_seq,
  obe21_bin        = levels(crude$obe21_bin),
  BMI_zscore       = 0,
  LS21_cat         = "neutral"
)

# Step 2: predicted probabilities
pred_probs <- predict(H_BMIz_age_polr,
                      newdata = pred_grid,
                      type    = "probs")

pred_df <- cbind(pred_grid, pred_probs)

# Step 3: reshape to long format
pred_long <- pred_df %>%
  pivot_longer(
    cols      = c("dissatisfied", "neutral", "satisfied"),
    names_to  = "LS_category",
    values_to = "probability"
  )

# Step 4: plot
ggplot(pred_long,
       aes(x        = age_2021_imputed,
           y        = probability,
           color    = obe21_bin,
           linetype = obe21_bin)) +
  geom_smooth(se     = TRUE,
              method = "loess",
              span   = 0.5) +
  facet_wrap(~ LS_category) +
  scale_color_manual(values = c("non-obese" = "#2166ac",
                                "obese"     = "#d6604d")) +
  labs(
    x        = "Age at baseline (years)",
    y        = "Predicted probability",
    color    = "Obesity status",
    linetype = "Obesity status",
    title    = "Predicted probability of life satisfaction across age",
    subtitle = "BMI z-score fixed at sample mean, baseline LS fixed at neutral"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

##hereditary thinness?----
crude %>%
  filter(BMI_21_label == "Underweight") %>%
  count(parentPhys_cat)

##BMI by z-score----
library(ggplot2)

# Compute z-score of BMI_21 within the crude sample
crude <- crude %>%
  mutate(BMI_21_z = (BMI_21 - mean(BMI_21, na.rm = TRUE)) / 
           sd(BMI_21, na.rm = TRUE))

# Plot
ggplot(crude, aes(x = BMI_21_z)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60,
                 fill = "#4575b4",
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "orange",
             linewidth = 0.7) +
  labs(
    title    = "BMI distribution of crude sample at baseline (2021)",
    subtitle = paste0("N = ", sum(!is.na(crude$BMI_21)),
                      "  |  Mean BMI = ",
                      round(mean(crude$BMI_21, na.rm = TRUE), 1),
                      " kg/m²  |  SD = ",
                      round(sd(crude$BMI_21, na.rm = TRUE), 1)),
    x        = "BMI z-score (standardised within crude sample)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey50")
  )
