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
    # parentPhys_cat = factor(case_when(
    #   momPhys_21_large == 1 & dadPhys_21_large == 1 ~ "both",
    #   momPhys_21_large == 1 | dadPhys_21_large == 1 ~ "one parent",
    #   momPhys_21_large == 0 & dadPhys_21_large == 0 ~ "neither"
    # ), levels = c("neither", "one parent", "both"))
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


library(tidyverse)
# install.packages("MASS")
library(MASS)





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
##crude ----
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

##crude adjusted----
H1_crude <- crude %>% run_polr(
  "H1_crude",
  LS24_cat ~ obe21_bin + age_2021_imputed
)


##restrictive----
H1_res <- restrictive %>% run_polr(
  "H1_res",
  LS24_cat ~ obe21_bin
)
nobs(H1_res)
margPre_H1_res <- run_margins(H1_res, "obe21_bin")

plot_margins(margPre_H1_res, "obe21_bin",
             x_label = "Obesity Status (2021)",
             title = "Predicted probability of life satisfaction (2024) by obesity status, restrictive sample")



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
table(crude$ob_trajectory, useNA = "ifany")

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

##***----
##forest plot----
##***----
library(dplyr); library(ggplot2); library(tibble)

extract_or <- function(model, block, drop = "age_2021_imputed") {
  ci <- suppressMessages(exp(cbind(OR = coef(model), confint(model))))
  as_tibble(ci, rownames = "term") %>%
    rename(lower = `2.5 %`, upper = `97.5 %`) %>%
    filter(!term %in% drop) %>%
    mutate(block = block)
}

h2_forest <- bind_rows(
  extract_or(H1_crude,            "Binary\n(ref: non-obese)"),
  extract_or(H2_severity_crude,   "Severity\n(ref: healthy weight)"),
  extract_or(H2_obePersist_crude, "Persistence\n(ref: obese at neither wave)"),
  extract_or(H2_AT_crude,         "AT typology\n(ref: concordant healthy)"),
  extract_or(H2_obTraj_crude,     "Trajectory\n(ref: never obese)")
) %>%
  mutate(
    label = term %>%
      sub("^obe21_bin", "", .) %>%
      sub("^BMI_21_label", "", .) %>%
      sub("^obePersist", "", .) %>%
      sub("^typology_adult", "", .) %>%
      sub("^ob_trajectory", "", .),
    block = factor(block, levels = unique(block)),
    label = factor(label, levels = rev(label))
  )

ggplot(h2_forest, aes(OR, label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
  geom_point(size = 2.6) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(trans = "log", breaks = c(.4,.5,.6,.8,1.0)) +
  labs(x = "Odds ratio (95% CI)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold"),
        panel.grid.major.y = element_blank())


#H3----
##CWP----
###crude----
H3_CWP_crude <- crude %>% run_polr(
  "H3_CWP_crude", 
  LS24_cat ~ obe21_bin * CWP_21 + age_2021_imputed
  )
nobs(H3_CWP_crude)
margPre_H3_CWP_crude <- run_margins(H3_CWP_crude, "CWP_21")

plot_margins(margPre_H3_CWP_21, "CWP_21",
             x_label = "Childhood (before age 13) weight perception",
             title = "Predicted probability of life satisfaction (2024) by childhood weight perception, crude sample")

###restrictive----
H3_CWP_res <- restrictive %>% run_polr(
  "H3_CWP_res", 
  LS24_cat ~ obe21_bin * CWP_21 + age_2021_imputed
  )
nobs(H3_CWP_res)
margPre_H3_CWP_res <- run_margins(H3_CWP_res, "CWP_21")




##AWP----
###crude----
H3_AWP_crude <- crude %>% run_polr(
  "H3_AWP_crude", 
  LS24_cat ~ obe21_bin * AWP_21 + age_2021_imputed
  )
nobs(H3_AWP_crude)
margPre_H3_AWP_crude <- run_margins(H3_AWP_crude, "AWP_21")

###restrictive----
H3_AWP_res <- restrictive %>% run_polr(
  "H3_AWP_res", 
  LS24_cat ~ obe21_bin * AWP_21 + age_2021_imputed
)
nobs(H3_AWP_res)
margPre_H3_AWP_res <- run_margins(H3_AWP_res, "AWP_21")



##sex-disaggregated parental----
###mom crude----
H3_mom_crude <- crude %>% run_polr(
  "H3_mom_crude", 
  LS24_cat ~ obe21_bin * momPhys_21_large + age_2021_imputed
  )
nobs(H3_mom_crude)
margPre_H3_mom_crude <- run_margins(H3_mom_crude, "momPhys_21_large")


###mom restrictive----
H3_mom_res <- restrictive %>% run_polr(
  "H3_mom_res", 
  LS24_cat ~ obe21_bin * momPhys_21_large + age_2021_imputed
)
nobs(H3_mom_res)
margPre_H3_mom_res <- run_margins(H3_mom_res, "momPhys_21_large")


###dad crude----
H3_dad_crude <- crude %>% run_polr(
  "H3_dad_crude", 
  LS24_cat ~ obe21_bin * dadPhys_21_large + age_2021_imputed
)
nobs(H3_dad_crude)
margPre_H3_dad_crude <- run_margins(H3_dad_crude, "dadPhys_21_large")


###dad restrictive----
H3_dad_res <- restrictive %>% run_polr(
  "H3_dad_res", 
  LS24_cat ~ obe21_bin * dadPhys_21_large + age_2021_imputed
)
nobs(H3_dad_res)
margPre_H3_dad_res <- run_margins(H3_dad_res, "dadPhys_21_large")



#H4----
##crude----
H4_crude <- crude %>% run_polr(
  "H4_crude",
  LS24_cat ~ obe21_bin + age_2021_imputed + momPhys_21_large + dadPhys_21_large
)
nobs(H4_crude)
margPre_H4_crude <- run_margins(H4_crude, "obe21_bin")


##restrictive----
H4_res <- restrictive %>% run_polr(
  "H4_res",
  LS24_cat ~ obe21_bin + age_2021_imputed + momPhys_21_large + dadPhys_21_large
)
nobs(H4_res)
margPre_H4_res <- run_margins(H4_res, "obe21_bin")

##***----
##figure----
##***----
h4_compare <- bind_rows(
  extract_or(H1_crude, "Unadjusted") %>% filter(grepl("obe21_bin", term)),
  extract_or(H4_crude, "Adjusted for age and\nsex-disaggregated parental body size") %>% filter(grepl("obe21_bin", term))
)

ggplot(h4_compare, aes(OR, block)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .12) +
  geom_point(size = 3) +
  scale_x_continuous(trans = "log", breaks = c(.5,.6,.7,.8,.9,1.0),
                     limits = c(.48, 1.05)) +
  labs(x = "Odds ratio (95% CI)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        aspect.ratio = .22)




#Non-participation analysis----
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

smd_vars <- c("BMI_21", "LS21", "age_2021_imputed", "edu_21", "workSchedule_3cat")

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
