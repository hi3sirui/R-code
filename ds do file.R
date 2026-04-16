library(dplyr)

#ds <- read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v5.csv")
ds <- read.csv("/Users/siruizhang/Thesis/Data_Lexi_v5 - Copy.csv")

View(ds)

#PREP----
# wave1 <- names(ds)[5:22]
# wave2 <- names(ds)[24:44]
# table(rowSums(is.na(ds[, wave1])))
# table(rowSums(is.na(ds[, wave2])))

ds <- ds %>%
  rename(LS21 = quality_of_life_a_k,
         H21 = height_k,
         W21 = weight_k,
         waist21 = waist_k,
         WS_a21 = weight_statements_a_k,
         WS_b21 = weight_statements_b_k,
         WS_c21 = weight_statements_c_k,
         WS_d21 = weight_statements_d_k,
         WCT_21 = weight_change_k,
         momPhys_21 = physique_mom_k,
         dadPhys21 = physique_dad_k,
         age_2021 = cpr_alder,
         speUd_21  = work_b_k1,
         diplUd_21 = work_b_k2,
         mastUd_21 = work_b_k3,
         kandiUd_21 = work_b_k4,
         PhD = work_b_k5,
         #work: shift type
         daySche_21 = work_schedule_a_k,
         nightSche_21 = work_schedule_b_k,
         eveSche_21 = work_schedule_c_k,
         #no. of years working a shift type
         dayScheYrs_21 = day_hours_k,
         nightScheYrs_21 = evening_hours_k,
         eveScheYrs_21 = night_hours_k,
         age_2024 = age,
         LS24 = qol,
         phyHealth_24 = phy_health_v2,
         mentHealth_24 = men_health_v2,
         W24 = weight_k_v2,
         H24 = height_k_v2,
         WS_a24 = weight_statements_a_k_v2,
         WCT_24 = weight_change_k_v2,
         momPhys_24 = physique_mom_k_v2,
         dadPhys24 = physique_dad_k_v2,
         #shift frequency / wk 
         nightFreq_24 = eve,
         eveFreq_24 = night,
         weekendFreq_24 = weekend,
         #no. of yrs working a shift type
         nightScheYrs_24 = evening_hours_k_v2,
         eveScheYrs_24 = night_hours_k_v2,
         lgbtID = lbgt,
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
         cpr_sex==1)


##WEIGHT----
###weight rescue----
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




##HEIGHT----
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








##BMI----
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



##LS----
ds <- ds %>%
  mutate(across(c(LS21, LS24), 
                ~case_when(
                  . <= 4 ~ "dissatisfied",
                  . == 5 ~ "neutral",
                  . >= 6 ~ "satisfied"
                ), 
                .names = "{.col}_label"))

ds <- ds %>%
  mutate(
    LS21_cat = factor(LS21_label, 
                      levels = c("dissatisfied", "neutral", "satisfied"),
                      ordered = TRUE),
    LS24_cat = factor(LS24_label, 
                      levels = c("dissatisfied", "neutral", "satisfied"),
                      ordered = TRUE)
  )

##dichotomize LGBT----
ds <- ds %>%
  mutate(
    lgbt_binary = case_when(
      lgbtID == 1 ~ 1,
      lgbtID == 2 ~ 0,
      lgbtID %in% c(3, 4) ~ NA_real_
    )
  )

table(ds$lgbt_binary, useNA = "always")
table(ds$eveSche_21, useNA = "always")

#crude sample: 17174----
sum(!is.na(ds$BMI_21) & !is.na(ds$LS21) & !is.na(ds$LS24))
crude_sample <- ds %>%
  filter(!is.na(ds$BMI_21) & !is.na(ds$LS21) & !is.na(ds$LS24))




#***----
#*
#*
#H1----
install.packages("MASS")
library(tidyverse)
library(MASS)


H1 <- polr(LS24_cat ~ obese_21, 
                 data = crude_sample, 
                 Hess = TRUE)

summary(H1)
exp(cbind(OR = coef(H1), confint(H1)))

##H1 + LS21_cat----
H1_long <- polr(LS24_cat ~ obeStat_21 + LS21_cat,
                data = crude_sample,
                Hess = TRUE)
summary(H1_long)
exp(cbind(OR = coef(H1_long), confint(H1_long)))

H1_linear <- lm(LS24 ~ obeStat_21+ LS21,
                data = crude_sample)
summary(H1_linear)
AIC(H1_linear)

##marginal predictions----
install.packages("marginaleffects")
library(marginaleffects)

pred_data <- avg_predictions(H1_long, 
                             variables = "obeStat_21",
                             type = "probs") %>%
  as.data.frame() %>%
  mutate(obese = if_else(obese_21 == 0, "Non-obese", "Obese"),
         LS_category = factor(group,
                              levels = c("dissatisfied","neutral","satisfied")))

ggplot(pred_data, aes(x = obese, y = estimate, fill = LS_category)) +
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
    title = "Predicted probability of life satisfaction by obesity status",
    subtitle = "Adjusted for baseline life satisfaction (2021)",
    x = "Obesity status (2021)",
    y = "Predicted probability",
    fill = "Life satisfaction (2024)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
















#H2: effect modifiers----

##H2 sample size: 16390----
H2_sample <- crude_sample %>%
  filter(!is.na(obePersist) & 
           !is.na(CWP_21) & 
           !is.na(parentPhys_cat))



##obesity persistence----
ds <- ds %>%
  mutate(
    obeStat_21 = case_when(
      BMI_21 >= 30 ~ 1,
      BMI_21 < 30  ~ 0
    ),
    obeStat_24 = case_when(
      BMI_24 >= 30 ~ 1,
      BMI_24 < 30  ~ 0
    ),
    obePersist = case_when(
      obeStat_21 == 0 & obeStat_24 == 0 ~ "never",
      obeStat_21 == 1 & obeStat_24 == 0 ~ "2021 only",
      obeStat_21 == 0 & obeStat_24 == 1 ~ "2024 only",
      obeStat_21 == 1 & obeStat_24 == 1 ~ "both waves"
    )
  )

table(ds$obePersist, useNA = "always")

ds <- ds %>%
  mutate(
    obePersist = factor(obePersist,
                        levels = c("never", "2021 only", 
                                   "2024 only", "both waves"))
  )

table(ds$obePersist, useNA = "always")


###H2_obePersist: LS24cat ~ obePersist + LS21_cat----
H2_obePersist <- polr(LS24_cat ~ obePersist + LS21_cat,
                   data = H2_sample,
                   Hess = TRUE)

summary(H2_obePersist)
exp(cbind(OR = coef(H2_obePersist), confint(H2_obePersist)))

###H2_obePersist marg predicted prob----
library(marginaleffects)
avg_predictions(H2_obePersist, 
                variables = "obePersist",
                type = "probs")



###visual----
pred_persist <- avg_predictions(H2_obePersist, 
                                variables = "obePersist",
                                type = "probs") %>%
  as.data.frame() %>%
  mutate(
    obePersist = factor(obePersist, 
                        levels = c("never", "2021 only", 
                                   "2024 only", "both waves")),
    group = factor(group, 
                   levels = c("dissatisfied", "neutral", "satisfied"))
  )

ggplot(pred_persist, aes(x = obePersist, y = estimate, fill = group)) +
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
    title = "Predicted probability of life satisfaction by obesity persistence",
    subtitle = "Adjusted for baseline life satisfaction (2021)",
    x = "Obesity persistence status",
    y = "Predicted probability",
    fill = "Life satisfaction (2024)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


##childhood weight perception ----
table(ds$WS_a21, useNA = "always")
ds <- ds %>%
  mutate(
    CWP_21 = factor(case_when(
      WS_a21 == 1 ~ "heavier",
      WS_a21 == 2 ~ "thinner",
      WS_a21 == 3 ~ "no difference"
    ),
    levels = c("no difference", "heavier", "thinner"))
  )

table(ds$CWP_21, useNA = "always")



###H2_CWP: LS24cat ~ CWP_21 + LS21_cat----
H2_CWP <- polr(LS24_cat ~ CWP_21 + LS21_cat,
               data = H2_sample,
               Hess = TRUE)

summary(H2_CWP)
exp(cbind(OR = coef(H2_CWP), confint(H2_CWP)))

###H2_CWP marg predicted prob & visual----
pred_CWP <- avg_predictions(H2_CWP, 
                            variables = "CWP_21",
                            type = "probs") %>%
  as.data.frame() %>%
  mutate(
    CWP_21 = factor(CWP_21, 
                    levels = c("no difference", "heavier", "thinner")),
    group = factor(group,
                   levels = c("dissatisfied", "neutral", "satisfied"))
  )

ggplot(pred_CWP, aes(x = CWP_21, y = estimate, fill = group)) +
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
    title = "Predicted probability of life satisfaction by childhood weight perception",
    subtitle = "Adjusted for baseline life satisfaction (2021)",
    x = "Childhood weight perception (before age 13)",
    y = "Predicted probability",
    fill = "Life satisfaction (2024)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")







##parental body size ----
table(ds$momPhys_21, useNA = "always")
table(ds$dadPhys21, useNA = "always")

ds <- ds %>%
  mutate(
    momPhys_21_large = case_when(
      momPhys_21 >= 1 & momPhys_21 <= 3 ~ 1,
      momPhys_21 >= 4 & momPhys_21 <= 9 ~ 0
    ),
    dadPhys_21_large = case_when(
      dadPhys21 >= 1 & dadPhys21 <= 3 ~ 1,
      dadPhys21 >= 4 & dadPhys21 <= 9 ~ 0
    ),
    parentPhys_cat = case_when(
      momPhys_21_large == 1 & dadPhys_21_large == 1 ~ "both",
      momPhys_21_large == 1 | dadPhys_21_large == 1 ~ "one parent",
      momPhys_21_large == 0 & dadPhys_21_large == 0 ~ "neither"
    ),
    parentPhys_cat = factor(parentPhys_cat,
                            levels = c("neither", "one parent", "both"))
  )

table(ds$parentPhys_cat, useNA = "always")

###H2_parent: LS24_cat ~ parentPhys_cat + LS21_cat----
H2_parent <- polr(LS24_cat ~ parentPhys_cat + LS21_cat,
                  data = H2_sample,
                  Hess = TRUE)

summary(H2_parent)
exp(cbind(OR = coef(H2_parent), confint(H2_parent)))
###H2_parent marg predicted prob & visual----
pred_parent <- avg_predictions(H2_parent, 
                               variables = "parentPhys_cat",
                               type = "probs") %>%
  as.data.frame() %>%
  mutate(
    parentPhys_21_large = factor(parentPhys_cat,
                                 levels = c("neither", "one parent", "both")),
    group = factor(group, levels = c("dissatisfied", "neutral", "satisfied"))
  )

ggplot(pred_parent, aes(x = parentPhys_21_large, y = estimate, fill = group)) +
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
    title = "Predicted probability of life satisfaction by parental body size",
    subtitle = "Adjusted for baseline life satisfaction (2021)",
    x = "Parental body size",
    y = "Predicted probability",
    fill = "Life satisfaction (2024)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



##testing prediction accuracy----
###brant test: all passed----
install.packages("brant")
library(brant)
brant(H1_long)
brant(H2_CWP)
brant(H2_obePersist)
brant(H2_parent)

###model fit assessment----
install.packages("DescTools")
library(DescTools)
PseudoR2(H1_long, which = c("McFadden", "Nagelkerke"))
PseudoR2(H2_obePersist, which = c("McFadden", "Nagelkerke"))
PseudoR2(H2_CWP, which = c("McFadden", "Nagelkerke"))
PseudoR2(H2_parent, which = c("McFadden", "Nagelkerke"))









#TABLE 1----
library(gtsummary)
##2021 panel----
crude_sample %>%
  mutate(obeStat_21 = factor(obeStat_21, 
                             levels = c(0,1),
                             labels = c("Non-obese", "Obese"))) %>%
  dplyr::select(obeStat_21, age_2021_imputed, BMI_21, LS21, 
                obePersist, CWP_21, parentPhys_cat) %>%
  tbl_summary(
    by = obeStat_21,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age (years)",
      BMI_21 ~ "BMI (kg/m²)",
      LS21 ~ "Life satisfaction (0-10)",
      obePersist ~ "Obesity persistence",
      CWP_21 ~ "Childhood weight perception",
      parentPhys_cat ~ "Parental body size"
    )
  ) %>%
  add_overall() %>%
  modify_spanning_header(all_stat_cols() ~ "**2021**") %>%
  bold_labels()

