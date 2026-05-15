library(dplyr)

# ds <- read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v5.csv")
ds <- read.csv("/Users/siruizhang/Thesis/Data_Lexi_v5 - Copy.csv")
# crude <- read.csv("C:/Users/SZHA0012/Documents/crude sample.csv")
# restrictive <- read.csv("C:/Users/SZHA0012/Documents/crude sample.csv")

# test <-  read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v5.csv")
test <- read.csv("/Users/siruizhang/Thesis/Data_Lexi_v5 - Copy.csv")
View(test)

test %>%
  count(cpr_alder < 25)

#PREP----
ds <- ds %>%
  rename(LS21 = quality_of_life_a_k,
         H21 = height_k,
         W21 = weight_k,
         waist21 = waist_k,
         CWP_21 = weight_statements_a_k,
         WS_b21 = weight_statements_b_k,
         WS_c21 = weight_statements_c_k,
         AWP_21 = weight_statements_d_k,
         WCT_21 = weight_change_k,
         momPhys_21 = physique_mom_k,
         dadPhys_21 = physique_dad_k,
         age_2021 = cpr_alder,
         speUd_21  = work_b_k1,
         diplUd_21 = work_b_k2,
         mastUd_21 = work_b_k3,
         kandiUd_21 = work_b_k4,
         PhD = work_b_k5,
         #work: shift type
         daySche_21 = work_schedule_a_k,
         eveSche_21 = work_schedule_b_k,
         nightSche_21 = work_schedule_c_k,
         #no. of years working a shift type
         dayScheYrs_21 = day_hours_k,
         eveScheYrs_21 = evening_hours_k,
         nightScheYrs_21 = night_hours_k,
         age_2024 = age,
         LS24 = qol,
         phyHealth_24 = phy_health_v2,
         mentHealth_24 = men_health_v2,
         W24 = weight_k_v2,
         H24 = height_k_v2,
         CWP_24 = weight_statements_a_k_v2,
         AWP_24 = weight_statements_d_k_v2,
         WCT_24 = weight_change_k_v2,
         momPhys_24 = physique_mom_k_v2,
         dadPhys_24 = physique_dad_k_v2,
         #shift frequency / wk 
         eveFreq_24 = eve,
         nightFreq_24 = night,
         weekendFreq_24 = weekend,
         #no. of yrs working a shift type
         eveScheYrs_24 = evening_hours_k_v2,
         nightScheYrs_24 = night_hours_k_v2,
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
##dichotomize BMI----
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
    
    ##obesity persistence----
    obePersist = factor(case_when(
      obe21_bin == "non-obese" & obe24_bin == "non-obese" ~ "never",
      obe21_bin == "obese" & obe24_bin == "non-obese" ~ "2021 only",
      obe21_bin == "non-obese" & obe24_bin == "obese"     ~ "2024 only",
      obe21_bin == "obese" & obe24_bin == "obese"     ~ "both waves"
    ), levels = c("never", "2021 only", "2024 only", "both waves"))
  )


#weight change thoughts----
ds <- ds %>%
  mutate(
    WCT_21_bin = factor(case_when(
      WCT_21 == 1 ~ "lose weight",
      WCT_21 == 2 | WCT_21 == 3 ~ "gain or no change",
      TRUE ~ NA_character_
    ), levels = c("gain or no change", "lose weight"))
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
    momPhys_21_AB = case_when(
      momPhys_21 == 1 | momPhys_21 == 2 ~ "1",
      momPhys_21 >2 ~ "0"
    ),
    dadPhys_21_AB = case_when(
    dadPhys_21 == 1 | dadPhys_21 == 2 ~ "1",
    dadPhys_21 > 2 ~ "0"
    ),
    parentPhys_AB = factor(case_when(
      momPhys_21_AB == 1 & dadPhys_21_AB == 1 ~ "both",
      momPhys_21_AB == 1 | dadPhys_21_AB == 1 ~ "one parent",
      momPhys_21_AB == 0 & dadPhys_21_AB == 0 ~ "neither"
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

#diploma edu----
ds <- ds %>%
  mutate(
    diplUd_21 = factor(diplUd_21,
      levels = c(0, 1),
      labels = c("no", "yes")
    )
    )


ds <- ds %>%
  mutate(
    diplUd_21_bin = case_when(
      diplUd_21 == "yes" ~ "yes",
      diplUd_21 == "no"  ~ "no",
      is.na(diplUd_21)   ~ "no",  # checkbox non-response = did not attain
      TRUE ~ NA_character_
    ) %>% factor(levels = c("no", "yes"))
  )

#working night schedule----
ds <- ds %>%
  mutate(nightSche_grp = factor(case_when(
    nightSche_21 == 1 ~ "yes",
    nightSche_21 == 0 ~ "no",
    is.na(nightSche_21) ~ "no"
  ), levels = c("no", "yes"))
  )

#working mixed shift----
ds <- ds %>%
  mutate(work_schedule_d_k == factor(case_when(
    work_schedule_d_k == 1 ~ "yes",
    work_schedule_d_k == 0 ~ "no",
    is.na(work_schedule_d_k) ~ "no"
  ), levels = c("no", "yes")
  )
  )

#working anything other than day schedule----
ds <- ds %>%
  mutate(
    dayOnly_21 = factor(
      if_else(daySche_21 == 1, "day schedule", "other"),
      levels = c("other","day schedule")
    )
  )

#working anything other than mixed shift----
ds <- ds %>%
  mutate(
    mixedOnly = factor(
      if_else(work_schedule_d_k == 1, "mixed schedule", "other"),
      levels = c( "other", "mixed schedule")
    )
  )


#family history of overweight----
ds <- ds %>%
  mutate(
    obeInh_24 = factor(case_when(
      obeInh_24 == 0 ~ "no",
      obeInh_24 == 1 ~ "yes"
    ), levels = c("no", "yes"))
  )
  

#Childhood weight perception 2021----
ds <- ds %>%
  mutate(
    CWP_21 = factor(case_when(
      CWP_21 == 1 ~ "heavier",
      CWP_21 == 2 ~ "thinner",
      CWP_21 == 3 ~ "no difference"
    ),
    levels = c("no difference", "heavier", "thinner"))
  )

#teenage years weight perception----
ds <- ds %>%
  mutate(
    WS_b21 = factor(case_when(
      WS_b21 == 1 ~ "heavier",
      WS_b21 == 2 ~ "thinner",
      WS_b21 == 3 ~ "no difference"
    ), levels = c("no difference", "heavier", "thinner"))
  )

#Adulthood weight perception in 2021----
ds <- ds %>%
  mutate(
    AWP_21 = factor(case_when(
      AWP_21 == 1 ~ "heavier",
      AWP_21 == 2 ~ "thinner",
      AWP_21 == 3 ~ "no difference"
    ),
    levels = c("no difference", "heavier", "thinner"))
  )


#Typology----
##CWP----
ds <- ds %>%
  mutate(
    typology_child = factor(case_when(
      obe21_bin == "non-obese" & CWP_21 == "no difference" ~ "concordant healthy",
      obe21_bin == "obese"     & CWP_21 == "heavier"        ~ "concordant heavy",
      obe21_bin == "non-obese" & CWP_21 == "heavier"        ~ "over-perceiver",
      obe21_bin == "obese"     & CWP_21 == "no difference"  ~ "under-perceiver",
      obe21_bin == "obese"     & CWP_21 == "thinner"        ~ "under-perceiver"
    ), levels = c("concordant healthy", "concordant heavy",
                  "over-perceiver", "under-perceiver"))
  )

##AWP----
ds <- ds %>%
  mutate(
    typology_adult = factor(case_when(
      obe21_bin == "non-obese" & AWP_21 == "no difference" ~ "concordant healthy",
      obe21_bin == "obese"     &  AWP_21 == "heavier"        ~ "concordant heavy",
      obe21_bin == "non-obese" &  AWP_21 == "heavier"        ~ "over-perceiver",
      obe21_bin == "obese"     &  AWP_21 == "no difference"  ~ "under-perceiver",
      obe21_bin == "obese"     &  AWP_21 == "thinner"        ~ "under-perceiver"
    ), levels = c("concordant healthy", "concordant heavy",
                  "over-perceiver", "under-perceiver"))
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
View(ds)

## Restrictive----
restrictive <- ds %>%
  filter(
    !is.na(BMI_21),
    !is.na(BMI_24),
    !is.na(LS21),
    !is.na(LS24),
    !is.na(CWP_21),
    !is.na(AWP_21),
    !is.na(momPhys_21),
    !is.na(dadPhys_21),
    !is.na(diplUd_21_bin),
    !is.na(obeInh_24),
    !is.na(age_2021_imputed)
  )
nrow(restrictive)

raw_res <- ds %>%
  filter(
    !is.na(BMI_21),
    !is.na(BMI_24),
    !is.na(LS21),
    !is.na(LS24),
    !is.na(CWP_21),
    !is.na(AWP_21),
    !is.na(momPhys_21),
    !is.na(dadPhys_21),
    !is.na(diplUd_21),
    !is.na(obeInh_24),
    !is.na(age_2021_imputed)
  )
nrow(raw_res)


sum(is.na(crude$CWP_21))
sum(is.na(crude$AWP_21))
sum(is.na(crude$momPhys_21))
sum(is.na(crude$dadPhys_21))
sum(is.na(crude$diplUd_21))
sum(is.na(crude$diplUd_21_bin))
sum(!complete.cases(crude[, c("CWP_21", "AWP_21")]))




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


library(ggplot2)

ggplot(crude, aes(x = LS24)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Distribution of Life Satisfaction at Baseline (2021)",
       x = "Life Satisfaction (0-10)",
       y = "Count") +
  theme_minimal()


#H1----
library(MASS)
library(tidyverse)

##crude ----
###unadjusted----
H1_crude <- crude %>% run_polr(
  "H1_crude",
  LS24_cat ~ obe21_bin
)
nobs(H1_crude)
# install.packages("marginaleffects")

margPre_H1_crude <- run_margins(H1_crude, "obe21_bin")

plot_margins(margPre_H1_crude, "obe21_bin",
             x_label = "Obesity Status (2021)",
             title = "Predicted life satisfaction category (2024) by obesity status"
             )

####adjusted----
H1_crudeAdj <- crude %>% run_polr(
  "H1_crudeAdj",
  LS24_cat ~ obe21_bin + LS21_cat
)
nobs(H1_crudeAdj)

margPre_H1_crudeAdj <- run_margins(H1_crudeAdj, "obe21_bin")


##restrictive----
###unadjusted----
H1_res <- restrictive %>% run_polr(
  "H1_res",
  LS24_cat ~ obe21_bin
)
nobs(H1_res)
margPre_H1_res <- run_margins(H1_res, "obe21_bin")


###adjusted----
H1_resAdj <- restrictive %>% run_polr(
  "H1_resAdj",
  LS24_cat ~ obe21_bin + LS21_cat
)
nobs(H1_resAdj)
margPred_H1_resAdj <- run_margins(H1_resAdj, "obe21_bin")


plot_margins(margPred_H1_resAdj, "obe21_bin",
             x_label = "Obesity Status (2021)",
             title = "Predicted probability of life satisfaction (2024) by obesity status, restrictive sample")












#H2----
##severity: by label, no product term----
###crude----
H2_severity <- crude %>% run_polr(
  "H2_severity",
  LS24_cat ~ BMI_21_label + LS21_cat
)
nobs(H2_severity)
margPre_H2_severity <- run_margins(H2_severity, "BMI_21_label")

###restrictive----
H2_severity_res <- restrictive %>% run_polr(
  "H2_severity_res",
  LS24_cat ~ BMI_21_label + LS21_cat
)
margPre_H2_severity_res <- run_margins(H2_severity_res, "BMI_21_label")

plot_margins(
  margPre_H2_severity, "BMI_21_label",
  x_label = "BMI category (2021)",
  title = "Predicted life satisfaction category (2024) by BMI category"
)


plot_margins(
  margPre_H2_severity_res, "BMI_21_label",
  x_label = "BMI category (2021)",
  title = "Predicted life satisfaction category (2024) by BMI category, restrictive sample"
)


View(crude)

##persistence: no product term----
###crude----
H2_obePersist <- crude %>% run_polr(
  "H2_obePersist",
  LS24_cat ~ obePersist + LS21_cat
)
nobs(H2_obePersist)
margPre_H2_obePersist <- run_margins(H2_obePersist, "obePersist")

###restrictive----
H2_obPersist_res <- restrictive %>% run_polr(
  "H2_obePersist_res",
  LS24_cat ~ obePersist + LS21_cat
)
nobs(H2_obPersist_res)
margPre_H2_obePersist_res <- run_margins(H2_obPersist_res, "obePersist")

plot_margins(margPre_H2_obePersist, "obePersist",
             x_label = "Obesity persistence",
             title = "Predicted life satisfaction category (2024) by obesity persistence")

plot_margins(margPre_H2_obePersist_res, "obePersist",
             x_label = "Obesity persistence",
             title = "Predicted probability of life satisfaction (2024) by obesity persistence, restricted sample")


##weight change thoughts----
###!!!NOT effect modifier
H2_WCT <- crude %>% run_polr(
  "H2_WCT",
  LS24_cat ~ obe21_bin * WCT_21_bin + LS21_cat
)
nobs(H2_WCT)
margPre_H2_WCT <- run_margins(H2_WCT, "WCT_21_bin")

plot_margins(
  margPre_H2_WCT, "WCT_21_bin",
  x_label = "Thoughts on changing weight (2021)",
  title = "Predicted probability of life satisfaction (2024) by thoughts on changing weight"
)

# Is WCT associated with obesity status?
table(crude$WCT_21_bin, crude$obe21_bin, useNA = "always")
chisq.test(table(crude$WCT_21_bin, crude$obe21_bin))

# Is WCT associated with continuous BMI?
crude %>%
  group_by(WCT_21_bin) %>%
  summarise(
    mean_BMI = mean(BMI_21, na.rm = TRUE),
    sd_BMI   = sd(BMI_21, na.rm = TRUE),
    n        = n()
  )

##childhood weight perception ----
###!!!only heavier----
###crude----
H2_CWP <- crude %>% run_polr(
  "H2_CWP",
  LS24_cat ~ obe21_bin * CWP_21 + LS21_cat
)
nobs(H2_CWP)

margPre_H2_CWP <- run_margins(H2_CWP, "CWP_21")

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
  LS24_cat ~ AWP_21 * obe21_bin + LS21_cat
)
nobs(H2_AWP)

margPre_H2_AWP <- run_margins(H2_AWP, "AWP_21")

###restrictive----
H2_AWP_res <- restrictive %>% run_polr(
  "H2_AWP_res",
  LS24_cat ~ AWP_21 * obe21_bin + LS21_cat
)
nobs(H2_AWP_res)

margPre_H2_AWP_res <- run_margins(H2_AWP_res, "AWP_21")


##parental body size A-C :(( ----
# H2_mom <- crude %>% run_polr(
#   "H2_mom",
#   LS24_cat ~ momPhys_21_large * obe21_bin + LS21_cat
# )
# 
# H2_dad <- crude %>% run_polr(
#   "H2_mom",
#   LS24_cat ~ dadPhys_21_large * obe21_bin + LS21_cat
# )

###crude----
H2_parents <- crude %>% run_polr(
  "H2_parents",
  LS24_cat ~ parentPhys_cat * obe21_bin + LS21_cat
)
nobs(H2_parents)

margPre_H2_parents <- run_margins(H2_parents, "parentPhys_cat")

###restrictive----
H2_parents_res <- restrictive %>% run_polr(
  "H2_parents_res",
  LS24_cat ~ parentPhys_cat * obe21_bin + LS21_cat
)
nobs(H2_parents_res)

margPre_H2_parents_res <- run_margins(H2_parents_res, "parentPhys_cat")

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

###restrictive----
H2_typology_AWP_res <- restrictive %>% run_polr(
  "H2_typology_AWP_res",
  LS24_cat ~ typology_adult + LS21_cat
)
nobs(H2_typology_AWP_res)
margPre_H2_typology_AWP_res <- run_margins(H2_typology_AWP_res, "typology_adult")



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

##CWP typology ----
###crude----
H2_typology_CWP <- crude %>% run_polr(
  "H2_typology_CWP",
  LS24_cat ~ typology_child + LS21_cat
)
nobs(H2_typology_CWP)
margPre_H2_typology_CWP <- run_margins(H2_typology_CWP, "typology_child")


###restrictive----
H2_typology_CWP_res <- restrictive %>% run_polr(
  "H2_typology_CWP_res",
  LS24_cat ~ typology_child + LS21_cat
)
nobs(H2_typology_CWP_res)
margPre_H2_typology_CWP_res <- run_margins(H2_typology_CWP_res, "typology_child")


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




#TABLE 1, crude----
library(gtsummary)
crude %>%
  dplyr::select(BMI_21_label, age_2021_imputed, BMI_21, 
                obePersist, CWP_21, AWP_21, parentPhys_cat, diplUd_21, obeInh_24) %>%
  tbl_summary(
    by = BMI_21_label,
    missing = "always",
    missing_text = "Missing",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age_2021_imputed ~ "Age",
      BMI_21 ~ "BMI (kg/m²)",
      obePersist ~ "Obesity persistence between baseline and follow-up",
      CWP_21 ~ "Childhood weight perception",
      AWP_21 ~ "Adulthood weight perception",
      parentPhys_cat ~ "Parental body size",
      diplUd_21 ~ "Attainment of diplomuddannelse",
      obeInh_24 ~ "Family history of overweight (heredity)"
    ),
    percent = "column",
    type = list(diplUd_21 ~ "categorical",
                obeInh_24 ~ "categorical"),
    value = list(diplUd_21 ~ "yes")
  ) %>%
  add_overall() %>%
  modify_spanning_header(all_stat_cols() ~ "**BMI categories, crude sample**") %>%
  bold_labels()


library(flextable)
table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "table1.docx")


#TABLE 1, restrictive----
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


#citation, version, & session info----
# citation()
# version$version.string
# citation("")
# packageVersion("")
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

audit_WSd <- summarise_by_var(crude, "AWP_21")
print_audit(audit_WSd, "Adulthood weight perception - 2021 (AWP_21)")

#16. momPhys_21_large
audit_momPhys_21_large <- summarise_by_var(crude, "momPhys_21_large")
print_audit(audit_momPhys_21_large, "biological mother's body size at age 40 - 2021 (momPhys_21_large")

#17. dadPhys_21_large
audit_dadPhys_21_large <- summarise_by_var(crude, "dadPhys_21_large")
print_audit(audit_dadPhys_21_large, "biological father's body size at age 40 - 2021 (dadPhys_21_large")


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

#NA comparison plot -----
audit_list <- list(
  audit_diplUd, 
  audit_speUd, 
  audit_mastUd, 
  audit_kandiUd, 
  audit_PhD,
  audit_nightSche, 
  audit_eveSche, 
  audit_daySche,
  audit_lgbt, 
  audit_obeInh,
  audit_CWP, 
  audit_WCT,
  audit_WSb, 
  audit_WSc, 
  audit_WSd,
  audit_BMI_cat, 
  audit_obePersist,
  audit_LS21cat, 
  audit_LS24cat,
  audit_momPhys_21_large,
  audit_dadPhys_21_large
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

plot_na_comparison <- function(audit_list) {
  
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

plot_margins(
  margPre_typAdult_bin_crude, "typAdult_bin",
  x_label = "Adulthood weight perception-status typology",
  title = "Predicted probability of life satisfaction (2024) by adulthood weight perception-status typology"
)




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

H3_m2 <- crude %>% run_polr(
  "H3_m2",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin
)
nobs(H3_m2)


H3_m3 <- crude %>% run_polr(
  "H3_m3",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin + obeInh_24
)
nobs(H3_m3)

H3_m4 <- crude %>% run_polr(
  "H3_m4",
  LS24_cat ~ obe21_bin + LS21_cat + age_2021_imputed + diplUd_21_bin + obeInh_24 + parentPhys_cat
)
nobs(H3_m4)




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



#playground----
##weight perception during teen :((----
teenPerc <- crude %>% run_polr(
  "teenPerc",
  LS24_cat ~ obe21_bin * WS_b21 + LS21_cat
)



