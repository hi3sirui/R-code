#READY TO USE V3 KEEP UPDATING----
library(dplyr)

v3 <- read.csv("L:/Auditdata/Students/Lexi/⭐⭐⭐Data_Lexi_v3⭐⭐⭐.csv")
View(v3)




#PREPARATION ----
##renaming columns ----
v3 <- v3 %>%
  rename(LS_2021 = quality_of_life_a_k,
         overall_health_2021 = health_k,
         height_2021 = height_k,
         weight_2021 = weight_k,
         waist_2021 = waist_k,
         weight_statement_a_2021 = weight_statements_a_k,
         weight_statement_b_2021 = weight_statements_b_k,
         weight_statement_c_2021 = weight_statements_c_k,
         weight_statement_d_2021 = weight_statements_d_k,
         weight_change_thoughts_2021 = weight_change_k,
         mom_physique_2021 = physique_mom_k,
         dad_physique_2021 = physique_dad_k,
         age_2024 = age,
         overall_health_2024 = srh,
         LS_2024 = qol,
         phy_health_2024 = phy_health_v2,
         men_health_2024 = men_health_v2,
         weight_2024 = weight_k_v2,
         height_2024 = height_k_v2,
         weight_statement_a_2024 = weight_statements_a_k_v2,
         weight_statement_b_2024 = weight_statements_b_k_v2,
         weight_statement_c_2024 = weight_statements_c_k_v2,
         weight_statement_d_2024 = weight_statements_a_k_v2,
         weight_change_thoughts_2024 = weight_change_k,
         mom_physique_2024 = physique_mom_k_v2,
         dad_physique_2024 = physique_dad_k_v2,
         age_2021 = cpr_alder
         )

##making cpr_alder to integer ----
v3 <- v3 %>%
  mutate(age_2021 = trunc(age_2021))

##making height to integer ----
v3 <- v3 %>%
  mutate(height_2021 = trunc(height_2021),
         height_2024 = trunc(height_2024))

##making weight to integer ----
v3 <- v3 %>%
  mutate(weight_2021 = trunc(weight_2021),
         weight_2024 = trunc(weight_2024))

##imputing age data ----
v3 <- v3 %>%
  mutate(
    age_2021_imputed = if_else(
      is.na(age_2021) & !is.na(age_2024),
      age_2024-3,
      age_2021
    )
  )

v3 <- v3 %>%
  mutate(
    age_2024_imputed = if_else(
      !is.na(age_2021) & is.na(age_2024),
      age_2021+3,
      age_2024
    )
  )

v3 <- v3 %>%
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

##removing participation histories----
library(tidyverse)
v3 <- v3 %>%
  select(-par_1993,-par_1999, -par_2009, -par_2020)


#sanity check: missing in 2021,  2024, OR both
# sum(is.na(v3$age_2024) | is.na(v3$cpr_alder)) == 
#   sum(!is.na(v3$age_flag)) + sum(is.na(v3$age_2024) & is.na(v3$cpr_alder)) 






##pooling sex from two years ----
v3 <- v3 %>%
  mutate(
    sex_valid = case_when(
      cpr_sex == 1 ~ "Female",
      cpr_sex == 2 ~ "Male",
      TRUE ~ NA_character_
    ),
    sex_valid = coalesce(sex_valid, sex)
  )

# #HISTOGRAM: age distribution ----
# library(ggplot2)
# library(scales)
# library(dplyr)
#  #only using valid fields from age_2021_imputed; also no need to add "na.rm=TRUE" at every step.
# age_valid <- v3 %>% filter(!is.na(age_2021_imputed))
# binwidth <- 1
# nrow(age_valid)
# 
# ggplot(age_valid) +
#   #histogram (counts)
#   geom_histogram(
#     aes(x=age_2021_imputed), 
#     binwidth = binwidth, fill="antiquewhite", color="white", alpha = 0.7, show.legend = FALSE) +
#   #observed density curves for 2021 & 2024
#   geom_density(aes(x = age_2021, y=after_stat(density)*n*binwidth, color="2021 observed"), linewidth = 1) +
#   geom_density(aes(x = age_2024, y=after_stat(density)*n*binwidth, color = "2024 observed"), linewidth = 1) +
#   scale_x_continuous(breaks = seq(0,100,by=10)) +
#   scale_y_continuous(breaks = breaks_width(200)) + 
#   scale_color_manual(
#     name = "Age data source",
#     values = c("2021 observed" = "grey",
#                "2024 observed" = "lightblue")
#   ) + 
#   labs(
#     title = "Age Distribution",
#     x = "Age (years), with 2021 imputed",
#     y = "Number of participants"
#   ) +
#   theme_minimal()

# #Warning message: Removed 24121 rows containing non-finite outside the scale range (`stat_bin()`). 
# #safety check
# nrow(valid_2021)
# #sanity check: exact count at age=60 if it's > 1000 ppl, as seen in Age Distribution graph
# sum(v3$age_2021_imputed == 60, na.rm = TRUE) 
# #sanity check: where the spikes are
# age_counts <- table(v3$age_2021_imputed)
# sort(age_counts, decreasing = TRUE) [1:10]







#WEIGHT DATA ----
##HES interval  ----
v3 <- v3 %>%
  mutate(
    w21_hes = if_else(weight_2021>=48.5 & weight_2021<=86.5, weight_2021, NA_real_)
  )
#sanity check, n = 41031
##sum(is.na(v3$weight_2021_valid))
#sanity check, n = 1651
#sum(is.na(v3$weight_2021_valid)[v3$sex_valid=="Male"])

v3 <- v3 %>%
  mutate(
    w24_hes = case_when(
      weight_2024>=48.5 & weight_2024<=99 ~ weight_2024,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )

v3 <- v3 %>%
  mutate(
    w24_hes_f = case_when(
      weight_2024>=48.5 & weight_2024<=86.5 & sex_valid == "Female" ~ weight_2024,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )


v3 <- v3 %>%
  mutate(
    w24_hes_m = case_when(
      weight_2024>=58.9 & weight_2024<=99 & sex_valid == "Male" ~ weight_2024,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )


#sanity check, n = 69720
#sum(is.na(v3$weight_2024_valid_male))
#sanity check, n = 48026
#sum(is.na(v3$weight_2024_valid_female))

#using weight_2024_valid if weight_2021 is empty


# #finding the numbers of outliers in 2021 based on the 2007/08 Danish Health Examination Survey
# outliers_2021_hes <- v3 %>% 
#   filter(weight_2021 <=48.5 | weight_2021 > 86.5) %>%
#   select(
#     ipnr,
#     age_2021,
#     cpr_sex,
#     height_2021,
#     weight_2021,
#     weight_2021_valid,
#     weight_2024
#   )
# View(outliers_2021_hes) #n = 5272
# write.csv(outliers_2021_hes, "outliers_2021_HES.csv")

#finding the numbers of outliers in 2024, dis-aggregated by sex, based on the 2007/08 Danish Health Examination Survey
#not sure if absolutely necessary

##HISTOGRAM: weight using HES interval ----
library(ggplot2)
library(scales)
library(dplyr)

binwidth <- 2
n21 <- length(na.omit(v3$w21_hes)) #n = 29465
n24 <- length(na.omit(v3$w24_hes_f))
n24Male <- length(na.omit(v3$w24_hes_m))

###plot 1: 2021 valid weight (females only) ----
ggplot(v3, aes(x=w21_hes)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n21 * 2, color = "2021 valid"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 95), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2021 weight Data Source", #title of the legend
    values = c("2021 valid" = "cyan4")
  ) + 
  labs(
    title = "2021 Weight distribution (females only)",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 2: 2024 valid weight females ----
ggplot(v3, aes(x=w24_hes_f)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n24 * 2, color = "2024 valid females"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 95), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 valid female",
    values = c("2024 valid females" = "cyan4")
  ) + 
  labs(
    title = "2024 Female weight distribution",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 3: 2024 male weight distribution----
ggplot(v3, aes(x=w24_hes_m)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24Male * 2, color = "2024 valid male"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(50, 110), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 valid male",
    values = c("2024 valid male" = "purple1")
  ) + 
  labs(
    title = "Male 2024 valid weight",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()


##plausibility approach----
###plaus weight interval----
weights_overview <- v3 %>%
  select(
    ipnr,
    age_2021,
    sex_valid,
    weight_2021,
    weight_2024
  )

v3 <- v3 %>%
  mutate(
    w21_plaus = case_when(
      weight_2021>=40 & weight_2021<=192 ~ weight_2021,
      TRUE ~ NA_real_
    )
  )

v3  <- v3 %>%
  mutate(
    w24_plaus = if_else(weight_2024>=40 & weight_2024<=192, weight_2024, NA_real_)
  )

v3 <- v3 %>%
  mutate(
    w24_plaus_f = case_when(
      weight_2024>=40 & weight_2024<=192 & sex_valid == "Female" ~ weight_2024,
      TRUE ~ NA_real_
    )
  )

v3 <- v3 %>%
  mutate(
    w24_plaus_m = case_when(
      weight_2024>=40 & weight_2024<=192 & sex_valid == "Male" ~ weight_2024,
      TRUE ~ NA_real_
    )
  )

View(v3)



###plot 4: 2021 plausible weight (females only) ----
library(ggplot2)
library(scales)
library(dplyr)

binwidth <- 2
n21_plaus <- length(na.omit(v3$w21_plaus))
n24_plaus_f <- length(na.omit(v3$w24_plaus_f))
n24_plaus_m <- length(na.omit(v3$w24_plaus_m))

ggplot(v3, aes(x=w21_plaus)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n21_plaus * 2, color = "2021 plausible"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(30, 220), breaks = seq(30, 220, by=20)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2021 weight Data Source", #title of the legend
    values = c("2021 plausible" = "cyan4")
  ) + 
  labs(
    title = "2021 Weight distribution - plausibility approach (females only)",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 5: 2024 valid weight females ----
ggplot(v3, aes(x=w24_plaus_f)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n24_plaus_f * 2, color = "2024 females plausible"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(30, 220), breaks = seq(30, 220, by=20)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 females plausible",
    values = c("2024 females plausible" = "cyan4")
  ) + 
  labs(
    title = "2024 Female weight distribution, plausibility approach",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 6: 2024 male weight distribution----
ggplot(v3, aes(x=w24_plaus_m)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24_plaus_m * 2, color = "2024 male plausible"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(30, 220), breaks = seq(50, 220, by=20)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 male plausible",
    values = c("2024 male plausible" = "purple1")
  ) + 
  labs(
    title = "Male 2024 weight, plausibility approach",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

##summary statistics ----
###HES interval----
library(dplyr)
summary(v3$w21_hes)
summary(v3$w24_hes_f)
summary(v3$w24_hes_m)
summary(v3$weight_2021)
w21_hes_sd <- sd(v3$w21_hes, na.rm = TRUE)
w24_hes_sd <- sd(v3$w24_hes, na.rm = TRUE)
w21_plaus_sd <- sd(v3$w21_plaus, na.rm = TRUE)
w24_plaus_sd <- sd(v3$w24_plaus, na.rm = TRUE)

w21_hes_sd
w24_hes_sd
w21_plaus_sd
w24_plaus_sd

#sanity check
#sum(is.na(v3$weight_2024_valid_female)) #n = 48026

sv <- function(x) unclass(summary(x))  # strip "table" class/attributes
w_hes <- data.frame(
  All_2021    = sv(v3$w21_hes),
  Female_2021 = sv(v3$w21_hes[v3$sex_valid=="Female"]),
  Male_2021   = sv(v3$w21_hes[v3$sex_valid == "Male"]),
  All_2024 = sv(v3$w24_hes),
  Female_2024 = sv(v3$w24_hes_f),
  Male_2024   = sv(v3$w24_hes_m),
  check.names = FALSE
)

library(knitr)
kable(w_hes, digits = 2, caption = "Summary statistics of valid weight by sex and year")

###plausibility interval----
sv <- function(x) unclass(summary(x)) 
w_plaus <- data.frame(
  All_2021 = sv(v3$w21_plaus),
  All_2024 = sv(v3$w24_plaus),
  Female_2024 = sv(v3$w24_plaus_f),
  Male_2024 = sv(v3$w24_plaus_m),
  check.names = FALSE
)
library(knitr)
kable(w_plaus, digits = 2, caption = "Summary statistics of weight by sex and year, plausibility approach")

###middle 95% - HES----
quantile(v3$weight_2021, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w21_hes, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_hes, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_hes_f, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_hes_m, probs = c(0.025, 0.975), na.rm = TRUE)

###middle 95% - plausibility----
quantile(v3$w21_plaus, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$weight_2024, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus_f, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus_m, probs = c(0.025, 0.975), na.rm = TRUE)

###n (filled both waves) under different intervals----
#HES interval
sum(!is.na(v3$w21_hes) & !is.na(v3$w24_hes)) # n = 14747
sum(!is.na(v3$w21_hes)&!is.na(v3$w24_hes_f)) #n = 14263

#plausibility interval
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus)) #n = 17419
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus_f)) #n = 17419

## two-curve overlay on 2021 data----
###plot 7: 2021 HES and plausible intervals on raw 2021 weight data----
ggplot(v3) +
  # THE BACKGROUND: Raw 2021 data with no modifiers
  geom_histogram(
    aes(x = weight_2021), 
    binwidth = 2, 
    fill = "grey90",    # Neutral color so the curves pop
    color = "white", 
    alpha = 0.8
  ) +
  
  # CURVE 1: HES Survey Interval (Strict)
  geom_density(
    aes(x = v3$w21_hes, y = after_stat(density) * n21 * binwidth, color = "HES interval (48.5-86.5)"), 
    linewidth = 1, adjust = 1.5, na.rm = TRUE
  ) +
  
  # CURVE 2: Plausibility Interval (Widened)
  geom_density(
    aes(x = w21_plaus, y = after_stat(density) * n21_plaus * binwidth, color = "Plausible (40-192)"), 
    linewidth = 1, adjust = 1.5, na.rm = TRUE
  ) +
  
  # SCALE & THEME
  scale_color_manual(values = c("HES interval (48.5-86.5)" = "firebrick", "Plausible (40-192)" = "dodgerblue")) +
  scale_x_continuous(limits = c(30, 200), breaks = seq(40, 200, 20)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Impact of Filtering Logic on 2021 Weight Data",
    subtitle = "Grey bars = Raw Data",
    x = "Weight (kg)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

###plot 8: 2024 HES and plausible intervals on raw 2021 weight data----
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus)) #n = 17419

n24_plaus <- length(na.omit(v3$w24_plaus))
n24_valid <- length(na.omit(v3$w24_hes))

ggplot(v3) +
  # THE BACKGROUND: Raw 2024 data with no modifiers
  geom_histogram(
    aes(x = weight_2024), 
    binwidth = 2, 
    fill = "grey90",    # Neutral color so the curves pop
    color = "white", 
    alpha = 0.8
  ) +
  
  # CURVE 1: HES Survey Interval (Strict)
  geom_density(
    aes(x = w24_hes, y = after_stat(density) * n24_valid * binwidth, color = "HES interval (48.5-86.5)"), 
    linewidth = 1, adjust = 1.5, na.rm = TRUE
  ) +
  
  # CURVE 2: Plausibility Interval (Widened)
  geom_density(
    aes(x = w24_plaus, y = after_stat(density) * n24_plaus * binwidth, color = "Plausible (40-192)"), 
    linewidth = 1, adjust = 1.5, na.rm = TRUE
  ) +
  
  # SCALE & THEME
  scale_color_manual(values = c("HES interval (48.5-86.5)" = "firebrick", "Plausible (40-192)" = "dodgerblue")) +
  scale_x_continuous(limits = c(30, 200), breaks = seq(40, 200, 20)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Impact of Filtering Logic on 2024 Weight Data",
    subtitle = "Grey bars = Raw Data",
    x = "Weight (kg)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

###plot 9: boxplot for filtering logic----
library(ggplot2)
library(tidyr)
library(dplyr)


# 1. Create a temporary 'long' dataframe for the comparison
interval_comp <- v3 %>%
  select(w21_hes, w21_plaus) %>%
  pivot_longer(cols = everything(), names_to = "Interval_Type", values_to = "Weight") %>%
  mutate(Interval_Type = case_when(
    Interval_Type == "w21_hes" ~ "HES (Strict: 48.5-86.5)",
    Interval_Type == "w21_plaus" ~ "Plausible (Wide: 40-192)"
  ))

# 2. Draw the comparison box plot
ggplot(interval_comp, aes(x = Interval_Type, y = Weight, fill = Interval_Type)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "black", outlier.shape = 1) +
  scale_fill_manual(values = c("HES (Strict: 48.5-86.5)" = "firebrick", 
                               "Plausible (Wide: 40-192)" = "dodgerblue")) +
  scale_y_continuous(breaks = seq(40, 200, by = 20)) +
  labs(
    title = "Comparison of Weight Distribution by Filtering Logic, using 2021 weight data",
    subtitle = "Comparing HES Survey constraints vs. Biological Plausibility",
    x = "Filtering Interval",
    y = "Weight (kg)"
  ) +
  stat_summary(
    geom = "text",
    fun = function(y) quantile(y, probs=c(0.25, 0.5, 0.75)),
    aes(label = round(after_stat(y),1)),
    nudge_x = 0.2, #nudge_x moves the text to the right of the box
    hjust = 0, #Left-aligns text at the nudge point
    size = 3.5,
    fontface = "bold"
  ) + 
  theme_minimal() +
  theme(legend.position = "none")

















#HEIGHT DATA----
#cleaning logic see *working note* google doc

##transformation logic ----
valid_height_logic <- function(h) {
  if (!is.na(h) && h >= 200 && h < 1000) {
    h <- (h %% 100) + 100
  }
  
  case_when(
    #7-digit outliers: Divide by 10000
    h >= 1000000 ~ h / 10000,
    
    #6-digit data: Divide by 1000
    h >= 100000 ~ h / 1000,
    
    #5-digit data: divide by 100
    h >= 10000 ~ h / 100,
    
    #4-digit: divide by 10
    h >= 1000 ~ h / 10,
    
    #Theory A: missing 0
    h >= 1 & h <=18 ~ h*10,
    
    #Theory B: dropped hundred
    h > 18 & h <100  ~ h+100,
    
    #within reason
    h >=140  & h <=200  ~ h,
    
    #suspected error
    h >=100  & h <140  ~ h,
    
    #error
    h==0 ~ h,
    
    TRUE ~ NA_real_
  )
}

##valid 21 funnel ----
v3 <- v3 %>%
  mutate(
    h21_valid = sapply(height_2021, valid_height_logic),

    h21_flag = case_when(
      height_2021 >= 1000000 ~ "7-digit",
      height_2021 >= 100000 ~ "6-digit",
      height_2021 >= 10000 ~ "5-digit",
      height_2021 >= 1000 ~ "4-digit",
      
      height_2021 > 200 & height_2021 < 1000 ~ "suspected error",
      height_2021 >=140 & height_2021 <=200 ~ "within reason",
      height_2021 >=100 & height_2021 < 140 ~ "suspected error",
      height_2021 >10 & height_2021 <= 18 ~ "Theory A",
      height_2021 >=1 & height_2021 <=10 ~ "suspected error",
      height_2021 > 18 & height_2021 < 100 ~ "Theory B",
      height_2021 == 0 ~ "x=0",
      TRUE ~"NA"
    ),
    h21_valid = round(h21_valid)
  )
View(v3)

heights_overview <- v3 %>%
  select(ipnr, height_2021, height_2024, h21_valid, h21_flag)
View(heights_overview)

h21_valid_check <- heights_overview %>%
  filter(h21_flag %in% c("7-digit", "6-digit", "5-digit", "4-digit", 
                         "Theory A", 
                         "Theory B", 
                         "suspected error", 
                         "x=0")) %>%
  
  # 2. Select the columns for your overview
  select(height_2021, height_2024, h21_valid, h21_flag) %>%
  
  # 3. Add a difference check to see how far off the raw data is from 2024
  mutate(gap_to_h24 = abs(h21_valid - height_2024)) %>%
  
  # 4. Sort by flag so you can review them in groups
  arrange(h21_flag)
View(h21_valid_check)


h21_valid_check %>% 
  count(h21_flag)

sum(!is.na(v3$height_2021))
sum(v3$height_2021>=140 & v3$height_2021<=200, na.rm=TRUE)

#sanity check
#sum(v3$h21_flag != "within reason" & v3$h21_flag != "NA", na.rm = TRUE)

v3 <- v3 %>%
  mutate(
    h_impt = case_when(
      # If "within reason", keep the math-cleaned 2021 height
      h21_flag == "within reason" ~ h21_valid,
      
      # If NOT "within reason", substitute ONLY if 2024 is valid and in range
      h21_flag != "within reason" & 
        !is.na(height_2024) & 
        height_2024 >= 140 & height_2024 <= 200 ~height_2024,
      
      # Otherwise, set to NA
      TRUE ~ NA_real_
    )
  )


v3 <- v3 %>%
  mutate(
    h_impt_flag = case_when(
        h21_flag == "within reason" ~ "2021 Original",
        h21_flag != "within reason" & !is.na(height_2024) ~ "Substituted from 2024",
        TRUE ~ "NA"
      )
    )

heights_overview <- v3 %>%
  select(
    ipnr,
    height_2021,
    height_2024,
    h21_valid,
    h21_flag,
    h_impt,
    h_impt_flag
  )
View(heights_overview)


##plot 10: height_2021 distribution with two density curves overlay----
binwidth_h <- 2
h21 <- sum(v3$h21_flag == "within reason", na.rm = TRUE)
h_final <- sum(!is.na(v3$h_impt))

ggplot(v3) +
  geom_histogram(
    aes(x = height_2021), 
    binwidth = binwidth_h, 
    fill = "antiquewhite", color = "white", alpha = 0.8
  ) +
  
  # BLUE CURVE: treated data
  geom_density(
    data = subset(v3, h21_flag == "within reason"),
    aes(x = h21_valid, y = after_stat(density) * h21 * binwidth_h, color = "treated 2021 height"), 
    linewidth = 1.2, linetype = "dashed", adjust = 1.5, na.rm = TRUE
  ) +

  # RED CURVE:Final data with substitutions
  geom_density(
    aes(x = h_impt, y = after_stat(density) * h_final * binwidth_h, color = "treated 2021 height with 2024 substitution"), 
    linewidth = 0.8, linetype = "solid", adjust = 1.5, na.rm = TRUE
  ) +
  
  scale_color_manual(values = c(
    "treated 2021 height" = "slategray3", 
    "treated 2021 height with 2024 substitution" = "steelblue1"
  )) +
  scale_x_continuous(limits = c(100, 220), breaks = seq(100, 220, 10)) + 
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Impact of 2024 Substitution on 2021 Height Distribution",
    subtitle = paste0("Total Cleaned: ", scales::comma(h_final), " (Rescued 170 outliers)"),
    x = "Height (cm)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

##plot 11: boxplot visualizing outliers and treated 2021----
summary(v3$h21_valid)
summary(v3$h_impt)

library(dplyr)
library(tidyr)

# Transform wide data to long format
v3_long <- v3 %>%
  # Select only the columns we want to compare
  select(height_2021, h21_valid, h_impt) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Source", 
    values_to = "Height"
  ) %>%
  # Rename the values in 'Source' to match your color vector labels
  mutate(Source = case_when(
    Source == "height_2021" ~ "Raw 2021",
    Source == "h21_valid" ~ "Treated 2021",
    Source == "h_impt"      ~ "Treated 2021 with substitutions from 2024",
    TRUE                    ~ Source
  ))


vision_friendly_colors <- c(
  "Raw 2021" = "#999999",     # Neutral Gray
  "Treated 2021" = "#E69F00",     # Vivid Orange
  "Treated 2021 with substitutions" = "#56B4E9"  # Sky Blue
)

# 2. Generate the Boxplot
ggplot(v3_long, aes(x = Source, y = Height, fill = Source)) +
  # Boxplot with outlines that contrast against the fills
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1, color = "black") +
  
  # SCALE: Keeping the Log10 scale for the 1,000,000 outliers
  scale_y_log10(
    breaks = c(1, 10, 140, 168, 200, 1000, 10000, 100000, 1000000),
    labels = scales::comma
  ) +
  
  # Apply the vision-friendly manual scale
  scale_fill_manual(values = vision_friendly_colors) +
  
  # THEME & LABELS
  labs(
    title = "Comparison of Raw Heights vs. Treated Distribution",
    x = "Data Source",
    y = "Height (cm, Log Scale)"
  ) +
  theme_minimal() +
  # Add white grid lines for better contrast on the log scale
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

##summary statistics----
quantile(v3$h21_valid, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(v3$h_impt, probs = c(0.025, 0.975), na.rm = TRUE)
sum(!is.na(v3$h_impt))
v3 %>%
  count(h_impt_flag) #substituted from 2024 = 10550













#SMD ANALYSIS----
v3_long <- v3 %>%
  select(
    w21_hes,
    w21_plaus,
    w24_hes,
    w24_plaus,
    h21_valid,
    h_impt,
    weight_2021,
    weight_2024,
    height_2021
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Source",
    values_to = "Value"
  ) %>%
  mutate(
    Measure = if_else(grepl("weight|w21|w24", Source), "Weight", "Height"),
    Status = case_when(
      # 1. RAW DATA: Any original column
      grepl("weight_2021|weight_2024|height_2021", Source) ~ "Original",
      # 2. HES logic for weight
      grepl("hes", Source) ~ "HES",
      
      # 3. HEIGHT SPECIFIC: The version with the 10,550 substitutions
      Source == "h_impt" ~ "Substituted",

      # 4. WEIGHT SPECIFIC: The weight version using plausibility intervals
      grepl("plaus|valid", Source) ~ "Plausibility",
      
      # Fallback just in case
      TRUE ~ "Other"
  )
)
View(v3_long)

library(dplyr)
library(tidyr)
library(smd)

w21 <- v3_long %>% filter(Source %in% c("w21_hes", "w21_plaus"))
w24 <- v3_long %>% filter(Source %in% c("w24_hes", "w24_plaus"))
h <- v3_long %>% filter(Source %in% c("h21_valid", "h_impt"))

##between treated groups ----
SMD_w21 <- smd(w21$Value, w21$Status, na.rm = TRUE)
SMD_w24 <- smd(w24$Value, w24$Status, na.rm = TRUE)
SMD_h <- smd(h$Value, h$Status, na.rm = TRUE)


##original VS treated----
###w21 Original vs HES----
w21_O_v_hes <- v3_long %>% filter(Source %in% c("weight_2021", "w21_hes"))
SMD_w21_OvH <- smd(w21_O_v_hes$Value, w21_O_v_hes$Status, na.rm = TRUE)

###w21 original vs Plausibility----
w21_O_v_plaus <- v3_long %>% filter(Source %in% c("weight_2021", "w21_plaus"))
SMD_w21_OvP <- smd(w21_O_v_plaus$Value, w21_O_v_plaus$Status, na.rm = TRUE)

### w24 original vs HES----
w24_O_v_hes <- v3_long %>% filter(Source %in% c("weight_2024", "w24_hes"))
SMD_w24_OvH <- smd(w24_O_v_hes$Value, w24_O_v_hes$Status, na.rm = TRUE)

### w24 original vs Plausibility----
w24_O_v_plaus <- v3_long %>% filter(Source %in% c("weight_2024", "w24_plaus"))
SMD_w24_OvP <- smd(w24_O_v_plaus$Value, w24_O_v_plaus$Status, na.rm = TRUE)

### h21 original vs treated (BIV) ----
h_O_v_treated <- v3_long %>% filter(Source %in% c("height_2021", "h21_valid"))
SMD_h_OvT <- smd(h_O_v_treated$Value, h_O_v_treated$Status, na.rm = TRUE)

### h21 original vs treated + substitution (impt) ----
h_O_v_impt <- v3_long %>% filter(Source %in% c("height_2021", "h_impt"))
SMD_h_OvI <- smd(h_O_v_impt$Value, h_O_v_impt$Status, na.rm = TRUE)


## Define the pairs for comparison----
comparisons <- list(
  w21_O_v_hes    = c("weight_2021", "w21_hes"),
  w21_O_v_plaus  = c("weight_2021", "w21_plaus"),
  w24_O_v_hes    = c("weight_2024", "w24_hes"),
  w24_O_v_plaus  = c("weight_2024", "w24_plaus"),
  h_O_v_treated = c("height_2021", "h21_valid"),
  h_O_v_impt   = c("height_2021", "h_impt")
)

## SMD for each pair----
smd_results <- lapply(comparisons, function(pair) {
  df_sub <- v3_long %>% filter(Source %in% pair)
  result <- smd(df_sub$Value, df_sub$Status, na.rm = TRUE)
  return(result$estimate)
})
unlist(smd_results)






















#BMI----
##w21_plaus & h_impt----
#I) 30-34.99kg/m2, II) 35-39.99kg/m2, and III) > 40kg/m2
v3 <- v3 %>%
  mutate(
    BMI_21 = w21_plaus / ((h_impt / 100)^2),
    BMI_24 = w24_plaus / ((h_impt / 100)^2)
  )

v3 <- v3 %>%
  mutate(across(c(BMI_21, BMI_24), 
                ~case_when(
                  . < 18.5 ~ "Underweight",
                  . >= 18.5 & . < 25 ~ "Healthy",
                  . >= 25 & . < 30 ~ "Overweight",
                  . >= 30 & . <35 ~ "Obese I",
                  . >=35 & . < 40 ~ "Obese II",
                  . >=40 ~ "Obese III"
                ), 
                .names = "{.col}_label"))

summary(v3$BMI_24)

##plot 12: scatter plot----
library(ggplot2)

ggplot(v3, aes(x = BMI_21, y = BMI_24)) +
  # Use very low alpha (0.1) due to the large N = 44,834
  geom_point(alpha = 0.1, color = "midnightblue") +
  
  # Add the 'No Change' reference line
  geom_abline(intercept = 0, slope = 1, color = "orange", linetype = "dashed", size = 1) +
  
  # Focus on the realistic BMI range (15 to 50)
  coord_cartesian(xlim = c(15, 50), ylim = c(15, 50)) +
  
  labs(
    title = "BMI Trajectory: 2021 and 2024",
    subtitle = "Points above the red line increased BMI; points below decreased.",
    x = "BMI in 2021 (Rescued/Substituted)",
    y = "BMI in 2024 (Rescued/Substituted)"
  ) +
  theme_minimal()

library(ggplot2)

###BMI density trajectory
# ggplot(v3, aes(x = BMI_21, y = BMI_24)) +
#   # Hexbins show where the most people 'clump'
#   geom_hex(bins = 60) + 
#   
#   # The 45-degree line (using the updated 'linewidth')
#   geom_abline(intercept = 0, slope = 1, color = "white", linetype = "dashed", linewidth = 0.8) +
#   
#   # Better color scale to see the density
#   scale_fill_viridis_c(option = "magma") +
#   
#   # Expand limits slightly to see the rescued outliers
#   coord_cartesian(xlim = c(15, 60), ylim = c(15, 60)) +
#   
#   labs(
#     title = "BMI Density Trajectory (2021 vs 2024)",
#     subtitle = "Brightest areas show where most participants land",
#     x = "BMI 2021 (Rescued/Substituted)",
#     y = "BMI 2024 (Rescued/Substituted)",
#     fill = "Count"
#   ) +
#   theme_minimal()

library(ggplot2)
library(tidyr)

##plot 13: BMI 21 & 24 density overlay----
# Reshape just the BMI columns for easy plotting
bmi_overlay_data <- v3 %>%
  select(BMI_21, BMI_24) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "BMI") %>%
  mutate(Year = if_else(Year == "BMI_21", "2021", "2024")) %>%
  filter(!is.na(BMI))

# Create the overlay plot
ggplot(bmi_overlay_data, aes(x = BMI, fill = Year)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("2021" = "steelblue1", "2024" = "grey2")) +
  coord_cartesian(xlim = c(15, 70)) + # Focused range for clinical relevance
  labs(
    title = "Population BMI Shift: 2021 vs 2024",
    x = "Body Mass Index (BMI)",
    y = "Density"
  ) +
  theme_minimal()

##summary statistics----
quantile(v3$BMI_21, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(v3$BMI_24, probs = c(0.025, 0.975), na.rm = TRUE)
sd(v3$BMI_21, na.rm = TRUE)
sd(v3$BMI_24, na.rm = TRUE)















#LS & BMI----














