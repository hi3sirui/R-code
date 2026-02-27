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

#HISTOGRAM: age distribution ----
library(ggplot2)
library(scales)
library(dplyr)
 #only using valid fields from age_2021_imputed; also no need to add "na.rm=TRUE" at every step.
age_valid <- v3 %>% filter(!is.na(age_2021_imputed))
binwidth <- 1
nrow(age_valid)

ggplot(age_valid) +
  #histogram (counts)
  geom_histogram(
    aes(x=age_2021_imputed), 
    binwidth = binwidth, fill="antiquewhite", color="white", alpha = 0.7, show.legend = FALSE) +
  #observed density curves for 2021 & 2024
  geom_density(aes(x = age_2021, y=after_stat(density)*n*binwidth, color="2021 observed"), linewidth = 1) +
  geom_density(aes(x = age_2024, y=after_stat(density)*n*binwidth, color = "2024 observed"), linewidth = 1) +
  scale_x_continuous(breaks = seq(0,100,by=10)) +
  scale_y_continuous(breaks = breaks_width(200)) + 
  scale_color_manual(
    name = "Age data source",
    values = c("2021 observed" = "grey",
               "2024 observed" = "lightblue")
  ) + 
  labs(
    title = "Age Distribution",
    x = "Age (years), with 2021 imputed",
    y = "Number of participants"
  ) +
  theme_minimal()

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
    weight_2021_valid = if_else(weight_2021>=48.5 & weight_2021<=86.5, weight_2021, NA_real_)
  )
#sanity check, n = 41031
##sum(is.na(v3$weight_2021_valid))
#sanity check, n = 1651
#sum(is.na(v3$weight_2021_valid)[v3$sex_valid=="Male"])

v3 <- v3 %>%
  mutate(
    weight_2024_valid = case_when(
      weight_2024>=48.5 & weight_2024<=99 ~ weight_2024,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )

v3 <- v3 %>%
  mutate(
    weight_2024_valid_female = case_when(
      weight_2024>=48.5 & weight_2024<=86.5 & sex_valid == "Female" ~ weight_2024,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )


v3 <- v3 %>%
  mutate(
    weight_2024_valid_male = case_when(
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
n21 <- length(na.omit(v3$weight_2021_valid)) #n = 29465
n24 <- length(na.omit(v3$weight_2024_valid_female))
n24Male <- length(na.omit(v3$weight_2024_valid_male))

###plot 1: 2021 valid weight (females only) ----
ggplot(v3, aes(x=weight_2021_valid)) +
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
ggplot(v3, aes(x=weight_2024_valid_female)) +
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
ggplot(v3, aes(x=weight_2024_valid_male)) +
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

weights_overview <- weights_overview %>%
  mutate(
    w21_plaus = case_when(
      weight_2021>=40 & weight_2021<=192 ~ weight_2021,
      TRUE ~ NA_real_
    )
  )

weights_overview  <- weights_overview %>%
  mutate(
    w24_plaus = if_else(weight_2024>=40 & weight_2024<=192, weight_2024, NA_real_)
  )

weights_overview <- weights_overview %>%
  mutate(
    w24_plaus_F = case_when(
      weight_2024>=40 & weight_2024<=192 & sex_valid == "Female" ~ weight_2024,
      TRUE ~ NA_real_
    )
  )

weights_overview <- weights_overview %>%
  mutate(
    w24_plaus_M = case_when(
      weight_2024>=40 & weight_2024<=192 & sex_valid == "Male" ~ weight_2024,
      TRUE ~ NA_real_
    )
  )

View(weights_overview)



###plot 4: 2021 plausible weight (females only) ----
library(ggplot2)
library(scales)
library(dplyr)

binwidth <- 2
n21_plaus <- length(na.omit(weights_overview$w21_plaus))
n24_plaus_F <- length(na.omit(weights_overview$w24_plaus_F))
n24_plaus_M <- length(na.omit(weights_overview$w24_plaus_M))

ggplot(weights_overview, aes(x=w21_plaus)) +
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
ggplot(weights_overview, aes(x=w24_plaus_F)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n24_plaus_F * 2, color = "2024 females plausible"), 
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
ggplot(weights_overview, aes(x=w24_plaus_M)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24_plaus_M * 2, color = "2024 male plausible"), 
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
###2021----
library(dplyr)
summary(v3$weight_2021_valid)
summary(v3$weight_2024_valid_female)
summary(v3$weight_2024_valid_male)
#sanity check
#sum(is.na(v3$weight_2024_valid_female)) #n = 48026

sv <- function(x) unclass(summary(x))  # strip "table" class/attributes
Weight_valid <- data.frame(
  All_2021    = sv(v3$weight_2021_valid),
  Female_2021 = sv(v3$weight_2021_valid[v3$sex_valid=="Female"]),
  Male_2021   = sv(v3$weight_2021_valid[v3$sex_valid == "Male"]),
  Female_2024 = sv(v3$weight_2024_valid_female),
  Male_2024   = sv(v3$weight_2024_valid_male),
  check.names = FALSE
)

library(knitr)
kable(Weight_valid, digits = 2, caption = "Summary statistics of valid weight by sex and year")

###2024----
sv <- function(x) unclass(summary(x)) 
weights_plaus <- data.frame(
  All_2021 = sv(weights_overview$w21_plaus),
  Female_2024 = sv(weights_overview$w24_plaus_F),
  Male_2024 = sv(weights_overview$w24_plaus_M),
  check.names = FALSE
)
library(knitr)
kable(weights_plaus, digits = 2, caption = "Summary statistics of weight by sex and year, plausibility approach")

###middle 95% - 2021----
quantile(v3$weight_2021, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$weight_2024_valid_female, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$weight_2024_valid_male, probs = c(0.025, 0.975), na.rm = TRUE)

###middle 95% - 2024----
quantile(weights_overview$w21_plaus, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(weights_overview$w24_plaus_F, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(weights_overview$w24_plaus_M, probs = c(0.025, 0.975), na.rm = TRUE)

###n (filled both waves) under different intervals----
#HES interval
sum(!is.na(v3$weight_2021_valid) & !is.na(v3$weight_2024_valid)) # n = 14747
sum(!is.na(v3$weight_2021_valid)&!is.na(v3$weight_2024_valid_female)) #n = 14263

#plausibility interval
sum(!is.na(weights_overview$w21_plaus) & !is.na(weights_overview$w24_plaus)) #n = 17419
sum(!is.na(weights_overview$w21_plaus) & !is.na(weights_overview$w24_plaus_F)) #n = 17419

## two-curve overlay on 2021 data----
###plot 7: 2021 HES and plausible intervals on raw 2021 weight data----
ggplot(weights_overview) +
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
    aes(x = v3$weight_2021_valid, y = after_stat(density) * n21 * binwidth, color = "HES interval (48.5-86.5)"), 
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
sum(!is.na(weights_overview$w21_plaus) & !is.na(weights_overview$w24_plaus)) #n = 17419

n24_plaus <- length(na.omit(weights_overview$w24_plaus))
n24_valid <- length(na.omit(v3$weight_2024_valid))

ggplot(weights_overview) +
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
    aes(x = v3$weight_2024_valid, y = after_stat(density) * n24_valid * binwidth, color = "HES interval (48.5-86.5)"), 
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

# We want to keep weights_heights_overview and add 'age' and 'region' from v3
weights_overview <- weights_overview %>%
  left_join(v3 %>% select(ipnr, weight_2021_valid, weight_2024_valid, weight_2024_valid_female, weight_2024_valid_male), by = "ipnr")

# 1. Create a temporary 'long' dataframe for the comparison
interval_comp <- weights_overview %>%
  select(weight_2021_valid, w21_plaus) %>%
  pivot_longer(cols = everything(), names_to = "Interval_Type", values_to = "Weight") %>%
  mutate(Interval_Type = case_when(
    Interval_Type == "weight_2021_valid" ~ "HES (Strict: 48.5-86.5)",
    Interval_Type == "w21_plaus"        ~ "Plausible (Wide: 40-192)"
  ))

# 2. Draw the comparison box plot
ggplot(interval_comp, aes(x = Interval_Type, y = Weight, fill = Interval_Type)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "black", outlier.shape = 1) +
  scale_fill_manual(values = c("HES (Strict: 48.5-86.5)" = "firebrick", 
                               "Plausible (Wide: 40-192)" = "dodgerblue")) +
  scale_y_continuous(breaks = seq(40, 200, by = 20)) +
  labs(
    title = "Comparison of Weight Distribution by Filtering Logic",
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

##filtering logic ----
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









# 
# #2021 HEIGHT DISTRIBUTION HISTOGRAM
# library(ggplot2)
# library(scales)
# library(dplyr)
# 
# binwidth <- 2
# n <- nrow(v3)
# 
# Height_2021 <- # 1. Recalculate n correctly
#   n_count <- nrow(v3)
# 
# ggplot(v3, aes(x = height_2021)) +
#   # Histogram
#   geom_histogram(
#     binwidth = 2, 
#     fill = "antiquewhite", 
#     color = "white", 
#     alpha = 0.7
#   ) +
#   # Density Curve - Simplified Scaling
#   geom_density(
#     aes(y = after_stat(density) * n_count * 2, color = "height 2021"), 
#     linewidth = 1, 
#     adjust=1.5,
#     na.rm = TRUE
#   ) +
#   # Manual Axis Limits (Adjust these to fit your data)
#   # Assuming height is between 140cm and 200cm
#   scale_x_continuous(limits = c(140, 200), breaks = seq(140, 200, 10)) +
#   
#   # REMOVED breaks_width(200) to stop the overlapping numbers
#   scale_y_continuous(labels = comma) + 
#   
#   scale_color_manual(
#     name = "Height Data Source",
#     values = c("height 2021" = "lightblue")
#   ) + 
#   labs(
#     title = "2021 Height Distribution",
#     x = "Height (cm)",
#     y = "Number of Participants"
#   ) +
#   theme_minimal()


