#READY TO USE V3 KEEP UPDATING----
library(dplyr)

v3 <- read.csv("/Users/siruizhang/Thesis/v3.csv")
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
# A reusable "Rescue Function" based on your logic
rescue_weight <- function(w_input, w_anchor) {
  case_when(
    # Baseline
    w_input >= 40 & w_input < 150 ~ w_input,
    
    # 5-digit: 98190 -> 98
    nchar(as.character(floor(w_input))) == 5 ~ w_input %/% 1000,
    
    # 4-digit: 7890 -> check front (78) vs back (90) against 2024 anchor
    nchar(as.character(floor(w_input))) == 4 ~ {
      front <- w_input %/% 100
      back  <- w_input %% 100
      if_else(abs(front - w_anchor) < abs(back - w_anchor), front, back)
    },
    
    # 3-digit > 180: check first two (954 -> 95)
    w_input > 180 & w_input <= 1000 & 
      abs((w_input %/% 10) - w_anchor) <= 20 ~ w_input %/% 10,
    
    # 3-digit 150-180: check last two (178 -> 78)
    w_input >= 150 & w_input <= 180 & 
      abs((w_input %% 100) - w_anchor) <= 20 ~ w_input %% 100,
    
    TRUE ~ w_input
  )
}

# Apply to BOTH variables in v3
v3 <- v3 %>%
  mutate(
    w21_treated = rescue_weight(weight_2021, weight_2024),
    w21_plaus = if_else(w21_treated >= 40 & w21_treated <=150, w21_treated, NA_real_),
    w21_hes   = if_else(w21_treated >= 48.5 & w21_treated <= 86.5, w21_treated, NA_real_)
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
n21 <- length(na.omit(v3$w21_hes))
n24Female <- length(na.omit(v3$w24_hes_f))
n24Male <- length(na.omit(v3$w24_hes_m))
n24 <- length(na.omit(v3$w24_hes))

###plot 1: 2021 weight (females only) ----
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
    aes(y = after_stat(density) * n21 * 2, color = "2021 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 95), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2021 weight Data Source", #title of the legend
    values = c("2021 weight" = "cyan4")
  ) + 
  labs(
    title = "2021 weight, HES interval, females only",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 2: 2024 females ----
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
    aes(y = after_stat(density) * n24Female * 2, color = "2024 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 95), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 weight, female",
    values = c("2024 weight" = "cyan4")
  ) + 
  labs(
    title = "2024 weight, HES interval, female",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 3: 2024 male----
ggplot(v3, aes(x=w24_hes_m)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24Male * 2, color = "2024 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(50, 110), breaks = seq(40, 110, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 valid male",
    values = c("2024 weight" = "purple1")
  ) + 
  labs(
    title = "2024 weight, HES interval, male",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 4: 2024 total
ggplot(v3, aes(x=w24_hes)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  # Density Curve - Simplified Scaling
  geom_density(
    aes(y = after_stat(density) * n24 * 2, color = "2024 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 110), breaks = seq(40, 110, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 weight",
    values = c("2024 weight" = "cyan4")
  ) + 
  labs(
    title = "2024 weight, HES interval, full cohort",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 4: 2024 male----
ggplot(v3, aes(x=w24_hes)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24Male * 2, color = "2024 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(50, 110), breaks = seq(40, 90, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 valid male",
    values = c("2024 weight" = "purple1")
  ) + 
  labs(
    title = "2024 weight, HES interval, male",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()


##plausibility approach----
###plaus weight interval----
weights_overview <- v3 %>%
  select(
    #ipnr,
    age_2021,
    sex_valid,
    weight_2021,
    weight_2024,
    w21_plaus,
    w24_plaus
  )
View(weights_overview)

v3 <- v3 %>%
  mutate(
    w21_plaus = case_when(
      weight_2021>=40 & weight_2021<=192 ~ weight_2021,
      
      # 2. FIVE-DIGIT: Take first two (98180 -> 98)
      nchar(as.character(floor(weight_2021))) == 5 ~ weight_2021 %/% 1000,
      
      # 3. FOUR-DIGIT: (e.g., 7890 -> 78 or 90). 
      # Picks the two digits closest to the 2024 anchor.
      nchar(as.character(floor(weight_2021))) == 4 ~ {
        front <- weight_2021 %/% 100
        back  <- weight_2021 %% 100
        if_else(abs(front - weight_2024) < abs(back - weight_2024), front, back)
      },
      
      # 4. THREE-DIGIT (181-1000): (e.g., 954 -> 95). 
      # Use first two digits if they match 2024 within +/- 20kg.
      weight_2021 > 180 & weight_2021 <= 1000 & 
        abs((weight_2021 %/% 10) - weight_2024) <= 20 ~ weight_2021 %/% 10,
      
      # 5. THREE-DIGIT (150-180): (e.g., 178 -> 78). 
      # Use LAST two digits if they match 2024 within +/- 20kg.
      weight_2021 >= 150 & weight_2021 <= 180 & 
        abs((weight_2021 %% 100) - weight_2024) <= 20 ~ weight_2021 %% 100,
      
      # 6. DEFAULT: If it doesn't fit a pattern, keep original (will likely be filtered later)
      TRUE ~ weight_2021
    ))

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



###plot 5: 2021 plausible weight ----
library(ggplot2)
library(scales)
library(dplyr)

binwidth <- 2
n21_plaus <- length(na.omit(v3$w21_plaus))
n24_plaus <- length(na.omit(v3$w24_plaus))
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
    title = "2021 Weight plausibility approach, females only",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

###plot 6: 2024 females ----
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

###plot 7: 2024 male----
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
  scale_x_continuous(limits = c(30, 220), breaks = seq(30, 220, by=20)) +
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

###plot 8: 2024, total
ggplot(v3, aes(x=w24_plaus)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n24_plaus * 2, color = "2024 plausible"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(30, 220), breaks = seq(30, 220, by=20)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 male plausible",
    values = c("2024 plausible" = "purple1")
  ) + 
  labs(
    title = "2024 Weight plausibility approach, full cohort",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()

##summary statistics ----
v3 %>% filter(weight_2021 != w21_plaus) %>% nrow()

stmd_h <- function(data, variable, group_var) {
  # Split data by group
  g1 <- data[[variable]][data[[group_var]] == unique(data[[group_var]])[1]]
  g2 <- data[[variable]][data[[group_var]] == unique(data[[group_var]])[2]]
  
  # Calculate means and sds
  m1 <- mean(g1, na.rm = TRUE)
  m2 <- mean(g2, na.rm = TRUE)
  s1 <- sd(g1, na.rm = TRUE)
  s2 <- sd(g2, na.rm = TRUE)
  n1 <- length(na.omit(g1))
  n2 <- length(na.omit(g2))
  
  # Pooled SD calculation
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  
  # Return the SMD
  return((m1 - m2) / pooled_sd)
}

###SMD, HES as baseline----
# Create the comparison frame
df_weight_compare <- data.frame(
  weight = c(v3$w21_hes, v3$w21_plaus),
  group  = c(rep("HES", nrow(v3)), rep("Plausibility", nrow(v3)))
) %>% filter(!is.na(weight))

# Run the calculation
weight_smd_result <- stmd_h(df_weight_compare, "weight", "group")
print(paste("New Weight SMD:", round(weight_smd_result, 4)))

###HES interval----
library(dplyr)
summary(v3$w21_hes)
summary(v3$w24_hes)
summary(v3$w24_hes_f)
summary(v3$w24_hes_m)
summary(v3$weight_2021)
w21_hes_sd <- sd(v3$w21_hes, na.rm = TRUE)
w24_hes_sd <- sd(v3$w24_hes, na.rm = TRUE)
w21_plaus_sd <- sd(v3$w21_plaus, na.rm = TRUE)
w24_plaus_sd <- sd(v3$w24_plaus, na.rm = TRUE)

#sanity check
#sum(is.na(v3$weight_2024_valid_female)) #n = 48026

sv <- function(x) unclass(summary(x))  # strip "table" class/attributes
w_hes <- data.frame(
  All_2021    = sv(v3$w21_hes),
  All_2024 = sv(v3$w24_hes),
  Female_2024 = sv(v3$w24_hes_f),
  Male_2024   = sv(v3$w24_hes_m),
  check.names = FALSE
)

library(knitr)
kable(w_hes, digits = 2, caption = "Summary statistics of valid weight by sex and year, HES interval")

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

quantile(v3$w24_hes, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_hes_f, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_hes_m, probs = c(0.025, 0.975), na.rm = TRUE)

###middle 95% - plausibility----
quantile(v3$w21_plaus, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$weight_2024, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus_f, probs = c(0.025, 0.975), na.rm = TRUE)

quantile(v3$w24_plaus_m, probs = c(0.025, 0.975), na.rm = TRUE)

###under different intervals----
#HES interval
sum(!is.na(v3$w21_hes) & !is.na(v3$w24_hes)) # n = 14759
sum(!is.na(v3$w21_hes)&!is.na(v3$w24_hes_f)) #n = 14275

#plausibility interval
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus)) #n = 17420
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus_f)) #n = 17420

## two-curve overlay on 2021 data----
###plot 8: 2021 HES and plausible intervals on raw 2021 weight data----
ggplot(v3) +
  # THE BACKGROUND: Raw 2021 data with no modifiers
  geom_histogram(
    aes(x = weight_2021), 
    binwidth = 2, 
    fill = "antiquewhite",    # Neutral color so the curves pop
    color = "white", 
    alpha = 0.8
  ) +
  
  # CURVE 1: HES Survey Interval (Strict)
  geom_density(
    aes(x = w21_hes, y = after_stat(density) * n21 * binwidth, color = "HES interval (48.5-86.5)"), 
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
    x = "Weight (kg)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

###plot 9: 2024 HES and plausible intervals on raw 2024 weight data----
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus)) #n = 17420

ggplot(v3) +
  # THE BACKGROUND: Raw 2024 data with no modifiers
  geom_histogram(
    aes(x = weight_2024), 
    binwidth = 2, 
    fill = "antiquewhite",    # Neutral color so the curves pop
    color = "white", 
    alpha = 0.8
  ) +
  
  # CURVE 1: HES Survey Interval (Strict)
  geom_density(
    aes(x = w24_hes, y = after_stat(density) * n24 * binwidth, color = "HES interval (48.5-86.5)"), 
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
    x = "Weight (kg)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

###plot 10: boxplot for filtering logic----
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
    h >=140  & h <=210  ~ h,
    
    #suspected error
    h >=100  & h <140  ~ h,
    
    #error
    h==0 ~ h,
    
    TRUE ~ NA_real_
  )
}

# male_overview <- v3 %>%
#   select(
#     sex_valid,
#     height_2024,
#     weight_2024,
#     w24_hes,
#     w24_plaus
#   )
# View(male_overview)


##valid 21 funnel ----
v3 <- v3 %>%
  mutate(
    h21_treated = sapply(height_2021, valid_height_logic),
    h_flag = case_when(
      height_2021 >= 1000000 ~ "7-digit",
      height_2021 >= 100000 ~ "6-digit",
      height_2021 >= 10000 ~ "5-digit",
      height_2021 >= 1000 ~ "4-digit",
      
      height_2021 > 200 & height_2021 < 1000 ~ "suspected error",
      height_2021 >=140 & height_2021 <=210 ~ "within reason",
      height_2021 >=100 & height_2021 < 140 ~ "suspected error",
      height_2021 >10 & height_2021 <= 18 ~ "Theory A",
      height_2021 >=1 & height_2021 <=10 ~ "suspected error",
      height_2021 > 18 & height_2021 < 100 ~ "Theory B",
      height_2021 == 0 ~ "x=0",
      TRUE ~"NA"
    ),
    h21_treated = round(h21_treated)
  )
View(v3)

# sum(!is.na(v3$height_2021))
# sum(v3$height_2021>=140 & v3$height_2021<=210, na.rm=TRUE)

#sanity check
#sum(v3$h21_flag != "within reason" & v3$h21_flag != "NA", na.rm = TRUE)

v3 <- v3 %>%
  mutate(h_impt = case_when(
    sex_valid == "Male" & !is.na(height_2024) & height_2024 >= 140 & height_2024 <= 210 ~ height_2024,
    
    # 2. FEMALES: Keep valid 2021 height if it exists
    sex_valid == "Female" & h_flag == "within reason" ~ h21_treated,
    
    # 3. FEMALES: Substitute from 2024 if 2021 is missing or an error
    sex_valid == "Female" & (is.na(height_2021) | h_flag %in% c("x=0", "suspected error")) & 
      !is.na(height_2024) & height_2024 >=140 & height_2024 <= 210 ~ height_2024,
    
    # 4. FEMALES: Keep other treated height theories
    h_flag %in% c("Theory A", "Theory B") | grepl("digit", h_flag) ~ h21_treated,
    
    TRUE ~ NA_real_
  ))


v3 <- v3 %>%
  mutate(
    h_impt_flag = case_when(
      # 1. MALES: New category for the recovered male cohort
      sex_valid == "Male" & !is.na(height_2024) & 
        height_2024 >= 140 & height_2024 <= 210 ~ "Male 2024",
      
      # 2. THE 2021 GROUP (Females): Original and math-rescued data
      sex_valid == "Female" & 
        (h_flag == "within reason" | grepl("digit", h_flag) | h_flag %in% c("Theory A", "Theory B")) &
        h21_treated >= 140 & h21_treated <= 210 ~ "Original and Treated",
      
      # 3. THE SUBSTITUTION GROUP (Females): Fixed with 2024 data
      sex_valid == "Female" & h_flag %in% c("suspected error", "x=0", "NA") & 
        !is.na(height_2024) & 
        height_2024 >= 140 & height_2024 <= 210 ~ "Substituted",
      
      # 4. THE DROPPED GROUP: No valid 2024 reference available
      (h_flag %in% c("suspected error", "x=0","NA") | is.na(height_2021)) & 
        (is.na(height_2024) | height_2024 < 140 | height_2024 > 210) ~ "NA - No reference",
      
      # 5. FALLBACK
      TRUE ~ "NA - Other"
    )
  )
View(v3)


h_f_full <- sum(v3$h_impt_flag == "Original and Treated", na.rm=TRUE) + sum(v3$h_impt_flag=="Substituted", na.rm = TRUE)
h21_f_T <- sum(v3$h_impt_flag=="Original and Treated")
h24_m <- sum(v3$h_impt_flag=="Male 2024")
h_final <- sum(!v3$h_impt_flag %in% c("NA - Other", "NA - No reference"))

##plot 11: treated height 2021 distribution with two density curves overlay----
binwidth_h <- 2

ggplot(v3) +
  geom_histogram(
    aes(x = height_2021), 
    binwidth = binwidth_h, 
    fill = "antiquewhite", color = "white", alpha = 0.8
  ) +
  
  # BLUE CURVE: treated & substituted data, females only
  geom_density(
    data = subset(v3, h_impt_flag %in% c("Substituted", "Original and Treated")),
    aes(x = h_impt, y = after_stat(density) * h_f_full * binwidth_h, color = "2021 height treated and substituted, females"), 
    linewidth = 1.2, linetype = "dashed", adjust = 1.5, na.rm = TRUE, alpha = 0.8
  ) +

  # RED CURVE: final, two genders
  geom_density(
    data = subset(v3, !h_impt_flag %in% c("NA - No reference", "NA - Other" )),
    aes(x = h_impt, y = after_stat(density) * h_final * binwidth_h, color = "2021 height treated and substituted, total"), 
    linewidth = 0.8, linetype = "solid", adjust = 1.5, na.rm = TRUE, alpha = 0.5
  ) +
  
  scale_color_manual(values = c(
    "2021 height treated and substituted, females" = "#0082B9", 
    "2021 height treated and substituted, total" = "#21C1BD"
  )) +
  scale_x_continuous(limits = c(130, 220), breaks = seq(130, 220, 10)) + 
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title = "Original 2021 height distribution, with density overlay of treated and substituted data",
    x = "Height (cm)", 
    y = "Number of Participants", 
    color = "Density Curve"
  ) +
  theme_minimal()

##plot 12: boxplot visualizing outliers and treated 2021----
summary(v3$h21_valid)
summary(v3$h_impt)

library(dplyr)
library(tidyr)

# Transform wide data to long format
v3_long <- v3 %>%
  # Select only the columns we want to compare
  select(height_2021, h21_treated, h_impt) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Source", 
    values_to = "Height"
  ) %>%
  # Rename the values in 'Source' to match your color vector labels
  mutate(Source = case_when(
    Source == "height_2021" ~ "Raw 2021",
    Source == "h21_treated" ~ "Treated 2021",
    Source == "h_impt"      ~ "Treated 2021 with substitutions from 2024",
    TRUE                    ~ Source
  ))


# 2. Generate the Boxplot
ggplot(v3_long, aes(x = Source, y = Height, fill = Source)) +
  # Boxplot with outlines that contrast against the fills
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 1, color = "black") +
  
  # SCALE: Keeping the Log10 scale for the 1,000,000 outliers
  scale_y_log10(
    breaks = c(1, 10, 140, 168, 200, 1000, 10000, 100000, 1000000),
    labels = scales::comma
  ) +

  # THEME & LABELS
  labs(
    title = "Comparison of outliers among different data treatment",
    subtitle = paste0("Treatment with substitutions includes male participants\n"), 
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
###height stratified----
h_f_sub <- sum(v3$h_impt_flag == "Substituted", na.rm = TRUE)

# 2. Create the table without the divider line to avoid the 'NA'
h_summary_table <- data.frame(
  "Data Category" = c("Female: Original & Treated", 
                      "Female: Substituted from 2024", 
                      "Male: Recovered 2024", 
                      "TOTAL ANALYTICAL SAMPLE"),
  "N" = c(h21_f_T, 
          h_f_sub, 
          h24_m, 
          h_final)
)

# View it
print(h_summary_table)


quantile(v3$h21_valid, probs = c(0.025, 0.975), na.rm = TRUE)
quantile(v3$h_impt, probs = c(0.025, 0.975), na.rm = TRUE)
sum(!is.na(v3$h_impt))
h21_valid_sd <- sd(v3$h21_valid, na.rm = TRUE)
h21_valid_sd
h_impt_sd <- sd(v3$h_impt, na.rm=TRUE)
h_impt_sd

v3 %>%
  count(h_impt_flag) #substituted from 2024 = 37

# heights_overview <- v3 %>%
#   select(ipnr, height_2021, height_2024, h21_valid, h21_flag, h_impt_flag)
# 
# h21_valid_check <- heights_overview %>%
#   filter(h21_flag %in% c("7-digit", "6-digit", "5-digit", "4-digit",
#                          "Theory A",
#                          "Theory B",
#                          "suspected error",
#                          "x=0")) %>%
#   
#   # 2. Select the columns for your overview
#   select(height_2021, height_2024, h21_valid, h21_flag, h_impt_flag ) %>%
#   
#   # 3. Add a difference check to see how far off the raw data is from 2024
#   mutate(gap_to_h24 = abs(h21_valid - height_2024)) %>%
#   
#   # 4. Sort by flag so you can review them in groups
#   arrange(h21_flag)
# View(h21_valid_check)











#SMD ANALYSIS----
v3_long <- v3 %>%
  select(
    w21_hes, w21_plaus, w24_hes, w24_plaus,
    h21_treated, h_impt, 
    weight_2021, weight_2024, height_2021
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Source",
    values_to = "Value"
  ) %>%
  mutate(
    # 1. Identify if it's Weight or Height
    Measure = if_else(grepl("weight|w21|w24", Source), "Weight", "Height"),
    
    # 2. Create the Status labels
    Status = case_when(
      grepl("weight_2021|weight_2024|height_2021", Source) ~ "Original",
      grepl("hes", Source) ~ "HES",
      Source == "h_impt" ~ "Substituted",
      grepl("plaus|treated", Source) ~ "Plausibility",
      TRUE ~ "Other"
    ),
    # 3. FORCE BASELINE: Order levels so 'Original' is always Group 1 (Reference)
    Status = factor(Status, levels = c("Original", "HES", "Plausibility", "Substituted"))
  )
View(v3_long)

library(dplyr)
library(tidyr)
library(smd)

##treated VS original----
# 1. Comparison for Weight 2021 (HES vs Original)
w21_hesComp <- v3_long %>% 
  filter(Source %in% c("weight_2021", "w21_hes")) %>% 
  droplevels() # This removes the 'Plausibility' and 'Substituted' labels
SMD_w21_hes <- smd(w21_hesComp$Value, w21_hesComp$Status, na.rm = TRUE)

# 2. Comparison for Weight 2021 (Plausibility vs Original)
w21_plausComp <- v3_long %>% 
  filter(Source %in% c("weight_2021", "w21_plaus")) %>% 
  droplevels()
SMD_w21_plaus <- smd(w21_plausComp$Value, w21_plausComp$Status, na.rm = TRUE)

#3 Comparison for Weight 2024 (HES vs Original)
w24_hesComp <- v3_long %>% 
  filter(Source %in% c("weight_2024", "w24_hes")) %>% 
  droplevels() # This removes the 'Plausibility' and 'Substituted' labels
SMD_w24_hes <- smd(w24_hesComp$Value, w24_hesComp$Status, na.rm = TRUE)

# 4 Comparison for Weight 2024 (Plausibility vs Original)
w24_plausComp <- v3_long %>% 
  filter(Source %in% c("weight_2024", "w24_plaus")) %>% 
  droplevels()
SMD_w24_plaus <- smd(w24_plausComp$Value, w24_plausComp$Status, na.rm = TRUE)

# 5. Comparison for Height 2021 (Substituted vs Original)
h_imptComp <- v3_long %>% 
  filter(Source %in% c("height_2021", "h_impt")) %>% 
  droplevels()
SMD_h_impt <- smd(h_imptComp$Value, h_imptComp$Status, na.rm = TRUE)

#6 Comparison for Height 2021 (treated vs Original)
h_treatedComp <- v3_long %>% 
  filter(Source %in% c("height_2021", "h21_treated")) %>% 
  droplevels()
SMD_h_treated <- smd(h_treatedComp$Value, h_treatedComp$Status, na.rm = TRUE)


## Define the pairs for comparison----
comparisons <- list(
  w21_hes_v_O    = c("weight_2021", "w21_hes"),
  w21_plaus_v_O  = c("weight_2021", "w21_plaus"),
  
  w24_hes_v_O    = c("weight_2024", "w24_hes"),
  w24_plaus_v_O  = c("weight_2024", "w24_plaus"),
  
  h_treated_v_O = c("height_2021", "h21_treated"),
  h_impt_v_O   = c("height_2021", "h_impt")
)

smd_results <- lapply(names(comparisons), function(nm) {
  pair <- comparisons[[nm]]
  
  # 1. Filter the data
  df_sub <- v3_long %>% 
    filter(Source %in% pair) %>% 
    filter(!is.na(Value))
  df_sub <- df_sub %>% droplevels()
  result <- smd(df_sub$Value, df_sub$Status, na.rm = TRUE)
  return(result$estimate)
})

# Name and View
names(smd_results) <- names(comparisons)
unlist(smd_results)

##among treated ----
comparisons_among_treated <- list(
  w21_plaus_v_hes = c("w21_hes", "w21_plaus"),
  w24_plaus_v_hes = c("w24_hes", "w24_plaus"),
  h_impt_v_treated  = c("h21_treated", "h_impt")
)

# Run all "Among Treated" comparisons at once
smd_among_results <- lapply(comparisons_among_treated, function(pair) {
  # 1. Filter and Drop unused levels to fix the "NA" error
  df_sub <- v3_long %>% 
    filter(Source %in% pair) %>% 
    droplevels() 
  
  # 2. Run SMD (R uses Factor Ranks to pick Baseline)
  res <- smd(df_sub$Value, df_sub$Status, na.rm = TRUE)
  
  # 3. Return the estimate
  return(res$estimate)
})

# Display final numeric estimates
unlist(smd_among_results)





















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
sd(v3$BMI_21, na.rm = TRUE)
sd(v3$BMI_24, na.rm = TRUE)

v3 %>%
  count(BMI_24_label)

##plot 13: scatter plot----
ggplot(v3, aes(x = BMI_21, y = BMI_24)) +
  # Use very low alpha (0.1) due to the large N = 44,834
  geom_point(alpha = 0.1, color = "midnightblue") +
  
  # Add the 'No Change' reference line
  geom_abline(intercept = 0, slope = 1, color = "orange", linetype = "dashed", linewidth = 1) +
  
  geom_smooth(
    aes(color = "Linear Trend"), 
    method = "lm", 
    se = TRUE, linewidth = 1, linetype = "solid"
  ) +
  
  # Focus on the realistic BMI range (15 to 50)
  coord_cartesian(xlim = c(10, 70), ylim = c(10, 70)) +
  
  labs(
    title = "BMI in 2021 and 2024",
    subtitle = paste0("Plausibility interval for weight; Treated and substituted data for height\n"), 
    x = "BMI in 2021",
    y = "BMI in 2024"
  ) +
  theme_minimal()

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

##plot 14: BMI 21 & 24 density overlay----
# Reshape just the BMI columns for easy plotting
bmi_overlay_data <- v3 %>%
  select(BMI_21, BMI_24) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "BMI") %>%
  mutate(Year = if_else(Year == "BMI_21", "2021", "2024")) %>%
  filter(!is.na(BMI))

# Create the overlay plot
ggplot(bmi_overlay_data, aes(x = BMI, fill = Year)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("2021" = "#AED581", "2024" = "#33691E")) +
  coord_cartesian(xlim = c(10, 70)) + # Focused range for clinical relevance
  labs(
    title = "Population BMI Shift: 2021 vs 2024",
    x = "Body Mass Index (BMI)",
    y = "Density"
  ) +
  theme_minimal()














#H1: LS & BMI global----
##sample audit: understanding attrition----
sample_audit <- v3 %>%
  summarise(
    Step_0_Total_Rows = n(),
    
    # 2021 Filters
    Step_1_Has_BMI_21 = sum(!is.na(BMI_21_label)),
    Step_2_Has_BMI_and_LS_21 = sum(!is.na(BMI_21_label) & !is.na(LS_2021)),
    
    # 2024 Filters
    Step_3_Has_BMI_24 = sum(!is.na(BMI_24_label)),
    Step_4_Longitudinal_BMI_LS = sum(!is.na(BMI_21_label) & !is.na(LS_2021) & 
                                       !is.na(BMI_24_label) & !is.na(LS_2024)),
    
    # Effect Modifiers (Replace 'Gender'/'Age' with your actual column names)
    Step_5_Full_Analytical_Sample = sum(!is.na(BMI_21_label) & !is.na(LS_2021) & 
                                          !is.na(BMI_24_label) & !is.na(LS_2024) &
                                          !is.na(sex_valid) & !is.na(age_2021_imputed))
  )

# Transpose it to make it a readable list
t(sample_audit)





# 1. Summary Table of Life Satisfaction by BMI Category
# Check the association using your professional labels
h1_results <- v3 %>%
  filter(!is.na(BMI_21_label), !is.na(LS_2021)) %>%
  group_by(BMI_21_label) %>%
  summarise(
    n = n(),
    mean_LS21 = mean(LS_2021, na.rm = TRUE),
    sd_LS21 = sd(LS_2021, na.rm = TRUE)
  ) %>%
  # Ensure the categories are in the right order for the table
  mutate(BMI_21_label = factor(BMI_21_label, 
                               levels = c("Underweight", "Healthy", "Overweight", 
                                          "Obese I", "Obese II", "Obese III"))) %>%
  arrange(BMI_21_label)

print(h1_results)



##one-way ANOVA----
# 1. Create a dedicated Hypothesis 1 dataframe
h1_data <- v3 %>%
  # Keep only those who have BOTH necessary ingredients
  filter(!is.na(BMI_21_label), !is.na(LS_2021)) %>%
  # This 'drops' any factor levels that have 0 people (like 'NA')
  mutate(BMI_21_label = droplevels(factor(BMI_21_label, 
                                          levels = c("Underweight", "Healthy", "Overweight",
                                                     "Obese I", "Obese II", "Obese III"))))

# 2. Run the ANOVA on this specific subset
h1_anova_final <- aov(LS_2021 ~ BMI_21_label, data = h1_data)

# 3. Check the summary
summary(h1_anova_final)
# 2. Run the ANOVA again on the NEW v3
h1_anova_final <- aov(LS_2021 ~ BMI_21_label, data = v3)

# 3. Check the "Observations Deleted" message at the bottom
summary(h1_anova_final)



##⭐️TABLE 1⭐️----
# 1. Install and load the package
library(gtsummary)

# 1. First, make sure sex_valid is a factor with both levels
v3$sex_valid <- factor(v3$sex_valid)

# 2. Build the table with safety checks
table1_final <- v3 %>%
  # Filter out the NAs for BMI first so the p-values don't get confused
  filter(!is.na(BMI_24_label)) %>% 
  select(age_2021_imputed, sex_valid, w21_plaus, h_impt, BMI_24_label, LS_2024) %>%
  tbl_summary(
    by = BMI_24_label,
    # 'categorical' forces both Male/Female to show even if one is low N
    type = list(sex_valid ~ "categorical"), 
    statistic = list(all_continuous() ~ "{mean} (±{sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label = list(sex_valid ~ "Sex", LS_2024 ~ "Life Satisfaction")
  ) %>%
  add_overall() %>%
  # Using 'everything()' ensures it tries to calculate p-values for all
  add_p(test = list(all_continuous() ~ "kruskal.test", 
                    all_categorical() ~ "chisq.test")) %>% 
  bold_labels()

# 3. View it
table1_final




# ###marginal effects plot----
# library(sjPlot)
# library(sjmisc)
# 
# # This creates a summary table of LS_2021 by BMI Categories
# # It's perfect for showing the Means and SDs clearly
# v3 %>%
#   group_by(BMI_21_label) %>%
#   select(LS_2021, age_2021_imputed, weight_2021) %>% 
#   descr() %>% # From the sjmisc package
#   as.data.frame()
# 
# # 1. Re-run your model as a Linear Model (lm)
# fit <- lm(LS_2021 ~ BMI_21_label, data = v3)
# 
# # 2. Plot the predicted Life Satisfaction for each category
# plot_model(fit, type = "pred", terms = "BMI_21_label") +
#   theme_sjplot() +
#   labs(title = "Predicted Life Satisfaction by BMI Category",
#        y = "Life Satisfaction (1-10)",
#        x = "BMI Category")







##plot 15: scatterplot LS21 & BMI----
library(ggplot2)

v3 %>%
  # # Apply your specific H1 exclusion criteria
  # filter(age_2021 >= 25) %>%
  # Remove NAs for the specific variables being plotted
  filter(!is.na(BMI_21), !is.na(LS_2021)) %>%
  # 
  ggplot(aes(x = BMI_21, y = LS_2021)) +
  # Jitter to show the density of your mature nurse population
  geom_jitter(alpha = 0.05, color = "midnightblue", width = 0.2, height = 0.2) +
  
  # The "H1 Line": Linear association for age 25+
  geom_smooth(method = "lm", color = "firebrick", linewidth = 1.2) +
  
  # Standardize the view to the 15-50 BMI range you cleaned
  coord_cartesian(xlim = c(10, 70)) +
  
  labs(
    title = "Hypothesis 1: BMI & Life Satisfaction (Full Cohort)",
    subtitle = paste0("N = ", sum(!is.na(v3$age_2021_imputed)), 
                      " | Substituted height & plausibility weight used\n",
                      "Red line indicates the linear association for all ages."),
    x = "BMI in 2021",
    y = "Life Satisfaction in 2021"
  ) +
  theme_minimal()

##statistics ----
###linear regression, 25+----
h1_model <- lm(LS_2021 ~ BMI_21, data = v3 %>% filter(age_2021 >= 25))
stargazer(h1_model,
          type = "text",
          title = "Table 2: Linear Regression of BMI on Life Satisfaction (Ages 25+)",
          column.labels = c("Life Satisfaction (2021)"),
          covariate.labels = c("BMI (2021)"),
          omit.stat = c("f", "ser"), # Clean up the bottom of the table
          digits = 3)

# library(sjPlot)
# 
# # 1. Run your H1 Model
# h1_model <- lm(LS_2021 ~ BMI_21, data = v3 %>% filter(age_2021 >= 25))
# 
# # 2. Create the HTML table for the Viewer Pane
# tab_model(h1_model, 
#           title = "Table 2: Association Between BMI and Life Satisfaction (Ages 25+)",
#           pred.labels = c("(Intercept)", "BMI (2021)"),
#           dv.labels = "Life Satisfaction Score",
#           string.p = "p-value",
#           digits = 3)
# library(stargazer)

###linear regression, full cohort----
h1_model_full <- lm(LS_2021 ~ BMI_21, data = v3)
stargazer(h1_model_full,
          type = "text",
          title = "Table 2: Linear Regression of BMI on Life Satisfaction (full cohort)",
          column.labels = c("Life Satisfaction (2021)"),
          covariate.labels = c("BMI (2021)"),
          omit.stat = c("f", "ser"), # Clean up the bottom of the table
          digits = 3)


# 







##plot 16: scatterplot w curve ----
v3 %>%
  # Apply filters BEFORE ggplot
  # filter(age_2021 >= 25) %>%
  filter(!is.na(BMI_21), !is.na(LS_2021)) %>%
  
  ggplot(aes(x = BMI_21, y = LS_2021)) +
  # Midnight blue points as requested
  geom_jitter(alpha = 0.05, color = "midnightblue", width = 0.2, height = 0.2) +
  
  # The loess line shows the "bend" at the low end
  geom_smooth(method = "loess", color = "steelblue1", linewidth = 1.2) + 
  
  # Standardize the view to see the full cleaned range
  coord_cartesian(xlim = c(10, 70)) +
  
  labs(
    title = "Hypothesis 1: Curvilinear Relationship (Full Cohort)",
    x = "BMI in 2021",
    y = "Life Satisfaction in 2021"
  ) +
  theme_minimal()



#DESCRIPTIVE STAT---
library(table1)
library(dplyr)

# 2. Prepare the analytic sample (N = 32,377)
# Using your v3 variables and filtering for complete cases
table1_data <- v3 %>%
  filter(age_2024_imputed >= 25) %>%
  select(
    age = age_2024_imputed,
    height = h_impt,
    weight = w21_plaus,
    bmi = BMI_24,
    ls = LS_2024,
    bmi_cat = BMI_24_label
  ) %>%
  na.omit()

# 3. Add professional labels (This makes the table look like a journal article)
label(table1_data$age)      <- "Age (Years)"
label(table1_data$height)   <- "Height (cm)"
label(table1_data$weight)   <- "Weight (kg)"
label(table1_data$bmi)      <- "Body Mass Index (kg/m²)"
label(table1_data$ls)       <- "Life Satisfaction (0-10)"
label(table1_data$bmi_cat)  <- "BMI Category (WHO)"

# 4. Generate the "Xiaohongshu-style" Three-Line Table
# This creates a professional HTML table you can copy directly to PPT
table1(~ age + height + weight + bmi + ls + bmi_cat, 
       data = table1_data,
       caption = "Table 1: Descriptive Statistics of the 2021 Danish Nurse Cohort Registry")














