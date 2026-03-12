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

View(v3)

##making cpr_alder to integer ----
v3 <- v3 %>%
  mutate(age_2021 = trunc(age_2021),
         age_2024 = trunc(age_2024))

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
    age_2021_imputed=trunc(age_2021_imputed)
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
##weight filtering----
library(dplyr)
library(gtsummary)
rescue_weight <- function(w_input, w_anchor, sex, year_val) {
  # 1. Pre-process the input: If 0 or NA, borrow from anchor
  x <- if_else(is.na(w_input) | w_input == 0, w_anchor, w_input)
  
  # 2. Run the logic
  case_when(
    # BLOCKER: If it's a 2021 Male, do NOT apply rescue, return original/NA
    year_val == 2021 & sex == "Male" ~ as.numeric(w_input),
    
    # Rule 5: Height Swap (Priority)
    x > 140 & x <= 190 & !is.na(w_anchor) & w_anchor < 100 ~ x %% 100,
    
    # Rule 0: Baseline (Standard plausible weight)
    x >= 40 & x <= 192 ~ x,
    
    # Rule 6: Missing Zero (e.g., 7 becomes 70)
    x > 0 & x < 10 ~ x * 10,
    
    # Rule 1: Grams (e.g., 75000 becomes 75)
    x >= 10000 & x < 100000 ~ x %/% 1000,
    
    # Rule 2: Dec High (>1400)
    x > 1400 & x < 10000 ~ x %/% 100,
    
    # Rule 3: Dec Low (1000-1400)
    x >= 1000 & x <= 1400 ~ x %/% 10,
    
    # Rule 4: 3-digit High (508-1000)
    x > 508 & x < 1000 ~ x %/% 10,
    
    # Default: If it doesn't fit any rule, it's garbage data
    TRUE ~ NA_real_ 
  )
}

v3 <- v3 %>%
  mutate(
    # Step 1: Clean 2021 using the ORIGINAL 2024 as the truth anchor
    w21_treated = rescue_weight(
      w_input  = weight_2021, 
      w_anchor = weight_2024, 
      sex      = sex_valid, 
      year_val = 2021
    ),
    
    # Step 2: Clean 2024 using the ORIGINAL 2021 as the truth anchor
    w24_treated = rescue_weight(
      w_input  = weight_2024, 
      w_anchor = weight_2021, # <--- Still using the raw 'weight_2021' here
      sex      = sex_valid, 
      year_val = 2024
    )
  )


v3 <- v3 %>%
  mutate(
    # 2021 Columns
    w21_plaus = if_else(w21_treated >= 40 & w21_treated <= 192, w21_treated, NA_real_),
    w21_hes   = if_else(w21_treated >= 48.5 & w21_treated <= 86.5, w21_treated, NA_real_),
    
    # 2024 Columns
    w24_plaus = if_else(w24_treated >= 40 & w24_treated <= 192, w24_treated, NA_real_),
    w24_hes   = if_else(w24_treated >= 48.5 & w24_treated <= 99, w24_treated, NA_real_)
  )

v3 <- v3 %>%
  mutate(
    w24_plaus_f = case_when(
      w24_treated >=40 & w24_treated<=192 & sex_valid == "Female" ~ w24_treated,
      TRUE ~ NA_real_
    )
  )

v3 <- v3 %>%
  mutate(
    w24_plaus_m = case_when(
      w24_treated>=40 & w24_treated<=192 & sex_valid == "Male" ~ w24_treated,
      TRUE ~ NA_real_
    )
  )

v3 <- v3 %>%
  mutate(
    w24_hes_f = case_when(
      w24_treated>=48.5 & w24_treated<=86.5 & sex_valid == "Female" ~ w24_treated,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )

v3 <- v3 %>%
  mutate(
    w24_hes_m = case_when(
      w24_treated>=58.9 & w24_treated<=99 & sex_valid == "Male" ~ w24_treated,
      TRUE ~ NA_real_ #real_ for numeric NA
    )
  )



##SMD, HES as baseline----
###2021----
library(smd)
smd_w21 <- smd(
  x = c(v3$w21_hes, v3$w21_plaus),
  g = c(rep("HES", nrow(v3)), rep("Plaus", nrow(v3))),
  na.rm = TRUE
)
print(smd_w21)

###2024----
smd_w24 <- smd(
  x = c(v3$w24_hes, v3$w24_plaus),
  g = c(rep("HES", nrow(v3)), rep("Plaus", nrow(v3))),
  na.rm = TRUE
)
print(smd_w24)


##summary statistics----
library(dplyr)
summary(v3$w21_hes)
summary(v3$w24_hes)

###HES----
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

###answered both years----
###HES interval----
sum(!is.na(v3$w21_hes) & !is.na(v3$w24_hes))
sum(!is.na(v3$w21_hes)&!is.na(v3$w24_hes_f))

###plausibility interval----
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus))
sum(!is.na(v3$w21_plaus) & !is.na(v3$w24_plaus_f)) #n = 17420


















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

##plot 13: treated height 2021 distribution with two density curves overlay----
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

##plot 14: boxplot visualizing outliers and treated 2021----
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
  count(h_impt_flag)

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
  count(BMI_21_label)

##plot 15: scatter plot----
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
# Formula: y ~ x (Dependent Variable ~ Independent Variable)
bmi_trend_model <- lm(BMI_24 ~ BMI_21, data = v3)

# View the coefficients (Slope and Intercept)
summary(bmi_trend_model) #0.96859 


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

##plot 16: BMI 21 & 24 density overlay----
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
    Step_4_Longitudinal_BMI_LS = sum(!is.na(BMI_21_label) & !is.na(LS_2021) & !is.na(BMI_24_label) & !is.na(LS_2024)),
    
    # Effect Modifiers (Replace 'Gender'/'Age' with your actual column names)
    Step_5_Full_Analytical_Sample = sum(!is.na(BMI_21_label) & !is.na(LS_2021) & !is.na(BMI_24_label) & !is.na(LS_2024) & !is.na(sex_valid) & !is.na(age_2021_imputed))
  )

# Transpose it to make it a readable list
t(sample_audit) #17568




###LS by BMI categories, 2021----
# Check the association using your professional labels
h21_results <- v3 %>%
  filter(!is.na(BMI_21_label), !is.na(LS_2021)) %>%
  group_by(BMI_21_label) %>%
  summarise(
    n = n(),
    mean_LS21 = mean(LS_2021, na.rm = TRUE),
    sd_LS21 = sd(LS_2021, na.rm = TRUE)
  ) %>%
  # Ensure the categories are in the right order for the table
  mutate(BMI_21_label = factor(BMI_21_label, 
                               levels = c("Underweight", "Healthy", "Overweight", "Obese I", "Obese II", "Obese III"))) %>%
  arrange(BMI_21_label)

print(h21_results)

###LS by BMI categories, 2024----
h24_results <- v3 %>%
  filter(!is.na(BMI_24_label), !is.na(LS_2024)) %>%
  group_by(BMI_24_label) %>%
  summarise(
    n = n(),
    mean_LS24 = mean(LS_2024, na.rm = TRUE),
    sd_LS24 = sd(LS_2024, na.rm = TRUE)
  ) %>%
  # Ensure the categories are in the right order for the table
  mutate(BMI_24_label = factor(BMI_24_label, 
                               levels = c("Underweight", "Healthy", "Overweight", "Obese I", "Obese II", "Obese III"))) %>%
  arrange(BMI_24_label)

print(h24_results)



##one-way ANOVA----
# 1. Create a dedicated Hypothesis 1 dataframe
anova21 <- v3 %>%
  # Keep only those who have BOTH necessary ingredients
  filter(!is.na(BMI_21_label), !is.na(LS_2021)) %>%
  # This 'drops' any factor levels that have 0 people (like 'NA')
  mutate(BMI_21_label = droplevels(factor(BMI_21_label, 
                                          levels = c("Underweight", "Healthy", "Overweight", "Obese I", "Obese II", "Obese III"))))

# 2. Run the ANOVA on this specific subset
ANOVA_21 <- aov(LS_2021 ~ BMI_21_label, data = anova21)
summary(ANOVA_21)

###LS by BMI, 2021, boxplot----
ggplot(v3 %>% filter(!is.na(BMI_21_label), !is.na(LS_2021)), 
       aes(x = BMI_21_label, y = LS_2021, fill = BMI_21_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Hide outliers to keep it clean
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(
    title = "Life Satisfaction across BMI Categories (2021)",
    x = "BMI Category",
    y = "Life Satisfaction (1-10 Scale)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###LS by BMI, 2024, boxplot----
ggplot(v3 %>% filter(!is.na(BMI_24_label), !is.na(LS_2024)), 
       aes(x = BMI_24_label, y = LS_2024, fill = BMI_24_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Hide outliers to keep it clean
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(
    title = "Life Satisfaction across BMI Categories (2024)",
    x = "BMI Category",
    y = "Life Satisfaction (1-10 Scale)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



###LS by BMI, 2024, boxplot----
library(gtsummary)
library(dplyr)
table_output <- v3 %>%
  mutate(
    # This forces the order you want in the table rows
    BMI_21_label = factor(BMI_21_label, levels = c(
      "Underweight", 
      "Healthy", 
      "Overweight", 
      "Obese I", 
      "Obese II", 
      "Obese III"
    ))
  ) %>%
  # Select the original variables you wanted to look at
  select(LS_2021, BMI_21_label) %>%
  tbl_summary(
    by = BMI_21_label, # This puts BMI in columns (or remove 'by' for rows)
    label = list(
      LS_2021 ~ "Life Satisfaction (0-10)",
      BMI_21_label ~ "BMI Category (WHO)"
    ),
    missing = "always",
    missing_text = "Missing Data"
  ) %>%
  add_p() %>%
  bold_labels()

# 2. View the table
table_output



##⭐️TABLE 1⭐️----
##descrptive stat 2021
v3 %>%
  # 1. Filter to Female only (analytical sample)
  filter(!is.na(BMI_21_label) & sex_valid == "Female") %>%
  
  # 2. Select variables, but DROP sex_valid here
  select(BMI_21_label, age_2021_imputed, w21_plaus, h_impt, LS_2021) %>%
  
  tbl_summary(
    by = BMI_21_label,
    missing = "no", 
    statistic = list(all_continuous() ~ "{mean} (±{sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label = list(
      age_2021_imputed ~ "Age (Years)",
      w21_plaus ~ "Weight (kg)",
      h_impt ~ "Height (cm)",
      LS_2021 ~ "Life Satisfaction (1-10)"
    )
  ) %>%
  add_overall() %>%
  # Now add_p() will work because all remaining variables have variation
  add_p(test = list(all_continuous() ~ "kruskal.test")) %>%
  bold_labels()

##descrptive stat 2024
v3 %>%
  # 1. Filter to Female only (analytical sample)
  filter(!is.na(BMI_24_label)) %>%
  
  # 2. Select variables, but DROP sex_valid here
  select(BMI_24_label, age_2024_imputed, w24_plaus, h_impt, LS_2024) %>%
  
  tbl_summary(
    by = BMI_24_label,
    missing = "no", 
    statistic = list(all_continuous() ~ "{mean} (±{sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    label = list(
      age_2024_imputed ~ "Age (Years)",
      w24_plaus ~ "Weight (kg)",
      h_impt ~ "Height (cm)",
      LS_2024 ~ "Life Satisfaction (1-10)"
    )
  ) %>%
  add_overall() %>%
  # Now add_p() will work because all remaining variables have variation
  add_p(test = list(all_continuous() ~ "kruskal.test")) %>%
  bold_labels()


##三线表2021----
table1_data <- v3 %>%
  select(
    age = age_2021_imputed,
    height = h_impt,
    weight = w21_plaus,
    bmi = BMI_21,
    ls = LS_2021,
    bmi_cat = BMI_21_label
  ) %>%
  na.omit()

# 3. Add professional labels (This makes the table look like a journal article)
label(table1_data$age)      <- "Age (Years)"
label(table1_data$height)   <- "Height (cm)"
label(table1_data$weight)   <- "Weight (kg)"
label(table1_data$bmi)      <- "Body Mass Index (kg/m²)"
label(table1_data$ls)       <- "Life Satisfaction (0-10)"
label(table1_data$bmi_cat)  <- "BMI Category (WHO)"

table1(~ age + height + weight + bmi + ls + bmi_cat, 
       data = table1_data,
       caption = "Descriptive Statistics of the 2021 Danish Nurse Cohort Registry")





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







##plot 17: 2021 LS & BMI----
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

### slope of the regression line----
h1_model <- lm(LS_2021 ~ BMI_21, data = v3)

# 2. View the full statistics (p-value, R-squared, etc.)
summary(h1_model)

# 3. Extract just the slope coefficient
h1_slope <- coef(h1_model)["BMI_21"]
print(paste("The slope for the BMI & Life Satisfaction relationship is:", round(h1_slope, 4)))




##LS24 & BMI24----
v3 %>%
  # # Apply your specific H1 exclusion criteria
  # filter(age_2021 >= 25) %>%
  # Remove NAs for the specific variables being plotted
  filter(!is.na(BMI_24), !is.na(LS_2024)) %>%
  # 
  ggplot(aes(x = BMI_24, y = LS_2024)) +
  # Jitter to show the density of your mature nurse population
  geom_jitter(alpha = 0.05, color = "midnightblue", width = 0.2, height = 0.2) +
    geom_smooth(method = "lm", color = "firebrick", linewidth = 1.2) +
    coord_cartesian(xlim = c(10, 70)) +
  
  labs(
    title = "Hypothesis 1: BMI & Life Satisfaction (Full Cohort)",
    subtitle = paste0("N = ", sum(!is.na(v3$age_2024_imputed)), 
                      " | Substituted height & plausibility weight used\n",
                      "Red line indicates the linear association for all ages."),
    x = "BMI in 2024",
    y = "Life Satisfaction in 2024"
  ) +
  theme_minimal()

###slope of the regression line----
h24_model <- lm(LS_2024 ~ BMI_24, data = v3)

# 2. View the full statistics (p-value, R-squared, etc.)
summary(h24_model)

# 3. Extract just the slope coefficient
h24_slope <- coef(h24_model)["BMI_24"]
print(paste("The slope for the BMI & Life Satisfaction relationship is:", round(h24_slope, 4)))




##LS21 & BMI 21----
v3 %>%
  # # Apply your specific H1 exclusion criteria
  # filter(age_2021 >= 25) %>%
  # Remove NAs for the specific variables being plotted
  filter(!is.na(BMI_21), !is.na(LS_2021)) %>%
  # 
  ggplot(aes(x = BMI_21, y = LS_2021)) +
  # Jitter to show the density of your mature nurse population
  geom_jitter(alpha = 0.05, color = "midnightblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "firebrick", linewidth = 1.2) +
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



###slope of the regression line----
h21_model <- lm(LS_2021 ~ BMI_21, data = v3)

# 2. View the full statistics (p-value, R-squared, etc.)
summary(h21_model)

# 3. Extract just the slope coefficient
h21_slope <- coef(h21_model)["BMI_21"]
print(paste("The slope for the BMI & Life Satisfaction relationship is:", round(h21_slope, 4)))





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

###linear regression, 2021, full cohort----
h1_model_full <- lm(LS_2021 ~ BMI_21, data = v3)
stargazer(h1_model_full,
          type = "text",
          title = "Table 2: Linear Regression of BMI on Life Satisfaction (full cohort)",
          column.labels = c("Life Satisfaction (2021)"),
          covariate.labels = c("BMI (2021)"),
          omit.stat = c("f", "ser"), # Clean up the bottom of the table
          digits = 3)

# 1. Run the linear model
# Y (Outcome) is Life Satisfaction, X (Predictor) is BMI
model_linear <- lm(LS_2021 ~ BMI_21, data = v3)

# 2. Look at the coefficients
summary(model_linear)

# 3. Extract the specific slope value
slope <- coef(model_linear)["BMI_21"]
print(paste("The slope of the linear relationship is:", round(slope, 4)))






###linear regression, 2024, full cohort




##curvilinear scatterplot, 2021 ----
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

###finding the vertex of the curve----
# 1. Run the quadratic model
quad_model <- lm(LS_2021 ~ BMI_21 + I(BMI_21^2), data = v3)

# 2. Extract coefficients
b21 <- coef(quad_model)["BMI_21"]
a21 <- coef(quad_model)["I(BMI_21^2)"]

# 3. Calculate Vertex
vertex_bmi_21 <- -b21 / (2 * a21)
vertex_bmi_21




##2024 CURVILINEAR----
v3 %>%
  # Apply filters BEFORE ggplot
  # filter(age_2021 >= 25) %>%
  filter(!is.na(BMI_24), !is.na(LS_2024)) %>%
  
  ggplot(aes(x = BMI_24, y = LS_2024)) +
  # Midnight blue points as requested
  geom_jitter(alpha = 0.05, color = "midnightblue", width = 0.2, height = 0.2) +
  
  # The loess line shows the "bend" at the low end
  geom_smooth(method = "loess", color = "steelblue1", linewidth = 1.2) + 
  
  # Standardize the view to see the full cleaned range
  coord_cartesian(xlim = c(10, 70)) +
  
  labs(
    title = "Hypothesis 1: Curvilinear Relationship (Full Cohort)",
    x = "BMI in 2024",
    y = "Life Satisfaction in 2024"
  ) +
  theme_minimal()

# 1. Run the model on a 'Realistic' BMI range (15 to 50)
quad_model_24_clean <- lm(LS_2024 ~ BMI_24 + I(BMI_24^2), 
                          data = v3 %>% filter(BMI_24 >= 15 & BMI_24 <= 50))

# 2. Extract coefficients
b24 <- coef(quad_model_24_clean)["BMI_24"]
a24 <- coef(quad_model_24_clean)["I(BMI_24^2)"]

# 3. Calculate Vertex: -b / (2 * a)
vertex_bmi_24 <- -b24 / (2 * a24)

print(paste("The Realistic BMI peak for 2024 is:", round(vertex_bmi_24, 2)))




#LS by BMI----
library(table1)
library(dplyr)
##2021----
# Using your v3 variables and filtering for complete cases


##2024----
library(gtsummary)
# 1. Create and store the table
LS_BMI_24 <- v3 %>%
  filter(!is.na(BMI_24_label), !is.na(LS_2024)) %>%
  # Define the 3 groups correctly
  mutate(LS_label = case_when(
    LS_2024 >= 6 ~ "Satisfied",
    LS_2024 == 5 ~ "Neutral",
    TRUE         ~ "Dissatisfied"
  )) %>%
  # Ensure factor levels match the labels created above
  mutate(LS_label = factor(LS_label, levels = c("Satisfied", "Neutral", "Dissatisfied"))) %>%
  select(LS_label, BMI_24_label) %>%
  tbl_summary(
    by = LS_label,
    label = list(BMI_24_label ~ "BMI 2024"),
    percent = "column"
  ) %>%
  add_overall() %>% # Optional: adds a 'Total' column
  add_p() %>%       # <--- THIS ADDS THE P-VALUE COLUMN
  bold_labels()

# 2. Display the table
LS_BMI_24

##2021----
library(gtsummary)
# 1. Create and store the table
LS_BMI_21 <- v3 %>%
  filter(!is.na(BMI_21_label), !is.na(LS_2021)) %>%
  # Define the 3 groups correctly
  mutate(LS_label = case_when(
    LS_2021 >= 6 ~ "Satisfied",
    LS_2021 == 5 ~ "Neutral",
    TRUE         ~ "Dissatisfied"
  )) %>%
  # Ensure factor levels match the labels created above
  mutate(LS_label = factor(LS_label, levels = c("Satisfied", "Neutral", "Dissatisfied"))) %>%
  select(LS_label, BMI_21_label) %>%
  tbl_summary(
    by = LS_label,
    label = list(BMI_21_label ~ "BMI 2021"),
    percent = "column"
  ) %>%
  add_overall() %>% # Optional: adds a 'Total' column
  add_p() %>%       # <--- THIS ADDS THE P-VALUE COLUMN
  bold_labels()

# 2. Display the table
LS_BMI_21



