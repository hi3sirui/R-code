library(dplyr)

df <- read.csv("C:/Users/SZHA0012/Documents/R-code/v3F.csv")
View(df)

library(ggplot2)

df %>%
  count(dad_physique_2021)


prep <- df %>%
  filter(
    !is.na(LS_2021), 
    !is.na(weight_statement_a_2021), 
    !is.na(BMI_21),
    !is.na(mom_physique_2021),
    !is.na(dad_physique_2021)
  ) %>%
  mutate(
    # Exposure: Childhood weight perception (Ref: No particular difference)
    CWP_21 = factor(weight_statement_a_2021, 
                            levels = c(3, 1, 2), 
                            labels = c("No difference", "Thicker", "Thinner")),
    
    # Modifier 1: BMI Groups for interaction (Ref: Healthy Weight)
    # Using existing BMI_21_label logic
    bmi_group = factor(BMI_21_label, 
                       levels = c("Healthy", "Underweight", "Overweight", "Obese I", "Obese II", "Obese III")),
    
    # Modifier 2: Parental Body Size (Binary: Large vs Not Large)
    # Typically, in the DNC, values >= 4 or 5 represent 'Large'. 
    # Adjust the threshold (here using 4) based on your specific scale.
    parental_large2 = if_else(mom_physique_2021 <= 3 & dad_physique_2021 <= 3, 1, 0),
    parental_large2 = factor(parental_large2, levels = c(0, 1), labels = c("Not Large", "Both large"))
  )


# Model 1: Simple Linear Regression----
model1 <- lm(LS_2021 ~ CWP_21 + age_2021_imputed, data = prep)
summary(model1)

##visual, no age control----
library(ggplot2)
library(dplyr)

# 1. Prepare data for plotting (calculating means and standard errors)
plot_data <- prep %>%
  group_by(CWP_21) %>%
  summarise(
    mean_LS = mean(LS_2021, na.rm = TRUE),
    se_LS = sd(LS_2021, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

# 2. Create the Bar Plot
ggplot(plot_data, aes(x = CWP_21, y = mean_LS, fill = CWP_21)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_LS - 1.96 * se_LS, ymax = mean_LS + 1.96 * se_LS), 
                width = 0.2) +
  # Using your preferred "antiquewhite" and a blue scale for contrast
  scale_fill_manual(values = c("No difference" = "antiquewhite3", 
                               "Thicker" = "#d6604d", 
                               "Thinner" = "#4393c3")) +
  coord_cartesian(ylim = c(6.5, 7.5)) + # Focusing on the relevant range
  labs(
    title = "Life Satisfaction by Childhood Weight Perception",
    x = "Perception of Weight (Before Age 13)",
    y = "Mean Life Satisfaction (0-10)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

##visual, w/ age control----
library(sjPlot)
plot_model(model1, type = "pred", terms = "CWP_21") +
  theme_minimal() +
  labs(
    title = "Adjusted Life Satisfaction by Childhood Weight Perception",
    subtitle = "Adjusted for Age",
    x = "Childhood Weight Perception",
    y = "Predicted Life Satisfaction (0-10)"
  )

library(ggplot2)
library(dplyr)

##visual: proportions of categorized LS by childhood weight perception----
prep %>%
  # 1. Categorize LS LOCALLY within this pipe
  mutate(LS_group = case_when(
    LS_2021 <= 4 ~ "Dissatisfied (0-4)",
    LS_2021 == 5 ~ "Neutral (5)",
    LS_2021 >= 6 ~ "Satisfied (6-10)"
  )) %>%
  # 2. Convert to factor to ensure correct order in the legend
  mutate(LS_group = factor(LS_group, 
                           levels = c("Satisfied (6-10)", "Neutral (5)", "Dissatisfied (0-4)"))) %>%
  # 3. Pass this temporary data to ggplot
  ggplot(aes(x = CWP_21, fill = LS_group)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  # Using the red-blue-grey palette established in your repository
  scale_fill_manual(values = c("Satisfied (6-10)" = "#2166ac", 
                               "Neutral (5)" = "#d1d1d1", 
                               "Dissatisfied (0-4)" = "#d6604d")) +
  labs(title = "Proportion of Life Satisfaction by Childhood Perception",
       x = "Childhood Weight Perception",
       y = "Percentage of Participants",
       fill = "Life Satisfaction") +
  theme_minimal()





#Model 2: + current BMI----
# Model 2: Interaction between Childhood Perception and Current BMI
# We ensure 'Healthy' is the reference for BMI and 'No difference' for Childhood
model2 <- lm(LS_2021 ~ CWP_21 * BMI_21_label + age_2021_imputed, data = prep)

summary(model2)
