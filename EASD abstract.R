library(dplyr)

df <- read.csv("C:/Users/SZHA0012/Documents/R-code/v3F.csv")

library(ggplot2)

#prep.df----
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
    bmi_group = factor(BMI_21_label, 
                       levels = c("Healthy", "Underweight", "Overweight", "Obese I", "Obese II", "Obese III")),
    
    bmi_H_O = factor(BMI_21_label,
                     levels = c("Healthy", "Obese I", "Obese II", "Obese III")),
    
    bmi_bin = factor(if_else(BMI_21_label=="Healthy", "Healthy", "Obese"),
                     levels = c("Healthy", "Obese")),
    parental_large = if_else(mom_physique_2021 <= 3 & dad_physique_2021 <= 3, 1, 0),
    parental_large = factor(parental_large, levels = c(0, 1), labels = c("None", "Both"))
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
  coord_cartesian(ylim = c(6.75, 7.5)) + # Focusing on the relevant range
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
model2 <- lm(LS_2021 ~ CWP_21 * bmi_group + age_2021_imputed,
                   data = prep)
summary(model2)

# model2 <- lm(LS_2021 ~ CWP_21 * bmi_H_O + age_2021_imputed,
#                    data = prep)
summary(model2)


##visuals: subgroups lm overlay----
library(ggplot2)
library(ggeffects)
library(dplyr)

predictions <- predict_response(model2, terms = c("CWP_21", "bmi_group"))

ggplot(predictions, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1) + # The lines for each BMI subgroup
  geom_point(size = 3) + # Points for each specific estimate
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + # 95% CIs
  theme_minimal() +
  labs(
    title = "Effect Modification: Life Satisfaction by Childhood Weight perception and Current BMI",
    subtitle = "Adjusted for Age; inspired by Corraini et al, 2017",
    x = "Childhood Weight Perception",
    y = "Predicted Life Satisfaction (0-10)",
    color = "Current BMI"
  ) +
  scale_color_brewer(palette = "Set2")



#Model 2: BMI dichotomized----
library(dplyr)

model2_binary <- prep %>%
  lm(LS_2021 ~ CWP_21 * bmi_bin + age_2021_imputed, data = .)

###visuals: dichotomized BMI----
library(sjPlot)
plot_model(model2_binary, 
           type = "pred", 
           terms = c("CWP_21", "bmi_bin"),
           colors = c("cyan4","firebrick")) +
  theme_minimal() + 
  geom_line(aes(group=group), size = 1) + 
  labs(
    title = "Current BMI as an Effect Modifier",
       subtitle = "Controlled for Age",
       x = "Childhood Weight Perception",
       y = "Predicted Life Satisfaction (0-10)")

##Model 2': healthy v obese categories----
model2_HvO <- prep %>%
  lm(LS_2021 ~ CWP_21 * bmi_H_O + age_2021_imputed, data = .)

summary(model2_HvO)
# 
# library(sjPlot)
# plot_model(model2_HvO, 
#            type = "pred", 
#            terms = c("CWP_21", "bmi_H_O"),
#            colors = c("cyan4","firebrick")) +
#   theme_minimal() + 
#   geom_line(aes(group=group), size = 1) + 
#   labs(
#     title = "Current BMI as an Effect Modifier",
#     subtitle = "Controlled for Age",
#     x = "Childhood Weight Perception",
#     y = "Predicted Life Satisfaction (0-10)")




#Model 3: + current BMI + parental body image----
# Model 3: 3-way Interaction
library(dplyr)
##models----

model3 <- prep %>%
  lm(LS_2021 ~ CWP_21 * bmi_bin * parental_large + age_2021_imputed, data = .)

summary(model3)


prep1 <- prep %>%
  mutate(
    parent_cat = case_when(
      mom_physique_2021 <= 3 & dad_physique_2021 <= 3 ~ "Both Large",
      mom_physique_2021 <= 3 | dad_physique_2021 <= 3 ~ "Only 1 Large",
      TRUE ~ "None Large"
    ),
    parent_cat = factor(parent_cat, levels = c("None Large", "Only 1 Large", "Both Large"))
  )

model3_pl <- prep1 %>%
  lm(LS_2021 ~ CWP_21 * bmi_bin * parent_cat + age_2021_imputed, data = .)

summary(model3_pl)

##visuals: 3-way----
library(ggeffects)
library(ggplot2)

###using parental_large----
# 1. Calculate the predicted values for all combinations
# x = CWP_21, group = bmi_bin, facet = parental_cat
plot_data <- predict_response(model3, 
                              terms = c("CWP_21", "bmi_bin", "parental_large"))

# 2. Create the "Final Story" Plot
ggplot(plot_data, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.5) +
  # Faceting by Parental Body Size creates the "Two Worlds" comparison
  facet_wrap(~facet) + 
  theme_minimal() +
  scale_color_manual(values = c("Healthy" = "#2166ac", "Obese" = "#b2182b")) +
  labs(
    title = "The Cumulative Impact on Life Satisfaction",
    subtitle = "Faceted by Parental Body Size | Adjusted for Age\n",
    x = "Childhood Weight Perception",
    y = "Predicted Life Satisfaction (0-10)",
    color = "Current BMI"
  ) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(lineheight = 1.2, face = "italic"),
    strip.text = element_text(size = 12, face = "bold") # Titles for the panels
  )

###using parent_cat----
plot_data_pl <- predict_response(model3_pl, 
                              terms = c("CWP_21", "bmi_bin", "parent_cat"))

# 2. Create the "Final Story" Plot
ggplot(plot_data_pl, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.5) +
  # Faceting by Parental Body Size creates the "Two Worlds" comparison
  facet_wrap(~facet) + 
  theme_minimal() +
  scale_color_manual(values = c("Healthy" = "#2166ac", "Obese" = "#b2182b")) +
  labs(
    title = "The Cumulative Impact on Life Satisfaction",
    subtitle = "Faceted by Parental Body Size | Adjusted for Age\n",
    x = "Childhood Weight Perception",
    y = "Predicted Life Satisfaction (0-10)",
    color = "Current BMI"
  ) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(lineheight = 1.2, face = "italic"),
    strip.text = element_text(size = 12, face = "bold") # Titles for the panels
  )


#confounder control----
##parental_large----
model3_controlled <- prep %>%
  lm(LS_2021 ~ CWP_21 * bmi_bin * parental_large + 
       age_2021_imputed + 
       mom_physique_2021 + dad_physique_2021, # Controlling for raw parental physique
     data = .)

summary(model3_controlled)

##parent_cat----
model3_controlled_pl <- prep1 %>%
  lm(LS_2021 ~ CWP_21 * bmi_bin * parent_cat + 
       age_2021_imputed + 
       mom_physique_2021 + dad_physique_2021, # Controlling for raw parental physique
     data = .)
summary(model3_controlled_pl)



##visual----
###parental_large----
library(sjPlot)
library(ggplot2)

# This plots the ADJUSTED predicted values across the 3 panels
plot_model(model3_controlled, 
           type = "pred", 
           terms = c("CWP_21", "bmi_bin", "parental_large"),
           title = "Adjusted Life Satisfaction: The Triple Interaction",
           legend.title = "Current BMI") +
  geom_line(linewidth = 1.2) +
    theme_minimal() +
  scale_color_manual(values = c("Healthy" = "#2166ac", "Obese" = "#b2182b")) +
  labs(subtitle = "Adjusted for Age & Parental Physique | N = 19,058 nurses",
       y = "Predicted Life Satisfaction (0-10)")

###parent_cat----
plot_model(model3_controlled_pl, 
           type = "pred", 
           terms = c("CWP_21", "bmi_bin", "parent_cat"),
           title = "Adjusted Life Satisfaction: The Triple Interaction",
           legend.title = "Current BMI") +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Healthy" = "#2166ac", "Obese" = "#b2182b")) +
  labs(subtitle = "Adjusted for Age & Parental Physique | N = 19,058 nurses",
       y = "Predicted Life Satisfaction (0-10)")


