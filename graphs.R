#WEIGHT----
##HES----
###histogram, 2021 weight, HES females only) ----
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
  scale_x_continuous(limits = c(40, 95), breaks = seq(40, 95, by=10)) +
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




###histogram, 2024 females ----
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
    title = "2024 weight",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()





###histogram, 2024, total----
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




###histogram, 2021, 25yo+ ----
ggplot(v3, aes(x=w24_hes)) +
  # Histogram
  geom_histogram(
    binwidth = 2, 
    fill = "antiquewhite", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(
    aes(y = after_stat(density) * n21_Agefilter * 2, color = "2021 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(50, 110), breaks = seq(50, 110, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2021 weight",
    values = c("2021 weight" = "#00897B")
  ) + 
  labs(
    title = "2021 weight, HES interval, 25+",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()




###histogram, 2024, 25yo+ ----
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
    aes(y = after_stat(density) * n24_Agefilter * 2, color = "2024 weight"), 
    linewidth = 1, 
    adjust=1.5,
    na.rm = TRUE
  ) +
  # Manual Axis Limits (Adjust these to fit your data)
  scale_x_continuous(limits = c(40, 110), breaks = seq(40, 110, by=10)) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(
    name = "2024 weight",
    values = c("2024 weight" = "#00897B")
  ) + 
  labs(
    title = "2024 weight, HES interval, 25+",
    x = "Weight (kg)",
    y = "Number of Participants"
  ) +
  theme_minimal()


