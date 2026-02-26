#SYNTAX
if_else(
  #condition, value_if_true, value_if_false
  )

case_when (
  #first condition 
  &
  #second condition...
  ~ #from which column the data is pulled
  TRUE ~ NA_real_ #what to put if conditions above don't apply
)


#ggplot plotting a HISTOGRAM
# --- REUSABLE DISTRIBUTION PLOT BLOCK ---
plot_distribution <- function(data, var_name, bin_w = 2, smooth = 1.5, color_hex = "lightblue") {
  
  n_obs <- nrow(data)
  
  ggplot(data, aes(x = .data[[var_name]])) +
    # 1. The Bars
    geom_histogram(
      binwidth = bin_w, 
      fill = "antiquewhite", 
      color = "white", 
      alpha = 0.7
    ) +
    # 2. The Density Line (The math block you should memorize)
    geom_density(
      aes(y = after_stat(density) * n_obs * bin_w), 
      color = color_hex, 
      linewidth = 1, 
      adjust = smooth
    ) +
    # 3. Formatting
    scale_y_continuous(labels = scales::comma, breaks = scales::breaks_width(1500)) +
    theme_minimal() +
    labs(x = var_name, y = "Count")
}

# How to use it:
# plot_distribution(v3, "height_2021", bin_w = 2, smooth = 1.5)
# smooth the curve: adjust = , e.g.:1.5



#FORMAT the output as a table from summary statistics
  #strip summary() class/attributes
  sv <- function(x) unclass(summary(x))
  #create data frame with summary statistics
  a <- data.frame(
    xxx    = sv(),
    check.names = FALSE  #Check names to comply to specific rules
  )
library(knitr)
kable(
  #format,
  #digits,
  #row names,
  #column names,
  #caption,
  #align: l=left, c=center, r=right,
  )
