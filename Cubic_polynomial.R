# Load necessary libraries
pacman::p_load(readxl, dplyr, ggplot2, pracma, magrittr, rootSolve)

# Set working directory (replace with your directory path)
setwd("your_directory_path")

# Load dataset (replace with your file path)
data <- read_xlsx("your_file_path.xlsx")

# View the first few rows of the dataset
head(data)

# Fit 2nd and 3rd degree polynomial models
cubic_model <- lm(response_variable ~ predictor_variable + I(predictor_variable^2) + I(predictor_variable^3), data = data)
summary(cubic_model)

quadratic_model <- lm(response_variable ~ predictor_variable + I(predictor_variable^2), data = data)
summary(quadratic_model)

# Calculate AIC and BIC values for model comparison
AIC_BIC <- c(AIC_cubic = AIC(cubic_model), BIC_cubic = BIC(cubic_model),
             AIC_quadratic = AIC(quadratic_model), BIC_quadratic = BIC(quadratic_model))
print(AIC_BIC)  # Select the model with the lowest AIC and BIC values

# Function to calculate standard error
se <- function(x) {
  return(sd(x) / sqrt(length(x)))
}

# Calculate standard error and prepare data for plotting
data_processed <- data.frame(
  predictor = data$predictor_variable,
  response = data$response_variable,
  response_se = (data$response_sd / sqrt(15))  # Standard error = sd / sqrt(n)
)

# Fit the cubic polynomial model
cubic_poly_model <- lm(response_variable ~ predictor_variable + I(predictor_variable^2) + I(predictor_variable^3), data = data)
summary(cubic_poly_model)

# Define the derivative function for the cubic polynomial
coef_vals <- coef(cubic_poly_model)
deriv_fun <- function(x) {
  coef_vals[2] + 2 * coef_vals[3] * x + 3 * coef_vals[4] * x^2  # Derivative of y = a + bx + cx^2 + dx^3
}

# Find critical points (where derivative = 0)
critical_points <- uniroot.all(deriv_fun, interval = c(min(data$predictor_variable), max(data$predictor_variable)))

# Evaluate the original function at critical points
g_fun <- function(x) {
  coef_vals[1] + coef_vals[2] * x + coef_vals[3] * x^2 + coef_vals[4] * x^3
}
critical_values <- sapply(critical_points, g_fun)

# Calculate the second derivative to classify critical points
second_deriv_fun <- function(x) {
  2 * coef_vals[3] + 6 * coef_vals[4] * x  # Second derivative of y = a + bx + cx^2 + dx^3
}
second_derivative_values <- sapply(critical_points, second_deriv_fun)

# Classify critical points as maxima, minima, or inflection points
critical_types <- ifelse(
  second_derivative_values > 0, "Minimum",
  ifelse(second_derivative_values < 0, "Maximum", "Inflection Point")
)

# Calculate the control value (predictor = 0)
control_value <- g_fun(0)

# Find LDS (Lowest Dose Stimulation) where the model crosses the control value
lds_fun <- function(x) {
  g_fun(x) - control_value
}
lds_points <- uniroot.all(lds_fun, interval = c(min(data$predictor_variable), max(data$predictor_variable)))

# Display results
results <- data.frame(
  Critical_X = critical_points,
  Critical_Y = critical_values,
  Second_Derivative = second_derivative_values,
  Type = critical_types
)

# Calculate Ymax (value of the function at the maximum critical point)
Ymax_Value <- g_fun(results$Critical_X[results$Type == "Maximum"])

# Calculate Mmax (percentage of Ymax relative to the control)
Mmax <- (Ymax_Value / control_value) * 100

# Display LDS points
lds_results <- data.frame(
  LDS_X = lds_points,
  LDS_Y = sapply(lds_points, g_fun)
)

# Save plot as SVG
svg("plot.svg", width = 9, height = 7, bg = "white")

# Plot data with error bars and regression lines
ggplot(data_processed, aes(x = predictor, y = response)) +
  geom_errorbar(aes(ymin = response - response_se, ymax = response + response_se), width = 1, color = "black", size = 1) +
  stat_smooth(method = "loess", se = FALSE, color = "red") +
  geom_hline(yintercept = control_value, linetype = "dashed", color = "black", size = 0.7) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3), se = TRUE, color = "#16A3CE") +
  scale_x_sqrt(breaks = c(0, 25, 50, 100, 200, 300, 400, 600, 800, 1000, 2000), expand = c(0, 1.1)) + #Dosis
  scale_y_continuous(breaks = seq(0, 35, by = 0.5), expand = c(0, 0), limits = c(1, 4)) +
  annotate("text", x = 600, y = 1.9, label = expression(y == 1 - 1%*% 10^{-4}*x + 1 %*% 10^{-7}*x^2), color = "black", size = 5) + #your estimators
  annotate("text", x = 500, y = 1.7, label = paste("y =", round(coef_vals[1], 3), "+", round(coef_vals[2], 5), "* x +", round(coef_vals[3], 7), "* x^2 +", round(coef_vals[4], 9), "* x^3"), color = "black", size = 5) +
  annotate("text", x = 500, y = 1.55, label = paste("R²-adj =", round(summary(cubic_poly_model)$adj.r.squared, 2)), color = "black", size = 5) +
  annotate("text", x = 500, y = 1.40, label = paste("Ymax =", round(Ymax_Value, 2)), color = "black", size = 5) +
  annotate("text", x = 500, y = 1.25, label = paste("Mmax =", round(Mmax, 2), "%"), color = "black", size = 5) +
  annotate("text", x = 500, y = 1.10, label = paste("LDS =", round(lds_results$LDS_X[1], 2)), color = "black", size = 5) +
  geom_text(aes(y = response + response_se + 0.1, label = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")), vjust = 0.1, color = "black", size = 6) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  labs(
    x = "Predictor Variable (units)",
    y = "Response Variable (units)"
  )

# Save plot
dev.off()
