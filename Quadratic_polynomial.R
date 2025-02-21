# Load necessary libraries
pacman::p_load(readxl, dplyr, ggplot2, pracma, magrittr, rootSolve)

# Set working directory (replace with your directory path)
setwd("your_directory_path")

# Load dataset (replace with your file path)
data <- read_xlsx("your_file_path.xlsx")

# View the first few rows of the dataset
head(data)

# Fit a quadratic polynomial model
quadratic_model <- lm(response_variable ~ predictor_variable + I(predictor_variable^2), data = data)
summary(quadratic_model)

# Fit a cubic polynomial model
cubic_model <- lm(response_variable ~ predictor_variable + I(predictor_variable^2) + I(predictor_variable^3), data = data)
summary(cubic_model)

# Calculate AIC and BIC values for model comparison
AIC_BIC <- c(AIC_quadratic = AIC(quadratic_model), BIC_quadratic = BIC(quadratic_model),
             AIC_cubic = AIC(cubic_model), BIC_cubic = BIC(cubic_model))
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

# Extract coefficients from the quadratic model
coef_vals <- coef(quadratic_model)
intercept <- coef_vals[1]  # Intercept term
linear_coef <- coef_vals[2]  # Linear coefficient
quadratic_coef <- coef_vals[3]  # Quadratic coefficient

# Define the quadratic function
quadratic_fun <- function(x) {
  intercept + linear_coef * x + quadratic_coef * x^2
}

# Define the derivative of the quadratic function
deriv_fun <- function(x) {
  linear_coef + 2 * quadratic_coef * x
}

# Find the critical point (where the derivative is zero)
critical_point <- uniroot.all(deriv_fun, interval = c(min(data$predictor_variable), max(data$predictor_variable)))

# Evaluate the quadratic function at the critical point
critical_value <- quadratic_fun(critical_point)

# Calculate the second derivative (constant for a quadratic model)
second_deriv_value <- 2 * quadratic_coef

# Classify the critical point
critical_type <- ifelse(second_deriv_value > 0, "Minimum", "Maximum")

# Calculate the control value (y at x = 0)
control_value <- quadratic_fun(0)

# Calculate Ymax (value of the function at the critical point if it's a maximum)
if (critical_type == "Maximum") {
  Ymax_Value <- critical_value
} else {
  Ymax_Value <- NA  # No maximum if the critical point is a minimum
}

# Calculate Mmax (percentage of Ymax relative to the control)
if (!is.na(Ymax_Value)) {
  Mmax <- (Ymax_Value / control_value) * 100
} else {
  Mmax <- NA
}

# Find LDS (Lowest Dose Stimulation)
# Solve the equation quadratic_fun(x) = control_value
lds_fun <- function(x) {
  quadratic_fun(x) - control_value
}

# Find the roots of the equation
lds_points <- uniroot.all(lds_fun, interval = c(min(data$predictor_variable), max(data$predictor_variable)))

# Filter points within the data range
lds_points <- lds_points[lds_points >= min(data$predictor_variable) & lds_points <= max(data$predictor_variable)]

# Identify the LDS (the lowest point where it crosses the control value)
if (length(lds_points) > 0) {
  lds <- min(lds_points)
  lds_value <- quadratic_fun(lds)
} else {
  lds <- NA
  lds_value <- NA
}

# Display results
results <- data.frame(
  Critical_X = critical_point,
  Critical_Y = critical_value,
  Second_Derivative = second_deriv_value,
  Type = critical_type,
  Control_Value = control_value,
  Ymax_Value = Ymax_Value,
  Mmax = Mmax,
  LDS_X = lds,
  LDS_Y = lds_value
)

print(results)

# Save plot as SVG
svg("Plot.svg", width = 9, height = 7, bg = "white")

# Plot data with error bars and regression lines
ggplot(data_processed, aes(x = predictor, y = response)) +
  geom_errorbar(aes(ymin = response - response_se, ymax = response + response_se), width = 0.5, color = "black", size = 0.1) +
  stat_smooth(method = "loess", se = FALSE, color = "red") +
  geom_hline(yintercept = control_value, linetype = "dashed", color = "black", size = 0.7) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  stat_smooth(method = lm, formula = y ~ x + I(x^2), se = TRUE, color = "#16A3CE") +
  scale_x_sqrt(breaks = seq(0, 2000, by = 200), expand = c(0, 1.1)) +
  scale_y_continuous(breaks = seq(0, 35, by = 0.2), expand = c(0, 0), limits = c(1, 2)) +
  annotate("text", x = 600, y = 1.9, label = paste("y =", round(intercept, 3), "-", round(linear_coef, 5), "* x +", round(quadratic_coef, 7), "* x^2"), color = "black", size = 5) +
  annotate("text", x = 600, y = 1.85, label = paste("R²-adj =", round(summary(quadratic_model)$adj.r.squared, 2)), color = "black", size = 5) +
  annotate("text", x = 600, y = 1.8, label = paste("Ymax =", round(Ymax_Value, 2)), color = "black", size = 5) +
  annotate("text", x = 600, y = 1.75, label = paste("Mmax =", round(Mmax, 2)), color = "black", size = 5) +
  annotate("text", x = 600, y = 1.7, label = paste("LDS =", round(lds, 2)), color = "black", size = 5) +
  geom_text(aes(y = response + response_se, label = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a")), vjust = 0.1, color = "black", size = 6) +
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
