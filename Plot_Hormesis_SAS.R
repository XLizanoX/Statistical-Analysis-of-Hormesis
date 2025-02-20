# Load necessary libraries using pacman
pacman::p_load(readxl, dplyr, ggplot2, pracma, magrittr, rootSolve)

# Load data
setwd("your_directory_path")  # Set your working directory
data <- read_xlsx("your_file_path.xlsx")  # Load your dataset

# Prepare data for plotting
data_processed <- data.frame(
  Tr = data$predictor_variable,  # Replace with your predictor variable name
  response = data$response_variable,  # Replace with your response variable name
  response_se = (data$response_sd / sqrt(15))  # Standard error = sd / sqrt(n)
  
  # Define the model function (e.g., Cedergreen or Brain-Cousens)
  model_function <- function(Tr, a, b, d, e, f) {
    (d + f * exp(-1 / (Tr^a))) / (1 + exp(b * log(Tr / e)))  # Replace with your model formula
  }
  
  # Define parameters and standard errors from SAS results
  params <- data.frame(
    param = c("a", "b", "d", "e", "f"),
    estimate = c(0.3462, 4.9534, 8.3386, 1665.75, 3.0962),  # Replace with your parameter estimates
    se = c(0.2557, 1.0867, 0.6131, 84.9200, 1.4022)  # Replace with your standard errors
  )
  
  # Generate predictions for the model
  Tr2 <- seq(min(data_processed$Tr), max(data_processed$Tr), length.out = 500)  # Generate a smooth range of predictor values
  predicted <- model_function(Tr2, params$estimate[1], params$estimate[2], params$estimate[3], params$estimate[4], params$estimate[5])
  pred_min <- model_function(Tr2, params$estimate[1] - params$se[1], params$estimate[2] - params$se[2], params$estimate[3] - params$se[3], params$estimate[4] - params$se[4], params$estimate[5] - params$se[5])
  pred_max <- model_function(Tr2, params$estimate[1] + params$se[1], params$estimate[2] + params$se[2], params$estimate[3] + params$se[3], params$estimate[4] + params$se[4], params$estimate[5] + params$se[5])
  
  # Calculate Ymax and Mmax
  Control <- params$estimate[3]  # Control value (d parameter)
  Ymax_value <- max(predicted)  # Maximum response value
  ymax <- Ymax_value / Control * 100  # Ymax as a percentage of control
  
  # Create plot data for observed data
  plot_data_observed <- data.frame(
    Tr = data_processed$Tr,
    response = data_processed$response,
    response_se = data_processed$response_se
  )
  
  # Create plot data for predicted data
  plot_data_predicted <- data.frame(
    Tr2 = Tr2,
    predicted = predicted,
    pred_min = pred_min,
    pred_max = pred_max
  )
  
  # Save plot as SVG
  svg("hormesis_plot.svg", width = 9, height = 7, bg = "white")
  
  # Create the plot
  ggplot() +
    # Observed data with error bars
    geom_errorbar(
      data = plot_data_observed,
      aes(x = Tr, ymin = response - response_se, ymax = response + response_se),
      width = 0.1, color = "black", size = 1
    ) +
    geom_point(
      data = plot_data_observed,
      aes(x = Tr, y = response), color = "darkred", size = 3, alpha = 0.7
    ) +
    
    # Reference line (control value)
    geom_hline(yintercept = Control, linetype = "dashed", color = "black", size = 0.7) +
    
    # Model predictions and confidence interval
    geom_line(
      data = plot_data_predicted,
      aes(x = Tr2, y = predicted), color = "#EB6500", size = 1
    ) +
    geom_ribbon(
      data = plot_data_predicted,
      aes(x = Tr2, ymin = pred_min, ymax = pred_max),
      fill = "#EB6500", alpha = 0.1
    ) +
    
    # Highlight Ymax and Mmax
    geom_vline(xintercept = 477.56, linetype = "dashed", color = "grey", size = 0.7) +  # Replace with your Mmax value
    annotate("text", x = 477.56, y = max(predicted), label = paste("Ymax (", round(ymax, 2), "%)"), color = "black", size = 5, fontface = "bold") +
    
    # Highlight LDS
    geom_vline(xintercept = 1341.22, linetype = "dashed", color = "#16A3CE", size = 0.7) +  # Replace with your LDS value
    annotate("text", x = 1341.22, y = max(predicted) * 0.9, label = "LDS", color = "black", size = 5, fontface = "bold") +
    
    # Add parameter estimates
    annotate("text", x = max(Tr2) * 0.1, y = max(predicted) * 0.8, label = paste("a =", round(params$estimate[1], 2), "±", round(params$se[1], 2)), color = "black", size = 5) +
    annotate("text", x = max(Tr2) * 0.1, y = max(predicted) * 0.75, label = paste("b =", round(params$estimate[2], 2), "±", round(params$se[2], 2)), color = "black", size = 5) +
    annotate("text", x = max(Tr2) * 0.1, y = max(predicted) * 0.7, label = paste("d =", round(params$estimate[3], 2), "±", round(params$se[3], 2)), color = "black", size = 5) +
    annotate("text", x = max(Tr2) * 0.1, y = max(predicted) * 0.65, label = paste("e =", round(params$estimate[4], 2), "±", round(params$se[4], 2)), color = "black", size = 5) +
    annotate("text", x = max(Tr2) * 0.1, y = max(predicted) * 0.6, label = paste("f =", round(params$estimate[5], 2), "±", round(params$se[5], 2)), color = "black", size = 5) +
    
    # Scales and themes
    scale_y_continuous(breaks = seq(0, max(predicted) * 1.2, by = 2), expand = c(0, 0)) +
    scale_x_log10(breaks = c(0, 25, 50, 100, 200, 400, 800, 2000), expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 10, color = "black"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    ) +
    labs(
      x = expression("Predictor Variable (" * mu * "M)"),  # Replace with your predictor label
      y = "Response Variable (units)"  # Replace with your response label
    )
  
  # Save plot
  dev.off()