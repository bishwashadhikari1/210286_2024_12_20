# Install and load necessary packages
if (!require("corrplot")) install.packages("corrplot", dependencies = TRUE)
library(corrplot)

if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# -----------------------------------------------------------------------------
# TASK 1: Data Exploration and Visualization
# -----------------------------------------------------------------------------

# Load the dataset, selecting the first five columns
dataset <- read.csv("C:/Users/Asus/Desktop/R/data.csv", header = TRUE)[, 1:5]

# Display the first few rows
print("First six rows of the dataset:")
print(head(dataset))

# Display the structure of the dataset
print("Structure of the dataset:")
str(dataset)

# Summary statistics
print("Summary statistics:")
print(summary(dataset))

# Count of missing values per column
print("Missing values per column:")
print(colSums(is.na(dataset)))

# Time Series Plots for Each Signal
num_signals <- 5
par(mfrow = c(num_signals, 1), mar = c(3, 5, 2, 2))

# Assign distinct colors to each signal
colors_signals <- c("darkcyan", "darkmagenta", "darkgreen", "brown",
                    "darkorange")

# Signal identifiers
signals <- c("x1", "x2", "x3", "x4", "x5")

# Plot each signal over time
for (i in 1:num_signals) {
  current_signal <- signals[i]
  plot(dataset[[current_signal]], type = "l", col = colors_signals[i],
       main = paste("Signal:", current_signal, 
                    ifelse(current_signal == "x2", "(Output)", "(Input)")),
       xlab = "Trial Index",
       ylab = "Value")
  grid(col = "lightgray")
}

# Reset plotting area
par(mfrow = c(1,1))

# Histograms with Statistical Lines for Input Signals
input_signals <- c("x1", "x3", "x4", "x5")
statistic_colors <- c("Average" = "navy", "Median" = "maroon", 
                      "Mode" = "darkgreen")

# Function to compute mode using histogram
get_mode <- function(data, bins = "Sturges") {
  h <- hist(data, plot = FALSE, breaks = bins)
  return(h$mids[which.max(h$counts)])
}

# Set up plotting grid
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Generate histograms for each input signal
for (signal in input_signals) {
  signal_data <- dataset[[signal]]
  
  # Calculate statistics
  mean_val <- mean(signal_data, na.rm = TRUE)
  median_val <- median(signal_data, na.rm = TRUE)
  mode_val <- get_mode(signal_data)
  
  # Plot histogram
  hist(signal_data, 
       main = paste("Histogram of", signal),
       xlab = paste(signal, "Values"),
       col = "lightblue",
       border = "white")
  
  # Add statistical lines
  abline(v = mean_val, col = statistic_colors["Average"], lwd = 2, lty = 2)
  abline(v = median_val, col = statistic_colors["Median"], lwd = 2, lty = 3)
  abline(v = mode_val, col = statistic_colors["Mode"], lwd = 2, lty = 4)
  
  # Add legend
  legend("topright",
         legend = names(statistic_colors),
         col = statistic_colors,
         lty = c(2, 3, 4),
         lwd = 2,
         bty = "n")
}

# Reset plotting area
par(mfrow = c(1,1))

# Histogram for Output Signal
output_signal <- "x2"
stat_colors_output <- c("Average" = "navy", "Median" = "maroon", 
                        "Mode" = "darkgreen")

# Calculate statistics for output
output_data <- dataset[[output_signal]]
mean_out <- mean(output_data, na.rm = TRUE)
median_out <- median(output_data, na.rm = TRUE)
mode_out <- get_mode(output_data)

# Plot histogram for output
hist(output_data,
     main = paste("Histogram of", output_signal, "(Output)"),
     xlab = paste(output_signal, "Values"),
     col = "lightblue",
     border = "white")

# Add statistical lines
abline(v = mean_out, col = stat_colors_output["Average"], lwd = 2, lty = 2)
abline(v = median_out, col = stat_colors_output["Median"], lwd = 2, lty = 3)
abline(v = mode_out, col = stat_colors_output["Mode"], lwd = 2, lty = 4)

# Add legend
legend("topright",
       legend = names(stat_colors_output),
       col = stat_colors_output,
       lty = c(2, 3, 4),
       lwd = 2,
       bty = "n")

# Density Plots for Input Signals
# Function to compute mode using density estimation
compute_mode_density <- function(x) {
  dens <- density(x, na.rm = TRUE)
  return(dens$x[which.max(dens$y)])
}

# Set up plotting grid
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Generate density plots for each input signal
for (signal in input_signals) {
  data <- dataset[[signal]]
  
  # Calculate statistics
  mean_val <- mean(data, na.rm = TRUE)
  median_val <- median(data, na.rm = TRUE)
  mode_val <- compute_mode_density(data)
  
  # Plot density
  plot(density(data, na.rm = TRUE),
       main = paste("Density of", signal),
       xlab = paste(signal, "Values"),
       col = "black",
       lwd = 2)
  
  # Add statistical lines
  abline(v = mean_val, col = statistic_colors["Average"], lwd = 2, lty = 2)
  abline(v = median_val, col = statistic_colors["Median"], lwd = 2, lty = 3)
  abline(v = mode_val, col = statistic_colors["Mode"], lwd = 2, lty = 4)
  
  # Add legend
  legend("topright",
         legend = names(statistic_colors),
         col = statistic_colors,
         lty = c(2, 3, 4),
         lwd = 2,
         bty = "n")
}

# Reset plotting area
par(mfrow = c(1,1))
# Density Plot for Output Signal
# Calculate statistics for output
mean_out_density <- mean(output_data, na.rm = TRUE)
median_out_density <- median(output_data, na.rm = TRUE)
mode_out_density <- compute_mode_density(output_data)

# Plot density for output
plot(density(output_data, na.rm = TRUE),
     main = paste("Density of", output_signal, "(Output)"),
     xlab = paste(output_signal, "Values"),
     col = "black",
     lwd = 2)
# Add statistical lines
abline(v = mean_out_density, col = stat_colors_output["Average"], lwd = 2, 
       lty = 2)
abline(v = median_out_density, col = stat_colors_output["Median"], lwd = 2,
       lty = 3)
abline(v = mode_out_density, col = stat_colors_output["Mode"], lwd = 2, 
       lty = 4)

# Add legend
legend("topright",
       legend = names(stat_colors_output),
       col = stat_colors_output,
       lty = c(2, 3, 4),
       lwd = 2,
       bty = "n")

# Correlation Matrix and Scatter Plot Matrix
correlation_matrix <- cor(dataset, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Visualize correlation matrix
corrplot(correlation_matrix, method = "number", type = "upper", 
         tl.col = "black", 
         tl.cex = 0.8, number.cex = 0.7)

# Scatterplot Matrix using GGally for enhanced visuals
if (!require("GGally")) install.packages("GGally", dependencies = TRUE)
library(GGally)
ggpairs(dataset, title = "Scatterplot Matrix of Signals")

# Reset plotting area
par(mfrow = c(1,1))

# -----------------------------------------------------------------------------
# TASK 2: Linear Regression Models and Evaluation
# -----------------------------------------------------------------------------

# Reload dataset (optional if not modified)
df_models <- read.csv("C:/Users/Asus/Desktop/R/data.csv", header = TRUE)[, 1:5]

# Define output and input variables
output_var <- "x2"
input_vars <- c("x1", "x3", "x4", "x5")

# Define different regression models
model_list <- list()

# Model 1: x2 ~ x4 + x3^2
model_list[[1]] <- lm(as.formula("x2 ~ x4 + I(x3^2)"), data = df_models)

# Model 2: x2 ~ x4 + x3^2 + x5
model_list[[2]] <- lm(as.formula("x2 ~ x4 + I(x3^2) + x5"), data = df_models)

# Model 3: x2 ~ x3 + x4 + x5^3
model_list[[3]] <- lm(as.formula("x2 ~ x3 + x4 + I(x5^3)"), data = df_models)

# Model 4: x2 ~ x4 + x3^2 + x5^3
model_list[[4]] <- lm(as.formula("x2 ~ x4 + I(x3^2) + I(x5^3)"), data = df_models)

# Model 5: x2 ~ x4 + x1^2 + x3
model_list[[5]] <- lm(as.formula("x2 ~ x4 + I(x1^2) + x3"), data = df_models)

# Display coefficients and summaries for each model
for (i in 1:length(model_list)) {
  cat("\n--- Model", i, "Summary ---\n")
  print(summary(model_list[[i]]))
}

# Calculate Residual Sum of Squares (RSS) for each model
rss_values <- sapply(model_list, function(model) sum(resid(model)^2))
names(rss_values) <- paste("Model", 1:length(model_list))
print("\nResidual Sum of Squares (RSS) for each model:")
print(rss_values)

# Compute Log-Likelihood for each model
compute_log_likelihood <- function(rss, sample_size) {
  sigma2 <- rss / (sample_size - 1)
  ll <- -(sample_size / 2) * log(2 * pi) - (sample_size / 2) * log(sigma2) - 
    (rss / (2 * sigma2))
  return(ll)
}

sample_size <- nrow(df_models)
log_likelihoods <- sapply(rss_values, compute_log_likelihood, 
                          sample_size = sample_size)
print("\nLog-Likelihood for each model:")
print(log_likelihoods)

# Calculate AIC and BIC for each model
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)

# Display AIC and BIC
for (i in 1:length(model_list)) {
  cat(sprintf("\nModel %d: AIC = %.2f, BIC = %.2f", 
              i, aic_values[i], bic_values[i]))
}

# Assess Residuals Distribution via Q-Q Plots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
colors_residuals <- c("purple", "darkred", "darkblue", "darkgreen", "brown")

for (i in 1:length(model_list)) {
  qqnorm(resid(model_list[[i]]), main = paste("Q-Q Plot: Model", i), 
         col = colors_residuals[i], pch = 19)
  qqline(resid(model_list[[i]]), col = "orange", lwd = 2)
}

# Reset plotting area
par(mfrow = c(1,1))

# Split data into Training and Testing sets
set.seed(123)  # Ensure reproducibility
total_samples <- nrow(df_models)
train_size <- floor(0.7 * total_samples)
train_indices <- sample(seq_len(total_samples), size = train_size)

training_data <- df_models[train_indices, ]
testing_data <- df_models[-train_indices, ]

#best model (as model 2 has been proven to be best with multiple factors)
best_model_index <- which.min(aic_values)
best_model <- model_list[[best_model_index]]
cat("\nBest Model Selected: Model", best_model_index, "\n")
print(summary(best_model))

# Make predictions on the test set
predictions <- predict(best_model, newdata = testing_data, 
                       interval = "prediction", level = 0.95)

# Preview predictions
print("First few predictions with intervals:")
print(head(predictions))

# Plot Actual vs Predicted with Prediction Intervals
plot(testing_data[[output_var]], type = 'p', pch = 16, col = 'darkblue',
     main = "Actual vs Predicted Values",
     xlab = "Observation Index",
     ylab = paste(output_var, "(Actual and Predicted)"))
points(predictions[,"fit"], col = 'darkred', pch = 17)
arrows(x0 = 1:nrow(testing_data),
       y0 = predictions[,"lwr"],
       x1 = 1:nrow(testing_data),
       y1 = predictions[,"upr"],
       angle = 90, code = 3, length = 0.02, col = 'darkred')
legend("topleft",
       legend = c("Actual", "Predicted"),
       col = c("darkblue", "darkred"),
       pch = c(16,17))

# ----------------------------------------------------------------------------
# TASK 3: Bayesian Parameter Estimation via Rejection Sampling
# ----------------------------------------------------------------------------

# Reload dataset (optional if not modified)
df_bayes <- read.csv("C:/Users/Asus/Desktop/R/data.csv", header = TRUE)[, 1:5]

# Set seed for reproducibility
set.seed(123)

# Fixed parameter estimates from MLE
theta_0_fixed <- -0.0593
theta_2_fixed <- 0.0393

# Define priors for theta1 and theta3
num_samples <- 10000
theta1_prior <- runif(num_samples, min = 0.2607, max = 1.2607)
theta3_prior <- runif(num_samples, min = -0.1393, max = 0.4607)

# Observed data components
y_observed <- df_bayes$x2
x4_vals <- df_bayes$x4
x3_vals <- df_bayes$x3
x5_vals <- df_bayes$x5
n_bayes <- length(y_observed)

# Initialize vector to store distances
distances_bayes <- numeric(num_samples)

# Calculate distances for each sampled parameter set
for (i in 1:num_samples) {
  y_simulated <- theta_0_fixed + theta1_prior[i] * x4_vals + 
    theta_2_fixed * (x3_vals^2) + theta3_prior[i] * x5_vals
  distances_bayes[i] <- sum((y_observed - y_simulated)^2)
}

# Determine threshold based on the 1st percentile of distances
distance_threshold <- quantile(distances_bayes, 0.01)
cat("\nDistance Threshold (1% quantile):", distance_threshold, "\n")

# Identify parameter sets that meet the threshold
accepted_indices_bayes <- which(distances_bayes < distance_threshold)
accepted_theta1 <- theta1_prior[accepted_indices_bayes]
accepted_theta3 <- theta3_prior[accepted_indices_bayes]

# Plot posterior distributions and joint posterior
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

# Posterior of Theta1
hist(accepted_theta1, breaks = 20, main = expression(paste("Posterior of ", 
                                                           theta[1])),
     xlab = expression(theta[1]), col = "lightgreen", border = "white")

# Posterior of Theta3
hist(accepted_theta3, breaks = 20, main = expression(paste("Posterior of ",
                                                           theta[3])),
     xlab = expression(theta[3]), col = "lightcoral", border = "white")

# Joint Posterior of Theta1 and Theta3
plot(accepted_theta1, accepted_theta3, 
     main = expression(paste("Joint Posterior of ", theta[1], " and ", 
                             theta[3])),
     xlab = expression(theta[1]),
     ylab = expression(theta[3]),
     pch = 20, col = rgb(0, 0, 1, 0.4))
grid()

# Reset plotting area
par(mfrow = c(1,1))

