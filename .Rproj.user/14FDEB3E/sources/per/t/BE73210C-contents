# Load the necessary libraries
library(stats)
library(pracma)
library(ggplot2)
library(data.table)
library(dplyr)
library(scales)

# Part A ------------------------------------------------------------------
# Define mean returns, standard deviations, and correlation
mean_return1 <- 0.015
mean_return2 <- 0.008
risk_free_rate <- 0.004
sd1 <- 0.1
sd2 <- 0.05
correlation <- 0.4
covariance <- correlation * sd1 * sd2
cov_matrix <- matrix(c(sd1^2, covariance, covariance, sd2^2), nrow=2)
inverse_cov_matrix <- inv(cov_matrix)

# Calculate the expected return and standard deviation for the tangency (optimal) portfolio
excess_returns <- c(mean_return1 - risk_free_rate, mean_return2 - risk_free_rate)
optimal_weights <- inverse_cov_matrix %*% excess_returns / sum(inverse_cov_matrix %*% excess_returns)
tangency_return <- sum(optimal_weights * c(mean_return1, mean_return2))
tangency_sd <- sqrt(t(optimal_weights) %*% cov_matrix %*% optimal_weights)

# Calculate the Sharpe Ratio (slope) for the tangency portfolio
tangency_sharpe_ratio <- (tangency_return - risk_free_rate) / tangency_sd

# Define the capital allocation line (CAL)
cal_slope <- tangency_sharpe_ratio
cal_intercept <- risk_free_rate

# Create a grid of portfolio weights
weights <- c(-5, -1.5, seq(-0.8, 1.2, 0.1), 2.5, 7.5)

# Create a data.table with all possible portfolio_weights
portfolio_weights_dt <- data.table(weight_asset1 = weights, weight_asset2 = 1 - weights, Correlation = rep(correlation <- c(-1, 0, 0.4, 0.5, 1), each = length(weights)))

# Covariance matrix
cov_matrix_gen <- function(correlation, sd1, sd2) {
  covariance <- correlation * sd1 * sd2
  cov_matrix <- matrix(c(sd1^2, covariance, covariance, sd2^2), nrow=2)  
}

# Calculate the expected return and standard deviation for all possible portfolios
portfolio_stats <- portfolio_weights_dt[, .(Return = weight_asset1 * mean_return1 + weight_asset2 * mean_return2,
                                            SD = sqrt(c(weight_asset1, weight_asset2) %*% cov_matrix_gen(Correlation, sd1, sd2) %*% c(weight_asset1, weight_asset2))[1]),
                                        by = .(weight_asset1, weight_asset2, Correlation)]

current_portfolio <- portfolio_stats[Correlation==0.4, .(weight_asset1, weight_asset2, Return, SD)] %>% copy()

# Calculate the CAL line
cal_x <- seq(0, max(current_portfolio$SD), 0.01)
cal_y <- cal_intercept + as.vector(cal_slope) * cal_x

# Create a data frame with the calculated values
cal_df <- data.frame(Return = cal_y, SD = cal_x)

# Create the plot using ggplot2
q1plot1 <- ggplot() +
  geom_point(data = current_portfolio, aes(x = SD, y = Return), size = 0.5, color = "blue", alpha = 0.5) +
  geom_point(aes(x = tangency_sd, y = tangency_return), color = "red", size = 2) +
  geom_line(data = cal_df, aes(x = SD, y = Return), color = "green", size = 1) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Investment Options, Tangency Portfolio, and CAL") +
  theme_minimal()

# Table of values
portfolio_stats_df <- current_portfolio %>%
  as.data.frame() %>%
  mutate(across(starts_with("weight"), percent),
         Return = percent(Return, accuracy = 0.01),
         SD = percent(SD, accuracy = 0.1))

# Part B ------------------------------------------------------------------
# Create a plot for multiple portfolios
portfolio_stats$Correlation <- as.factor(portfolio_stats$Correlation)

# Updated ggplot code
q1plot2 <- ggplot() +
  geom_point(data = portfolio_stats[Correlation != 0.4], aes(x = SD, y = Return, color = Correlation, group = Correlation), alpha = 0.5) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Investment options given different SDs") +
  theme_minimal() +
  scale_color_discrete(name = "Correlation Parameter")


