# Load the necessary libraries
library(stats)
library(pracma)
library(ggplot2)
library(data.table)
library(dplyr)
library(scales)

# Define mean returns, standard deviations, and correlation
mean_return1 <- 0.015
mean_return2 <- 0.008
sd1 <- 0.1
sd2 <- 0.05
correlation <- 0.4

# Calculate the covariance
covariance <- correlation * sd1 * sd2

# Define the risk-free rate (assumed, use appropriate value)
risk_free_rate <- 0.004

# Calculate optimal weights for the tangency portfolio using Sharpe Ratio
cov_matrix <- matrix(c(sd1^2, covariance, covariance, sd2^2), nrow=2)
inverse_cov_matrix <- inv(cov_matrix)

mean_returns <- c(0.015, 0.008)
A <- t(mean_returns) %*% solve(cov_matrix) %*% mean_returns
B <- t(rep(1, 2)) %*% solve(cov_matrix) %*% mean_returns
C <- t(rep(1, 2)) %*% solve(cov_matrix) %*% rep(1,2)
D <- A * C - B^2

tangency_return <- (risk_free_rate*B-A)/(risk_free_rate*C-B)
tangency_sd <- ((C*tangency_return^2-2*B*tangency_return+A)/D)^0.5

## ChatGPT
excess_returns <- c(mean_return1 - risk_free_rate, mean_return2 - risk_free_rate)
optimal_weights <- inverse_cov_matrix %*% excess_returns / sum(inverse_cov_matrix %*% excess_returns)

# Calculate the expected return and standard deviation for the tangency portfolio
tangency_return <- sum(optimal_weights * c(mean_return1, mean_return2))
tangency_sd <- sqrt(t(optimal_weights) %*% cov_matrix %*% optimal_weights)

# Calculate the Sharpe Ratio for the tangency portfolio
tangency_sharpe_ratio <- (tangency_return - risk_free_rate) / tangency_sd

# Define the capital allocation line (CAL)
cal_slope <- tangency_sharpe_ratio
cal_intercept <- risk_free_rate

# Create a grid of portfolio weights
weights <- c(-5, -1.5, seq(-0.8, 1.2, 0.1), 2.5, 7.5)

# Create a data.table with all possible portfolio_weights
portfolio_weights_dt <- data.table(weight_asset1 = weights, weight_asset2 = 1 - weights)

# Calculate the expected return and standard deviation for all possible portfolios
portfolio_stats <- portfolio_weights_dt[, .(Return = weight_asset1 * mean_return1 + weight_asset2 * mean_return2,
                                            SD = sqrt(c(weight_asset1, weight_asset2) %*% cov_matrix %*% c(weight_asset1, weight_asset2))[1]),
                                        by = .(weight_asset1, weight_asset2)]

# Calculate the CAL line
cal_x <- seq(0, max(portfolio_stats$SD), 0.01)
cal_y <- cal_intercept + as.vector(cal_slope) * cal_x

# Create a data frame with the calculated values
cal_df <- data.frame(Return = cal_y, SD = cal_x)

# Create the plot using ggplot2
q1plot <- ggplot() +
  geom_line(data = portfolio_stats, aes(x = SD, y = Return), size = 0.5, color = "blue", alpha = 0.5) +
  geom_point(aes(x = tangency_sd, y = tangency_return), color = "red", size = 4) +
  geom_line(data = cal_df, aes(x = SD, y = Return), color = "green", size = 1) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Efficient Frontier, Tangency Portfolio, and CAL") +
  theme_minimal()








