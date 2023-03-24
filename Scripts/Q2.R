# Load the necessary libraries
library(data.table)
library(ggplot2)

# Read the data into R
stock_returns <- fread("Data/Q2.csv")

# Manipulate Data ---------------------------------------------------------
# Store the investment categories and investment types
investment_categories <- as.character(unlist(stock_returns[1, ]))[-1]
investment_types <- as.character(unlist(stock_returns[2, ]))[-1]
stock_returns <- stock_returns[-c(1, 2)]
colnames(stock_returns) <- c("Date", as.vector(investment_types))

# Convert the data.table to long format using melt()
stock_returns_melt <- melt(stock_returns, id.vars = "Date", variable.name = "Investment", value.name = "Return")
stock_returns_melt[, Investment_Category := investment_categories[Investment], by = Investment]
stock_returns_melt <- stock_returns_melt[, .(Investment, Investment_Category, Date, Return)][, Return := as.numeric(Return)]

# Part I ------------------------------------------------------------------
# Calculate mean return
mean_returns <- stock_returns_melt[, .(Mean_Return = mean(Return)), by = .(Investment, Investment_Category)]
sd_returns <- stock_returns_melt[, .(SD_Return = sd(Return)), by = .(Investment, Investment_Category)]
risk_free_rate <- mean_returns[Investment == "US Riskfree", Mean_Return]

# Calculate the covariances and correlations among risky assets
risky_assets <- stock_returns_melt[Investment != "US Riskfree"]
risky_assets_wide <- dcast(risky_assets, Date ~ Investment, value.var = "Return")

# Remove the 'Date' column
risky_assets_wide[, Date:=NULL]

# Compute the covariance matrix
cov_matrix <- cov(risky_assets_wide)

# Compute the correlation matrix
cor_matrix <- cor(risky_assets_wide)

print(mean_returns)
print(sd_returns)
print(risk_free_rate)
print(cov_matrix)
print(cor_matrix)

# Part II ------------------------------------------------------------------

# Part A ------------------------------------------------------------------

# Filter the data to keep only the country portfolios
country_portfolios <- risky_assets[Investment_Category == "Country Port"]

# Calculate the mean returns and covariance matrix for the country portfolios
mean_returns <- country_portfolios[, .(Mean_Return = mean(Return)), by = Investment]
cov_matrix <- cov(country_portfolios[, dcast(.SD, Date ~ Investment, value.var = "Return")][, Date:=NULL])

A <- t(mean_returns$Mean_Return) %*% solve(cov_matrix) %*% mean_returns$Mean_Return
B <- t(rep(1, 6)) %*% solve(cov_matrix) %*% mean_returns$Mean_Return
C <- t(rep(1, 6)) %*% solve(cov_matrix) %*% rep(1,6)
D <- A * C - B^2

# Define the function for the portfolio variance
portfolio_variance <- function(E_Rp) {
  return((C * E_Rp^2 - 2 * B * E_Rp + A) / D)
}

# Define the range of expected returns
E_Rp_range <- seq(min(mean_returns$Mean_Return), max(mean_returns$Mean_Return), length.out = 500)

# Calculate the portfolio variances
portfolio_variances <- sapply(E_Rp_range, portfolio_variance)

# Calculate the portfolio standard deviations (Sigmas)
portfolio_sigmas <- sqrt(portfolio_variances)

# Create a data frame with the expected returns and portfolio standard deviations
efficient_frontier_data <- data.frame(E_Rp = E_Rp_range, Sigma = portfolio_sigmas)


# Part B ------------------------------------------------------------------

# Find GMVP
GMVP_Return <- B/C
GMVP_Var <- 1/C
GMVP_SD <- 1/sqrt(C)

# Plot Part II
q2plot1 <- ggplot() +
  geom_point(data = efficient_frontier_data, aes(x = Sigma, y = E_Rp), size = 0.5, color = "blue", alpha = 0.5) +
  geom_point(aes(x = GMVP_SD, y = GMVP_Return), color = "red", size = 4) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Efficient Frontier and Tangency Portfolio") +
  theme_minimal()

print(plot)

# Part III ------------------------------------------------------------------

# Part A ------------------------------------------------------------------

# Find CML
optimal_return <- (A-risk_free_rate*B)/(B-risk_free_rate*C)
optimal_sd <- ((C*optimal_return^2-2*B*optimal_return+A)/D)^0.5

cml_slope <- (D*optimal_sd)/(C*optimal_return-B)
cml_intercept <- risk_free_rate

cml_x <- seq(0, max(efficient_frontier_data$Sigma), 0.01)
cml_y <- cml_intercept + as.vector(cml_slope) * cml_x

cml_df <- data.frame(Return = cml_y, SD = cml_x)

# Graph CML
q2plot2 <- ggplot() +
  geom_point(data = efficient_frontier_data, aes(x = Sigma, y = E_Rp), size = 0.5, color = "blue", alpha = 0.5) +
  geom_point(aes(x = GMVP_SD, y = GMVP_Return), color = "red", size = 4) +
  geom_line(data = cml_df, aes(x = SD, y = Return), color = "green", size = 1) +
  # geom_line(data = sml_df, aes(x = SD, y = Return), color = "blue", size = 1) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Efficient Frontier, Tangency Portfolio, and CML") +
  theme_minimal()

print(q2plot2)

# Part B ------------------------------------------------------------------

# Find market price of risk
# The market price of risk (also known as the risk premium per unit of risk or the Sharpe ratio of the market portfolio) is a measure that quantifies the relationship between the excess return of a portfolio (i.e., the return above the risk-free rate) and the portfolio's risk, as measured by its standard deviation or volatility. It is a key concept in finance and portfolio management, as it helps investors understand the compensation they can expect to receive for taking on additional risk.
# In the context of the Capital Asset Pricing Model (CAPM), the market price of risk is represented by the equity risk premium, which is the difference between the expected return of the market portfolio and the risk-free rate. The market price of risk measures the additional return that investors require for holding a risky asset or portfolio compared to a risk-free asset, such as a Treasury bond.
# Mathematically, the market price of risk can be calculated as:
# Market Price of Risk = (Expected Return of the Portfolio - Risk-Free Rate) / Portfolio Standard Deviation
# A higher market price of risk indicates that investors are demanding a higher return for taking on additional risk. Conversely, a lower market price of risk suggests that investors are willing to accept a lower return for bearing the same level of risk. The market price of risk is an important concept for understanding the trade-off between risk and return, and it is widely used in portfolio optimization and risk management.

# Part C ------------------------------------------------------------------

# Find weights
optimal_weights <- (solve(cov_matrix) %*% matrix(c(mean_returns$Mean_Return, rep(1,nrow(mean_returns))), nrow=nrow(mean_returns)) %*% 
  matrix(c(C, -B, -B, A), nrow=2) %*% matrix(c(optimal_return, 1), nrow = 2))/rep(D, 6)

# Part D ------------------------------------------------------------------

portfolio_options <- data.table(`Risky Portfolio` = c(0, 300000, 500000), `Risk Free Portfolio` = c(500000, 200000, 0))

portfolio_options[, `:=`(Return = `Risky Portfolio`*(1+optimal_return[1])+`Risk Free Portfolio`*(1+risk_free_rate), 
             SD = sqrt(`Risky Portfolio`^2*optimal_sd[1]^2))]

# Part E ------------------------------------------------------------------
# Find betas
mean_returns[, Beta:=(Mean_Return-risk_free_rate)/(optimal_return[1] - risk_free_rate)]

# Find sml line
sml_slope <- optimal_return - risk_free_rate
sml_intercept <- risk_free_rate

sml_x <- seq(0, max(efficient_frontier_data$Sigma), 0.01)
sml_y <- sml_intercept + as.vector(sml_slope) * cml_x

sml_df <- data.frame(Return = sml_y, SD = sml_x)

# Plot line
q2plot2 <- ggplot() +
  geom_line(data = sml_df, aes(x = SD, y = Return), color = "blue", size = 1) +
  labs(x = "Portfolio Standard Deviation (Sigma)", y = "Portfolio Expected Return (E(R))",
       title = "Efficient Frontier, Tangency Portfolio, and CML") +
  theme_minimal()

# Australia Beta
mean_returns[Investment=="Australia", Beta]
# This is the systemic risk of the asset which cannot be diversified away

# Part F ------------------------------------------------------------------
return = risk_free_rate + (0.0045/optimal_sd^2)*(optimal_return - risk_free_rate)
