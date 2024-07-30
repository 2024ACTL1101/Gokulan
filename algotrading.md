"ACTL1101 Assignment Part B by Gokulan Vijeyakumar 2024 T2"


output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Calculating daily returns
df$AMD_Return <- (df$AMD / lag(df$AMD) - 1)
df$GSPC_Return <- (df$GSPC / lag(df$GSPC) - 1)
df <- df %>% drop_na()
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
df$Daily_Risk_Free_Rate <- (1 + df$RF / 100)^(1 / 360) - 1
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$AMD_Excess_Return <- df$AMD_Return - df$Daily_Risk_Free_Rate
df$GSPC_Excess_Return <- df$GSPC_Return - df$Daily_Risk_Free_Rate
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
capm_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)
summary(capm_model)

# Finding the Beta Value
beta <- summary(capm_model)$coefficients[2, 1]  

beta
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

The found value beta, a variable which models the systematic risk, as stated above is 1.57, which means if there is a change in the market by 1%, AMD should change by 1.57%, and hence AMD is theoretically 1.57 times more volatile that the market, suggesting that there is a greater risk associated with AMD, which may be accompanied with greater return.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Plotting the Required Line
ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  labs(title = "CAPM: AMD vs. S&P 500 Excess Returns",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return") +
  theme_minimal()
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


From the analysis done below, as seen from the data the annual expected return for AMD is approximately 18.0%. The 90% prediction interval for AMD's annual expected return is between approximately -50.07% and 86.14%. This wide range reflects the significant volatility associated with AMD, indicating that while there is a potential for high or low return.




```{r pi}
# Setting Variables
beta <- coef(capm_model)
crfr <- 0.05
expannualret <- 0.133

# Number of observations
n <- nrow(df)

# Mean of X
X_bar <- mean(df$GSPC_Excess_Return, na.rm = TRUE)

# SE of the Estimate
s_e <- sqrt(sum(residuals(capm_model)^2) / (n - 2))

# Sum of Squares of GSPC
S_xx <- sum((df$GSPC_Excess_Return - X_bar)^2, na.rm = TRUE)

# Daily SF
s_f <- s_e * sqrt(1 + 1/n + ((expannualret - crfr - X_bar)^2 / S_xx))

# Annual SF
annual_s_f <- s_f * sqrt(252)

# Annual Expected Return
annual_expected_return_amd <- crfr + beta * (expannualret - crfr)

# 90% prediction interval
alpha <- 0.10
t_value <- qt(1 - alpha / 2, df = n - 2)

lower_bound <- annual_expected_return_amd - t_value * annual_s_f
upper_bound <- annual_expected_return_amd + t_value * annual_s_f

# Printing the Results
print (annual_expected_return_amd)
print (lower_bound)
print (upper_bound)
```






