"ACTL1101 Assignment Part A by Gokulan Vijeyakumar 2024 T2"

output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}
# Load data from CSV file
amd_df <- read.csv("~/Downloads/AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date, "%d/%m/%Y")
amd_df$close <- as.numeric(amd_df$Adj)
amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, costs/proceeds, and accumulated shares
amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#Looping code for each rode in specific dates
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  if (i == 1 || amd_df$date[i] < as.Date("2019-07-07") || amd_df$date[i] > as.Date("2020-07-07")) {
    next
  }
  if (previous_price == 0 || current_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    cost <- current_price * share_size
    amd_df$costs_proceeds[i] <- -cost
    accumulated_shares <- accumulated_shares + share_size
  }
  previous_price <- current_price
}
# Selling all shares on the last day within the trading period
last_trading_day <- tail(which(amd_df$date <= as.Date("2020-07-07")), 1)
if (accumulated_shares > 0) {
  amd_df$trade_type[last_trading_day] <- "sell"
  proceeds <- amd_df$close[last_trading_day] * accumulated_shares
  amd_df$costs_proceeds[last_trading_day] <- proceeds
  accumulated_shares <- 0
}

# Updating accumulated shares column
for (i in 1:nrow(amd_df)) {
  if (i > 1) {
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + ifelse(is.na(amd_df$trade_type[i]), 0, ifelse(amd_df$trade_type[i] == "buy", share_size, -accumulated_shares))
  } else {
    amd_df$accumulated_shares[i] <- ifelse(is.na(amd_df$trade_type[i]), 0, ifelse(amd_df$trade_type[i] == "buy", share_size, 0))
  }
}
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past three years 
```{r period}
trading_period <- amd_df[amd_df$date >= as.Date("2019-07-07") & amd_df$date <= as.Date("2020-07-07"),]
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
#To find the total profit/loss
total_profit_loss <- sum(trading_period$costs_proceeds)

#To find the total invested capital 
invested_capital <- -sum(trading_period$costs_proceeds[trading_period$costs_proceeds < 0])

#To find the ROI
ROI <- (total_profit_loss / invested_capital) * 100
total_profit_loss
invested_capital
ROI
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
#Initialising variables
share_size <- 100
profit_percentage <- 0.20
accumulated_shares <- 0
total_buy_cost <- 0

for (i in 2:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  previous_price <- amd_df$close[i-1]
# Implementing the strategy (If price increased by 20% from APP, sell half)

  if (accumulated_shares > 0) {
    avg_purchase_price <- total_buy_cost / accumulated_shares
    if (current_price >= (1 + profit_percentage) * avg_purchase_price) {
      amd_df$trade_type[i] <- "sell"
      sell_shares <- accumulated_shares / 2
      amd_df$costs_proceeds[i] <- sell_shares * current_price
      accumulated_shares <- accumulated_shares - sell_shares
    }
  }
# (Buy if the current price < previous price)
  if (current_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    cost <- current_price * share_size
    amd_df$costs_proceeds[i] <- -cost
    accumulated_shares <- accumulated_shares + share_size
    total_buy_cost <- total_buy_cost + cost
  }
}
# Updating the accumulated shares column
amd_df$accumulated_shares <- cumsum(ifelse(amd_df$trade_type == "buy", share_size, ifelse(amd_df$trade_type == "sell", -share_size/2, 0)))

```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
#To find the new total profit/loss
total_profit_loss_new <- -sum(amd_df$costs_proceeds, na.rm = TRUE)

#To find the new total invested capital 
invested_capital_new <- -sum(amd_df$costs_proceeds[amd_df$costs_proceeds < 0], na.rm = TRUE)

#To find the new ROI
ROI_new <- (total_profit_loss_new / invested_capital_new) * 100

print(total_profit_loss_new)
print(invested_capital_new)
print(ROI_new)
```

ROI Normal --> 26.7225, ROI Strategy --> 21.9263 

Discussion:
For the assignment, I created an algorithmic trading plan following the instructions, choosing a trading window that was July 7, 2019, to July 7, 2020. By following the instructions to create a simple algorithm into place, a profit-taking plan was added, allowing me to find a ROI. This simple strategy produced a 26% return on investment, a relatively high ROI. As per the instructions, an additional profit-taking mechanism was added to improve the original plan. The strategy highlighted that if the stock price rose by 20% from the average purchase price it was designed to sell half of the holdings, theoretically improving the ROI by retaining earnings effectively. However, the improved strategy produced a smaller ROI of 21%. This decrease is most likely due to events such as the COVID-19 pandemic in early 2020, causing this new profit-strategy implemented to create a worse outcome than without the strategy. The initial strategy's decision to buy during price drops would have led to substantial purchases during this period, making it a better alternative for ROI than the implemented one which was not suited to the irregularity COVID-19 brought in the market. This makes sense as due to the vast price drops occurring, the first strategy is better suited than selling if the price rises by 20% as this did not occur sufficiently at this time. Although, the subsequent recovery in stock prices, particularly in the tech sector, would have contributed to the overall profit and high ROI’s received. Another driver of the ROI was that AMD became a key player in the tech industry as AMD continued to strengthen its market position with the launch of new processors and GPUs, innovating the sector. This is indicated clearly by its rise in stock price from 30 to 52 during this period along with their net income increasing by 349%, demonstrating how its own development in the sector helped to increase this ROI. Further, the company announced partnerships with major firms such as Microsoft and Sony in the development of consoles such as the PS5, enhancing its growth. By analysing the two trading methods from July 7, 2019, to July 7, 2020, I was able to understand how AMD's stock price was impacted by both business and market changes. The profit-taking strategy offered an approach which led to a ROI of 21%, whereas the first method yielded a higher ROI of 26%. Ultimately, the use of two strategies illustrated how crucial it is to modify trading algorithms in response to market situations and to continuously improve tactics in order to maximise profits.








**Answer:**

```r
#fill the code
```
