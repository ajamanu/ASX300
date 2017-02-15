# Index Analysis.R
# Analysis of Stock Indexes
# http://www.mattdancho.com/investments/2016/10/23/SP500_Analysis.html
# Created by Aja Manu 15/02/17

# Set working directory
setwd("~/R/Quantmod")

# Clear working directory
rm(list = ls())

# Load library
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots

# Web-scrape S&P200 stock list
#sp_200 <- read_html("https://en.wikipedia.org/wiki/S%26P/ASX_200") %>%
#      html_node("table.wikitable") %>%
#      html_table() %>%
#      select(`Symbol`, Company, GICS, Sector, Headquarters) %>%
#      as_tibble()

# Load data from file
asx_300 <- read.csv("ASX300.csv") %>%
      as_tibble()

# Format names
names(asx_300) <- asx_300 %>% 
      names() %>% 
      str_to_lower() %>% 
      make.names()

# Show results
asx_300

# Get a frequency of sectors
asx_300 %>%
      # Summarise data by frequency
      group_by(sector) %>%
      summarise(count = n()) %>%
      # Visualize 
      ggplot(aes(x = sector %>% fct_reorder(count),
                 y = count
      )) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), size = 3, nudge_y = 4, nudge_x = .1) + 
      scale_y_continuous(limits = c(0,100)) +
      ggtitle(label = "Sector Frequency Among ASX300 Stocks") +
      xlab(label = "GICS Sector") +
      theme(plot.title = element_text(size = 16)) + 
      coord_flip()

# Create ticker for getting data
asx_300$symbol <- paste0(str_sub(asx_300$ticker, start = -3), # get the last three characters
                         ".AX")

# Only keep the asx stocks
asx_300$country <- str_sub(asx_300$ticker, end = 3) # get the first three letters forom the tickers
asx_300 <- asx_300 %>%
      subset(country == "ASX")

# Convinence function for getting data
get_stock_prices <- function(symbol, return_format = "tibble", ...) {
      # Get stock prices
      stock_prices_xts <- getSymbols(Symbols = symbol, auto.assign = FALSE, ...)
      # Rename
      names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      # Return in xts format if tibble is not specified
      if (return_format == "tibble") {
            stock_prices <- stock_prices_xts %>%
                  as_tibble() %>%
                  rownames_to_column(var = "Date") %>%
                  mutate(Date = ymd(Date))
      } else {
            stock_prices <- stock_prices_xts
      }
      stock_prices
}

# Convinence function for log returns
get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
      # Convert tibble to xts
      if (!is.xts(x)) {
            x <- xts(x[,-1], order.by = x$Date)
      }
      # Get log returns
      log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
      # Rename
      names(log_returns_xts) <- "Log.Returns"
      # Return in xts format if tibble is not specified
      if (return_format == "tibble") {
            log_returns <- log_returns_xts %>%
                  as_tibble() %>%
                  rownames_to_column(var = "Date") %>%
                  mutate(Date = ymd(Date))
      } else {
            log_returns <- log_returns_xts
      }
      log_returns
}

# Check if the functions work
"AAC.AX" %>% 
      get_stock_prices(return_format = "tibble") %>% 
      get_log_returns(return_format = "tibble") 

# need to remove certain stocks that are no longer in the index
asx_300 <- asx_300[asx_300$ticker != "ASX:WGX", ]

# Get sotck price for all asx300
asx_300 <- asx_300 %>%
      mutate(
            stock.prices = map(symbol, 
                               function(.x) get_stock_prices(.x, 
                                                             return_format = "tibble",
                                                             from = "2007-01-01",
                                                             to = "2016-12-31")
            ),
            log.returns  = map(stock.prices, 
                               function(.x) get_log_returns(.x, return_format = "tibble")),
            mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
            sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
            n.trade.days = map_dbl(stock.prices, nrow)
      )  

# plot interactive plot
plot_ly(data   = asx_300,
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ n.trade.days,
        colors = "Blues",
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", symbol, "</em><br>",
                         "Ticker: ", ticker, "<br>",
                         "Sector: ", sector, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
      layout(title   = 'S&amp;P200 Analysis: Stock Risk vs Reward',
             xaxis   = list(title = 'Risk/Variability (StDev Log Returns)',
                            gridcolor = 'rgb(255, 255, 255)',
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
             yaxis   = list(title = 'Reward/Growth (Mean Log Returns)',
                            gridcolor = 'rgb(255, 255, 255)',
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
             margin = list(l = 100,
                           t = 100,
                           b = 100),
             font   = list(color = '#FFFFFF'),
             paper_bgcolor = 'rgb(0, 0, 0)',
             plot_bgcolor = 'rgb(0, 0, 0)')

# Filter stocks to get ones with high return and low variablity
asx_300 %>%
      filter(mean.log.returns >= 0.001,
             sd.log.returns < 0.0315) %>%
      select(ticker, mean.log.returns:n.trade.days) %>%
      arrange(mean.log.returns %>% desc())

# Getting correlations
limit <- 30
asx_300_hp <- asx_300 %>%
      filter(n.trade.days > 1000) %>%
      filter(sd.log.returns < 0.0315) %>%
      mutate(rank = mean.log.returns %>% desc() %>% min_rank()) %>%
      filter(rank <= limit) %>%
      arrange(rank) %>%
      select(ticker, rank, mean.log.returns, sd.log.returns, log.returns)
asx_300_hp 

asx_300_hp_unnest <- asx_300_hp %>%
      select(ticker, log.returns) %>%
      unnest()
asx_300_hp_unnest

asx_300_hp_spread <- asx_300_hp_unnest %>%
      spread(key = ticker, value = Log.Returns) %>%
      na.omit()
asx_300_hp_spread

asx_300_hp_cor <- asx_300_hp_spread %>%
      select(-Date) %>%
      cor() 
asx_300_hp_cor[1:6, 1:6] # show first 6 columns and rows

sp_200_hp_cor %>%
      corrplot(order   = "hclust", 
               addrect = 11)

# Simulation stock price
# Simulate for APO
getSymbols("APO.AX") # get price for APO

# Get daily returns
APO.AX %>%
      Ad() %>%
      dailyReturn(type = 'log') %>% 
      head() 

APO_log_returns <- APO.AX %>%
      Ad() %>%
      dailyReturn(type = "log")
names(APO_log_returns) <- "APO.Log.Returns"

# Plot the log-returns    
APO_log_returns %>%    
      ggplot(aes(x = APO.Log.Returns)) + 
      geom_histogram(bins = 100) + 
      geom_density() +
      geom_rug(alpha = 0.5) 

# Examin distribution
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- APO_log_returns %>% 
      quantile(probs = probs, na.rm = TRUE)
dist_log_returns

# Get mean and sd of returns
mean_log_returns <- mean(APO_log_returns, na.rm = TRUE)
sd_log_returns <- sd(APO_log_returns, na.rm = TRUE)

mean_log_returns %>% exp()

# Monte Carlo Simulation for Stock
# Parameters
N     <- 252 # Number of Stock Price Simulations
M     <- 250  # Number of Monte Carlo Simulations   
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- APO.AX$APO.AX.Adjusted[[nrow(APO.AX$APO.AX.Adjusted)]]

# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
      monte_carlo_mat[[1, j]] <- price_init
      for(i in 2:N) {
            monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
      }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>%
      as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
      gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
      ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
      geom_line(alpha = 0.1) +
      ggtitle(str_c("MA: ", M, 
                    " Monte Carlo Simulations for Prices Over ", N, 
                    " Trading Days"))

# Get CI for the stock price at the end of sim
end_stock_prices <- price_sim %>% 
      filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)

# Inputs
N_hist          <- nrow(APO.AX) / 252
p_start_hist    <- APO.AX$APO.AX.Adjusted[[1]]
p_end_hist      <- APO.AX$APO.AX.Adjusted[[nrow(APO.AX)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# CAGR calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1