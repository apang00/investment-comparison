#### Preamble ####
# Purpose: Model data in different ways for paper analysis
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Data cleaned and in analysis path

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(ggplot2)

#### Read data ####
crypto <- read_csv(here::here("data/analysis_data/crypto prices.csv"))
metals <- read_csv(here::here("data/analysis_data/metals prices.csv"))
cryptov <- read_csv(here::here("data/analysis_data/crypto volatility.csv"))
metalsv <- read_csv(here::here("data/analysis_data/metals volatility.csv"))

price_join <- merge(crypto, metals, by = "Date", all = FALSE)


### Model data ####

# Price Data chart
compilation <- ggplot() +
  geom_line(data = crypto, aes(x = Date, y = eth_Close, color = "Ethereum"), size = 0.75) +
  geom_line(data = crypto, aes(x = Date, y = btc_Close, color = "Bitcoin"), size = 0.75) +
  geom_line(data = metals, aes(x = Date, y = gold_Close, color = "Gold"), size = 0.75) +
  geom_line(data = metals, aes(x = Date, y = silver_Close, color = "Silver"), size = 0.75) +
  labs(title = "Asset Comparisons 2017 to Present",
       x = "Date", y = "Closing Amount ($USD)") +
  scale_color_manual(values = c("Ethereum" = "royalblue", "Bitcoin" = "sienna2", "Gold" = "#FFD700", "Silver" = "#C0C0C0"),
                     labels = c("Ethereum" = "Ethereum (20 Units)", "Bitcoin" = "Bitcoin", "Gold" = "Gold (30 oz.)", "Silver" = "Silver (2500 oz.)")) +
  guides(color = guide_legend(title = "Assets")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

compilation

# Volatility Data Chart
volatility <- ggplot() +
  geom_line(data = cryptov, aes(x = Date, y = eth_ATR_norm, color = "Ethereum"), size = 0.75) +
  geom_line(data = cryptov, aes(x = Date, y = btc_ATR_norm, color = "Bitcoin"), size = 0.75) +
  geom_line(data = metalsv, aes(x = Date, y = gold_ATR_norm, color = "Gold"), size = 0.75) +
  geom_line(data = metalsv, aes(x = Date, y = silver_ATR_norm, color = "Silver"), size = 0.75) +
  labs(title = "Normalized ATR of Assets 2017 to Present",
       x = "Date", y = "Normalized ATR Value") +
  scale_color_manual(values = c("Ethereum" = "royalblue", "Bitcoin" = "sienna2", "Gold" = "#FFD700", "Silver" = "#C0C0C0")) +
  guides(color = guide_legend(title = "Assets")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

volatility

# Regression Chart BTC-Gold
bgfit <- lm(gold_Close ~ btc_Close, data = price_join)
bgplot <- ggplot(price_join, aes(x = btc_Close, y = gold_Close)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = coef(bgfit)[1], slope = coef(bgfit)[2], color = "red") +
  labs(title = "Bitcoin vs. Gold Prices Regression Model",
       x = "Bitcoin Closing Price", y = "Gold Closing Price")
print(bgplot)

# Regression for BTc-Silver
bsfit <- lm(silver_Close ~ btc_Close, data = price_join)
bsplot <- ggplot(price_join, aes(x = btc_Close, y = silver_Close)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = coef(bsfit)[1], slope = coef(bsfit)[2], color = "red") +
  labs(title = "Bitcoin vs. Silver Prices Regression Model",
       x = "Bitcoin Closing Price", y = "Silver Closing Price")
print(bsplot)

# Regression for Ethereum-Gold
egfit <- lm(gold_Close ~ eth_Close, data = price_join)
egplot <- ggplot(price_join, aes(x = eth_Close, y = gold_Close)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = coef(egfit)[1], slope = coef(egfit)[2], color = "red") +
  labs(title = "Ethereum vs. Gold Prices Regression Model",
       x = "Ethereum Closing Price", y = "Gold Closing Price")
print(egplot)

# Regression for Ethereum-Silver
esfit <- lm(silver_Close ~ eth_Close, data = price_join)
esplot <- ggplot(price_join, aes(x = eth_Close, y = silver_Close)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = coef(esfit)[1], slope = coef(esfit)[2], color = "red") +
  labs(title = "Ethereum vs. Silver Prices Regression Model",
       x = "Ethereum Closing Price", y = "Silver Closing Price")
print(esplot)


#### Save model ####
saveRDS(
  compilation,
  file = here::here("models/price chart.rds")
)

saveRDS(
  volatility,
  file = here::here("models/volatility chart.rds")
)

saveRDS(
  bgplot,
  file = here::here("models/btcgold Regression.rds")
)

saveRDS(
  bsplot,
  file = here::here("models/btcsilver Regression.rds")
)

saveRDS(
  egplot,
  file = here::here("models/ethgold Regression.rds")
)

saveRDS(
  esplot,
  file = here::here("models/ethsilver Regression.rds")
)

