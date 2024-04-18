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

# Regression Chart Metals and Crypto
crypto$totalc <- crypto$eth_Close + crypto$btc_Close
metals$totalm <- metals$gold_Close + metals$silver_Close
price_join <- merge(crypto, metals, by = "Date", all = FALSE)
price_join <- select(price_join, Date, totalc, totalm)

esfit <- stan_glm(
  formula = totalc ~ totalm,
  data = price_join,
  family = gaussian()
)

esplot <- ggplot(price_join, aes(x = totalm, y = totalc)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = coef(esfit)[1], slope = coef(esfit)[2], color = "red") +
  labs(title = "Crypto vs. Metals Prices Regression Model",
       x = "Metals Closing Price", y = "Cryptos Closing Price")


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
  esplot,
  file = here::here("models/Regression charts.rds")
)

