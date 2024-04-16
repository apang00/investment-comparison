#### Preamble ####
# Purpose: Clean the four datasets so that unneccessary data is removed and the dates match up
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Having the raw data downloaded from the sites aforementioned

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(haven)
library(here)
library(TTR)
library(arrow)

#### Clean data ####

eth <- read_csv(here::here("data/raw_data/ETH-USD.csv"))
btc <- read_csv(here::here("data/raw_data/BTC-USD.csv"))
silver <- read_csv(here::here("data/raw_data/silver.csv"))
gold <- read_csv(here::here("data/raw_data/gold.csv"))

## Section 1 ## 
# In this category I will set 1 btc as the standard on the graph and try to 
# match the other values to be in the 1 btc vicinity.
# For closing prices, I will round to the nearest integer and for dates, I will
# select the fist and 15th of every month starting 2017-11-15 and ending with
# 2024-04-01

# I am doing 20x for eth to match btc
eth_clean <- eth %>%
  select(Date, Close) %>%
  mutate(Close = round(Close * 20, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  rename(eth_Close = Close)

btc_clean <- btc %>%
  select(Date, Close) %>%
  mutate(Close = round(Close, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  filter(Date >= "2017-11-14") %>%
  rename(btc_Close = Close)

# 30 oz. of gold to match BTC
gold_clean <- gold %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Date = format(Date, "%Y-%m-%d")) %>% 
  select(Close, Date) %>%
  mutate(Close = round(Close * 30, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  filter(Date >= "2017-11-14") %>%
  rename(gold_Close = Close)

# 2500 oz of silver to match btc
silver_clean <- silver %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Date = format(Date, "%Y-%m-%d")) %>% 
  select(Close, Date) %>%
  mutate(Close = round(Close * 2500, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  filter(Date >= "2017-11-14") %>%
  rename(silver_Close = Close)

# join the 4 datasets into 2, one for crypto one for metals
crypto <- inner_join(eth_clean, btc_clean, by = "Date") 
metals <- inner_join(gold_clean, silver_clean, by = "Date")
metals <- metals %>%
  select(Date, everything())

## Section 2 ##
# this section I will look at the Average True Range of each asset after 
# normalization

# Removes all the 0s and nulls in the datasets as inaccuracies
eth <- eth %>%
  filter(!is.na(Open) & Open != 0.00,
         !is.na(High) & High != 0.00,
         !is.na(Low) & Low != 0.00,
         !is.na(Close) & Close != 0.00)

btc <- btc %>%
  filter(!is.na(Open) & Open != 0.00,
         !is.na(High) & High != 0.00,
         !is.na(Low) & Low != 0.00,
         !is.na(Close) & Close != 0.00)

silver <- silver %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Date = format(Date, "%Y-%m-%d")) %>% 
  filter(!is.na(Open) & Open != 0.00,
         !is.na(High) & High != 0.00,
         !is.na(Low) & Low != 0.00,
         !is.na(Close) & Close != 0.00)

gold <- gold %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Date = format(Date, "%Y-%m-%d")) %>% 
  filter(!is.na(Open) & Open != 0.00,
         !is.na(High) & High != 0.00,
         !is.na(Low) & Low != 0.00,
         !is.na(Close) & Close != 0.00)

# Calculate True Range (TR) for each asset
eth$eth_TR <- pmax(eth$High - eth$Low, abs(eth$High - lag(eth$Close)), abs(eth$Low - lag(eth$Close)))
btc$btc_TR <- pmax(btc$High - btc$Low, abs(btc$High - lag(btc$Close)), abs(btc$Low - lag(btc$Close)))
silver$silver_TR <- pmax(silver$High - silver$Low, abs(silver$High - lag(silver$Close)), abs(silver$Low - lag(silver$Close)))
gold$gold_TR <- pmax(gold$High - gold$Low, abs(gold$High - lag(gold$Close)), abs(gold$Low - lag(gold$Close)))

# Calculate Average True Range (ATR) over a specified period I chose 15 days
period <- 15
eth$eth_ATR <- SMA(eth$eth_TR, n = period)
btc$btc_ATR <- SMA(btc$btc_TR, n = period)
silver$silver_ATR <- SMA(silver$silver_TR, n = period)
gold$gold_ATR <- SMA(gold$gold_TR, n = period)

# Normalize ATR for each asset
eth$eth_ATR_norm <- eth$eth_ATR / eth$Close
btc$btc_ATR_norm <- btc$btc_ATR / btc$Close
silver$silver_ATR_norm <- silver$silver_ATR / silver$Close
gold$gold_ATR_norm <- gold$gold_ATR / gold$Close

eth <- eth %>%
  filter(!is.na(eth_ATR_norm)) %>%
  select(Date, eth_TR, eth_ATR, eth_ATR_norm)

btc <- btc %>%
  filter(!is.na(btc_ATR_norm)) %>%
  select(Date, btc_TR, btc_ATR, btc_ATR_norm)

silver <- silver %>%
  filter(!is.na(silver_ATR_norm)) %>%
  select(Date, silver_TR, silver_ATR, silver_ATR_norm)

gold <- gold %>%
  filter(!is.na(gold_ATR_norm)) %>%
  select(Date, gold_TR, gold_ATR, gold_ATR_norm)

crypto_volatility <- inner_join(eth, btc, by = "Date")
metals_volatility <- inner_join(gold, silver, by = "Date")
metals_volatility <- metals_volatility %>%
  select(Date, everything())

# since there are way too many data points, I will be taking only a bimonthly 
# average of the ATR and the normalized ATR
metals_volatility_clean <- metals_volatility %>%
  arrange(Date) %>%  
  group_by(group = (row_number() - 1) %/% 15) %>%  
  summarise(
    Date = last(Date), 
    across(c(gold_ATR, gold_ATR_norm, silver_ATR, silver_ATR_norm), mean)  
  ) %>%
  ungroup() %>%  
  select(-group)

crypto_volatility_clean <- crypto_volatility %>%
  arrange(Date) %>%  
  group_by(group = (row_number() - 1) %/% 15) %>%  
  summarise(
    Date = last(Date),  
    across(c(eth_ATR, eth_ATR_norm, btc_ATR, btc_ATR_norm), mean) 
  ) %>%
  ungroup() %>%  
  select(-group)

#### Save data ####
write_csv(crypto, here::here("data/analysis_data/crypto prices.csv"))
write_csv(metals, here::here("data/analysis_data/metals prices.csv"))
write_csv(crypto_volatility_clean, here::here("data/analysis_data/crypto volatility.csv"))
write_csv(metals_volatility_clean, here::here("data/analysis_data/metals volatility.csv"))

