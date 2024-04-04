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

#### Clean data ####

# In this category I will set 1 btc as the standard on the graph and try to 
# match the other values to be in the 1 btc vicinity.
# For closing prices, I will round to the nearest integer and for dates, I will
# select the fist and 15th of every month starting 2017-11-15 and ending with
# 2024-04-01

eth <- read_csv(here::here("data/raw_data/ETH-USD.csv"))
btc <- read_csv(here::here("data/raw_data/BTC-USD.csv"))
silver <- read_csv(here::here("data/raw_data/silver.csv"))
gold <- read_csv(here::here("data/raw_data/gold.csv"))

# I am doing 20x for eth to match btc
eth_clean <- eth %>%
  select(Date, Close, Volume) %>%
  mutate(Close = round(Close * 20, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  rename(eth_Close = Close)

btc_clean <- btc %>%
  select(Date, Close, Volume) %>%
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

# 2000 oz of silver to match btc
silver_clean <- silver %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Date = format(Date, "%Y-%m-%d")) %>% 
  select(Close, Date) %>%
  mutate(Close = round(Close * 2000, 0)) %>%
  filter(substr(Date, 9, 10) %in% c("01", "15")) %>%
  filter(Date >= "2017-11-14") %>%
  rename(silver_Close = Close)

# join the 4 datasets into 2, one for crypto one for metals
crypto <- inner_join(eth_clean, btc_clean, by = "Date")
metals <- inner_join(gold_clean, silver_clean, by = "Date")
metals <- metals %>%
  select(Date, everything())

#### Save data ####
write_csv(crypto, here::here("data/analysis_data/crypto.csv"))
write_csv(metals, here::here("data/analysis_data/metals.csv"))
