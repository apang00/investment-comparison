#### Preamble ####
# Purpose: Describe how the process was downloaded
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Having access to the internet to access and download raw data


#### Workspace setup ####
library(tidyverse)
library(haven)
library(here)

#### Download data ####

### Data for precious metals were downloaded here: ###
# For silver: https://markets.businessinsider.com/commodities/silver-price
# For gold: https://markets.businessinsider.com/commodities/gold-price

### Data for Crypto is downloaded here: ###
# For ethereum: https://finance.yahoo.com/quote/ETH-USD/history
# For bitcoin: https://finance.yahoo.com/quote/BTC-USD/history

#### Save data ####

eth <- read_csv(here::here("inputs/data/raw_data/ETH-USD.csv"))
btc <- read_csv(here::here("inputs/data/raw_data/BTC-USD.csv"))
silver <- read_csv(here::here("inputs/data/raw_data/gold.csv"))
gold <- read_csv(here::here("inputs/data/raw_data/silver.csv"))



         
