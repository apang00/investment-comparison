#### Preamble ####
# Purpose: Test datasets for issues and abnormalities
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Data cleaned and in analysis path


#### Workspace setup ####
library(tidyverse)


#### Test data ####
crypto <- read_csv(here::here("data/analysis_data/crypto prices.csv"))
metals <- read_csv(here::here("data/analysis_data/metals prices.csv"))
cryptov <- read_csv(here::here("data/analysis_data/crypto volatility.csv"))
metalsv <- read_csv(here::here("data/analysis_data/metals volatility.csv"))

# Test that years exist between 2017 to 2024
all(as.numeric(format(crypto$Date, "%Y")) %in% 2017:2024)
all(as.numeric(format(metals$Date, "%Y")) %in% 2017:2024)

# Test that all dates are either 01 or 15
all(format(crypto$Date, "%d") %in% c("01", "15"))
all(format(metals$Date, "%d") %in% c("01", "15"))

# Test for range of values, to account for potential typos or data misinputs
!any(crypto$eth_Close > 100000) && !any(crypto$eth_Close < 500)
!any(crypto$btc_Close > 100000) && !any(crypto$btc_Close < 2000)
!any(metals$gold_Close > 100000) && !any(metals$gold_Close < 20000)
!any(metals$silver_Close > 100000) && !any(metals$silver_Close < 20000)

# Testing for any lines that are blanks
!any(is.na(cryptov)) 
!any(is.na(metalsv)) 
