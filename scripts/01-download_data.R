#### Preamble ####
# Purpose: Describe how the process was downloaded
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Having access to kaggle.com and having downloaded the raw data


#### Workspace setup ####
library(tidyverse)
library(haven)
library(here)

#### Download data ####

# Data for precious metals were downloaded here: https://www.kaggle.com/datasets/damianprsk/precious-metals-price-dataset?resource=download
# Data for crypto was downloaded here: https://www.kaggle.com/datasets/himanshunakrani/cryptocurrencies-dataset?select=crypto_data_updated_13_november.csv
# Both datasets were downloaded manually from kaggle.com

#### Save data ####

metals <- read_csv(here::here("inputs/data/raw_data/metals_price.csv"))
crypto <- read_csv(here::here("inputs/data/raw_data/crypto_data_updated_13_november.csv"))


         
