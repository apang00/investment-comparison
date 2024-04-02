#### Preamble ####
# Purpose: Clean the two datasets so that unneccessary data is removed and the dates match up
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Having the raw data downloaded from kaggle.com

#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Clean data ####
metals <- read_csv("inputs/data/raw_data/metals_price.csv")
crypto <- read_csv("inputs/data/raw_data/crypto_data_updated_13_november.csv")


#### Save data ####
write_csv(cleaned_data, "outputs/data/analysis_data.csv")
