#### Preamble ####
# Purpose: Create some sample points to see what data could look like in a graph
# Author: Yi Fei Pang
# Date: 04 April 2023
# Contact: yifei.pang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Understanding of the structure and result of dataset desired

#### Workspace setup ####
library(ggplot2)
library(dplyr)
library(tidyverse)

set.seed(123)

#### Simulate data ####
# Create a simplified dataframe with what my data could potentially look like
# with 6 years between 2017 and 2022 with all the values for the 4 assets

start_year <- 2017
end_year <- 2022

Years <- seq(start_year, end_year, by = 1)

BTC <- runif(6, min = 25000, max = 32000)
Ethereum <- runif(6, min = 1600, max = 2200)
Gold <- runif(6, min = 1800, max = 2400)
Silver <- runif(6, min = 20, max = 35)

sample_data <- data.frame(Year = Years, BTC = BTC,
                 Ethereum = Ethereum, Gold = Gold, Silver = Silver)

# Create the line graph
ggplot(sample_data, aes(x = Years)) +
  geom_line(aes(y = BTC, color = "BTC")) +
  geom_line(aes(y = Ethereum, color = "Ethereum")) +
  geom_line(aes(y = Gold, color = "Gold")) +
  geom_line(aes(y = Silver, color = "Silver")) +
  labs(title = "Crypto and Precious Metals Prices Over Time",
       x = "Date",
       y = "Price",
       color = "Asset") +
  scale_color_manual(values = c(BTC = "blue", Ethereum = "green", Gold = "gold", Silver = "gray"),
                     labels = c("BTC", "Ethereum", "Gold", "Silver")) +
  theme_minimal()

# From the linegraph, I notice that to observe trends better, they have to be 
# at around the same price range. For example, for gold to be have the same
# price and variance as BTC, I would have to use 10oz so it can be around the 
# 20,000 to 30,000 range so that the graph is more digestable
