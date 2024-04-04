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
crypto <- read_csv(here::here("data/analysis_data/crypto.csv"))
metals <- read_csv(here::here("data/analysis_data/metals.csv"))

### Model data ####
compilation <- ggplot() +
  geom_line(data = crypto, aes(x = Date, y = eth_Close, color = "Ethereum"), size = 0.75) +
  geom_line(data = crypto, aes(x = Date, y = btc_Close, color = "Bitcoin"), size = 0.75) +
  geom_line(data = metals, aes(x = Date, y = gold_Close, color = "Gold"), size = 0.75) +
  geom_line(data = metals, aes(x = Date, y = silver_Close, color = "Silver"), size = 0.75) +
  labs(title = "Asset Comparisons 2017 to Present",
       x = "Date", y = "Closing Amount ($USD)") +
  scale_color_manual(values = c("Ethereum" = "royalblue", "Bitcoin" = "sienna2", "Gold" = "#FFD700", "Silver" = "#C0C0C0"),
                     labels = c("Ethereum" = "Ethereum (20 Units)", "Bitcoin" = "Bitcoin", "Gold" = "Gold (30 oz.)", "Silver" = "Silver (2000 oz.)")) +
  guides(color = guide_legend(title = "Assets")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  

"
first_model <-
  stan_glm(
    formula = flying_time ~ length + width,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )"


#### Save model ####
saveRDS(
  compilation,
  file = here::here("models/compilation.rds")
)


