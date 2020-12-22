# Preprocessing the data for the incoming students project

library(tidyverse)
library(here)

# load data

df <- read.csv(here::here("survey", "data", "open_science.csv"))


