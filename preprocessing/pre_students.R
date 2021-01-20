# Preprocessing the data for the incoming students project

##### LIBRARY #####

library(tidyverse)
library(here)


####### FUNCTIONS ############
# from Mathew Ling's Misinformation repo on Github

# modify Mathew Ling's 'read_qualtrics' function to remove 'janitor_names'

read_qualtrics <- function(file, legacy = TRUE) {
  a <- readr::read_csv(file)
  if (legacy == FALSE) {
    a <- a[3:nrow(a), ]
  } else {
    a <- a[2:nrow(a), ]
  }
  a %>% readr::type_convert(trim_ws = TRUE)
}

# Mathew Ling's 'meta_rename' function

meta_rename <-  function(df, metadata, old, new) {

  keys   <- metadata[[deparse(substitute(old))]]
  values <- metadata[[deparse(substitute(new))]]
  rename_at(df, vars(keys), ~ values)
}

##### LOAD DATA #####

df <- here::here("survey", "data", "raw_data.csv") %>%
  read_qualtrics()

df <- df %>%
  mutate(id = 1:n()) %>%
  select (-c(V1:Consent))


# note re: eligibility
# only those who say "1" are eligible

df <- df %>%
  filter(Eligibility %in% 1)



