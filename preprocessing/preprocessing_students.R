##### INCOMING STUDENTS' KNOWLEDGE OF RESEARCH PRACTICES #####
##### Jennifer Beaudry, Matthew Williams, Michael Philipp ####

##### PREPROCESSING THE DATA #####


##### LIBRARY #####

library(tidyverse)
library(here)


####### FUNCTIONS ############
# from Mathew Ling's Misinformation Github repo
# https://github.com/Lingtax/misinformation/blob/master/R/read_qualtrics.R

read_qualtrics <- function(file, legacy = TRUE) {
  a <- readr::read_csv(file) %>% janitor::clean_names()
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
  rename_at(df, vars(all_of(keys)), ~ values)
}

##### LOAD DATA #####

# raw data without labels
df_num <- here::here("survey", "data", "students_raw_data.csv") %>%
  read_qualtrics(legacy = TRUE) %>%
  mutate(id = 1:n())


# raw data with labels
df_lab <- here::here("survey", "data", "students_raw_data_with_labels.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  mutate(id = 1:n())

# load metadata

metadata <- read_csv(here::here("data", "students_metadata_working_data.csv")) %>%
  select(-c(X10,"note:")) %>%  # delete unnecessary columns
  filter(old_variable != "NA") # remove the instruction variables


# Breadcrumb: likely rename to remove working data

#### CLEAN DATA ####

df_num <- df_num %>%
  relocate(id) %>% # move the ID variable to the first column
  select(-c(v1:consent)) %>%  # remove extraneous columns
  select(-c(uni_country:nationality))  # remove Qualtrics-coded variables [use label dataset for these]

df_lab <- df_lab %>%
  relocate(id) %>% # move the ID variable to the first column
  select(-c(start_date:consent)) %>%  # remove extraneous columns
  select(-c(uni:major,
            gender,
            q54,
            practices_1:rep_perc_1,
            crisis_text:comments)) %>%  # remove numeric variables
  rename_all(paste0, "_lab") %>% # add "_num" to differentiate the columns with numbers vs. labels
  rename(id = id_lab) # except for the id column because we need that for the join!

# join the two tibbles
df <- df_num %>% inner_join(df_lab, by = "id")


##### RECODE VARIABLE NAMES #####

# recode variable labels according to metadata

df <- meta_rename(df, metadata, old = old_variable, new = new_variable)


# Breadcrumbs turn eligibility into a factor & use the info.
# again, will need to include the variable labels for those that make sense! [done!]
# so the next step should be to select columns from the two data files & merge them [done!]
# Also need to spend time working on the metadata!
# will need to keep the num & label versions for the strongly agree variables [done!]


#### CONVERT VARIABLES INTO FACTORS ####

df$age <- factor(df$age)
df$age_lab <- factor(df$age_lab)
df$eligibility <- factor(df$eligibility)
df$eligibility_lab <- factor(df$eligibility_lab)
df$high_school <- factor(df$high_school)
df$high_school_lab <- factor(df$high_school_lab)
df$psych_hs <- factor(df$psych_hs)
df$psych_hs_lab <- factor(df$psych_hs_lab)
df$first_gen_lab <- factor(df$first_gen_lab)
df$critical_cnorm_lab <- factor(df$critical_cnorm_lab)
df$critical_norm_lab <- factor(df$critical_norm_lab)
df$prereg_norm_lab <- factor(df$prereg_norm_lab)
df$prereg_cnorm_lab <- factor(df$prereg_cnorm_lab)
df$reg_report_norm_lab <- factor(df$reg_report_norm_lab)
df$reg_report_cnorm_lab <- factor(df$reg_report_cnorm_lab)
df$phack_cnorm_lab <- factor(df$phack_cnorm_lab)
df$phack_norm_lab <- factor(df$phack_norm_lab)
df$hark_cnorm_lab <- factor(df$hark_cnorm_lab)
df$hark_norm_lab <- factor(df$hark_norm_lab)
df$info_for_rep_norm_lab <- factor(df$info_for_rep_norm_lab)
df$info_for_rep_cnorm_lab <- factor(df$info_for_rep_cnorm_lab)
df$preprint_norm_lab <- factor(df$preprint_norm_lab)
df$preprint_cnorm_lab <- factor(df$preprint_cnorm_lab)
df$open_materials_norm_lab <- factor(df$open_materials_norm_lab)
df$open_materials_cnorm_lab <- factor(df$open_materials_cnorm_lab)
df$open_data_norm_lab <- factor(df$open_data_norm_lab)
df$open_data_cnorm_lab <- factor(df$open_data_cnorm_lab)
df$open_acc_norm_lab <- factor(df$open_acc_norm_lab)
df$open_acc_cnorm_lab <- factor(df$open_acc_cnorm_lab)
df$crisis_lab <- factor(df$crisis_lab)


#### DETERMINE ELIGIBILITY ####

# note re: eligibility
# only those who say "1" are eligible

df <- df %>%
  filter(eligibility %in% 1)



################### WRITE DATA TO CSV #############

# when done preprocessing, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "students_processed.csv"), row.names = FALSE)
