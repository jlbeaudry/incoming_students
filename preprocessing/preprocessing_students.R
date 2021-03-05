##### INCOMING STUDENTS' KNOWLEDGE OF RESEARCH PRACTICES #####
##### Jennifer Beaudry, Matthew Williams, Michael Philipp ####

##### PREPROCESSING THE DATA #####


##### LIBRARY #####

library(tidyverse)
library(here)
library(tools)


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

# Matt Williams's missing data function; counts how many NAs there are

miss_fun = function(x){
  sum(is.na(x))
}

##### LOAD DATA #####

# raw data without labels
df_num <- here::here("data", "students_raw_data.csv") %>%
  read_qualtrics(legacy = TRUE) %>%
  mutate(id = 1:n())


# raw data with labels
df_lab <- here::here("data", "students_raw_data_with_labels.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  mutate(id = 1:n())

# load metadata
metadata <- read_csv(here::here("data", "students_metadata.csv")) %>%
  select(-c(X10,"note:")) %>%  # delete unnecessary columns
  filter(old_variable != "NA", old_variable != "exclude") # remove the instruction variables

# Breadcrumb: likely rename to remove working data [done!]

#### CLEAN DATA ####

df_num <- df_num %>%
  relocate(id) %>% # move the ID variable to the first column
  select(-c(v1:consent,scenario_remind)) %>%  # remove extraneous columns
  select(-c(uni_country:nationality))  # remove Qualtrics-coded variables [use label dataset for these]

df_lab <- df_lab %>%
  relocate(id) %>% # move the ID variable to the first column
  select(-c(start_date, end_date, progress:consent)) %>%  # remove extraneous columns
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

# Breadcrumbs turn eligibility & criteria into a factor & use the info. [done]
# again, will need to include the variable labels for those that make sense! [done!]
# so the next step should be to select columns from the two data files & merge them [done!]
# Also need to spend more time working on the metadata!
# will need to keep the num & label versions for the strongly agree variables [done!]


#### CONVERT VARIABLES INTO FACTORS ####


###### REARRANGE THE VARIABLES IN THE TIBBLE TO ALIGN WITH SURVEY ORDER ######
# at least somewhat; I haven't moved the number and label variables of each
  # question together, but this brings the demographics info together

df <- df %>%
  relocate (c(uni_country, nationality), .after = gender)

######### RECODE GENDER ##########

# gender --> need to run through gendercoder [breadcrumb!]

########## EXCLUSION CRITERIA ############

# Breadcrumbs: random things:
  # check to see how we used the R refs in the VR document [done & fixed]
  # send M&M the data files & tell them where they belong... [done]
  # what are we going to do with those folks who did not give their age [retain] or
  # give information about their enrolment [exclude], as per prereg [done]


# key items in the study; look to see who did NOT respond to any of these
study_var <- c("critical_cnorm",
                 "critical_norm",
                 "prereg_norm",
                 "prereg_cnorm",
                 "reg_report_norm",
                 "reg_report_cnorm",
                 "phack_cnorm",
                 "phack_norm",
                 "hark_cnorm",
                 "hark_norm",
                 "info_for_rep_norm",
                 "info_for_rep_cnorm",
                 "preprint_norm",
                 "preprint_cnorm",
                 "open_materials_norm",
                 "open_materials_cnorm",
                 "open_data_norm",
                 "open_data_cnorm",
                 "open_access_norm",
                 "open_access_cnorm",
                 "preregistration",
                 "registered_report",
                 "incomplete_results",
                 "harking",
                 "detailed_methodology",
                 "preprint_pre",
                 "open_materials",
                 "open_data",
                 "available_data",
                 "open_access",
                 "rep_perc",
                 "crisis")

# create new variable showing the number of items with missing values
df$nmiss <- apply(X = df[, study_var], MARGIN = 1, FUN = miss_fun)


# create a new factor `exclude` to indicate which participants meet our inclusion criteria

df <- df %>%
  mutate(exclude = factor(case_when(
    eligibility == '1' & nmiss < 32  ~ "include",
    TRUE ~ "exclude")))

# can use just eligibility == 1 to cover the exclusion criteria of age & Qualtrics status.
  # also need to exclude those who were eligible, but did not answer any of the
  # key questions (nmiss = 32).


# [[BREADCRUMBS: CLEAN ALL OF THIS UP & WORK THROUGH THE LOGIC AGAIN, INCLUDE
#   THE KEY INFO RE HOW MANY WERE ELIGIBLE, BUT DID NOT COMPLETE ANY KEY
#   MEASURES [88] BASED ON THE ANTI-JOIN!]] [done!]
# add the exclusion variables to the dataframe & metadata (and remove when
  # loading metadata) [still need to do!]

# create new variables for each of the eliibility criteria
  # capture the hieararchy of decisions, but forcing previously-excluded cases to
  # be recoded as "N/A" for each subsequent criterion [done!]

df <- df %>%
  mutate (age_criteria = factor(case_when (
    age_lab %in% "0-17 years" ~ "exclude",
    TRUE ~ "include")
  )) %>%
  mutate (eligibility_criteria = factor(case_when (
    age_criteria %in% "exclude" ~ "N/A",
    eligibility %in% NA ~ "exclude",
      eligibility != "1" ~ "exclude",
      TRUE ~ "include")
  )) %>%
  mutate (status_criteria = factor(case_when (
    age_criteria %in% "exclude" ~ "N/A",
    eligibility_criteria %in% "exclude" ~ "N/A",
    status_lab != "IP Address" ~ "exclude",
               TRUE ~ "include")
  )) %>%
  mutate(nmiss_criteria = factor(case_when (
    age_criteria %in% "exclude" ~ "N/A",
    eligibility_criteria %in% "exclude" ~ "N/A",
    status_criteria %in% "exclude" ~ "N/A",
    nmiss < 32  ~ "include",
    TRUE ~ "exclude")
  ))

################### WRITE DATA TO CSV #############

# when done preprocessing, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "students_processed.csv"), row.names = FALSE)

################### CODING QUALITATIVE RESPONSES #############

# We will need to code their qualitative responses (university, degree,
  # major). To do so, for the eligible participants, I am selecting those
  # variables and exporting them to a separate document.

# [[breadcrumbs: rejoin the coded values back to the df &
  # then write that data to the main data file.
  # Move this above when done preprocessing. ]]

qual <- df %>%
  filter(exclude %in% "include") %>%
  select(c(id,university, degree, major))

# force the responses to title case & then group those that are similar
  # use the id number to count how many responses matched
  # do this separately for uni, degree, and major

qual_uni <- qual %>%
  transmute(university = toTitleCase(university),id) %>%
  group_by(university) %>%
  summarise(n_distinct(id)) %>%
  rename(`frequency` = `n_distinct(id)`)

qual_deg <- qual %>%
  transmute(degree = toTitleCase(degree),id) %>%
  group_by(degree) %>%
  summarise(n_distinct(id)) %>%
  rename(`frequency` = `n_distinct(id)`)

qual_maj <- qual %>%
  transmute(major = toTitleCase(major),id) %>%
  group_by(major) %>%
  summarise(n_distinct(id)) %>%
  rename(`frequency` = `n_distinct(id)`)

# write each of them to csv files
write.csv(qual_uni, here::here("preprocessing", "qual_responses_uni.csv"), row.names = FALSE)

write.csv(qual_deg, here::here("preprocessing", "qual_responses_deg.csv"), row.names = FALSE)

write.csv(qual_maj, here::here("preprocessing", "qual_responses_maj.csv"), row.names = FALSE)

