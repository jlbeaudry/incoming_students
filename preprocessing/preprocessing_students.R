##### INCOMING STUDENTS' KNOWLEDGE OF RESEARCH PRACTICES #####
##### Jennifer Beaudry, Matthew Williams, Michael Philipp & Emily Kothe ####

##### PREPROCESSING THE DATA #####


##### LIBRARY #####

library(tidyverse)
library(here)
library(tools)
library(gendercoder) #devtools::install_github("ropenscilabs/gendercoder")


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
df_num <- here::here("survey", "data", "students_raw_data.csv") %>%
  read_qualtrics(legacy = TRUE) %>%
  mutate(id = 1:n())


# raw data with labels
df_lab <- here::here("survey", "data", "students_raw_data_with_labels.csv") %>%
  read_qualtrics(legacy = FALSE) %>%
  mutate(id = 1:n())

# load metadata
metadata <- read_csv(here::here("data", "students_metadata.csv")) %>%
  #dplyr::select(-c(X10,"note:")) %>%  # delete unnecessary columns
  filter(old_variable != "NA", old_variable != "exclude") # remove the instruction variables

#### CLEAN DATA ####

df_num <- df_num %>%
  relocate(id) %>% # move the ID variable to the first column
  dplyr::select(-c(v1:consent,scenario_remind)) %>%  # remove extraneous columns
  dplyr::select(-c(uni_country:nationality))  # remove Qualtrics-coded variables [use label dataset for these]

df_lab <- df_lab %>%
  relocate(id) %>% # move the ID variable to the first column
  dplyr::select(-c(start_date, end_date, progress:consent)) %>%  # remove extraneous columns
  dplyr::select(-c(uni:major,
            gender,
            q54,
            practices_1:rep_perc_1,
            crisis_text:comments)) %>%  # remove numeric variables
  rename_all(paste0, "_lab") %>% # add "_num" to differentiate the columns with numbers vs. labels
  rename(id = id_lab) # except for the id column because we need that for the join!

# join the two tibbles and delete columns that we don't need
df <- df_num %>% inner_join(df_lab, by = "id")



##### RECODE VARIABLE NAMES #####

# recode variable labels according to metadata

df <- meta_rename(df, metadata, old = old_variable, new = new_variable)


#### CONVERT VARIABLES INTO FACTORS ####


###### REARRANGE THE VARIABLES IN THE TIBBLE TO ALIGN WITH SURVEY ORDER ######
# bring the demographics info together

df <- df %>%
  relocate (c(uni_country, nationality), .after = gender)

######### RECODE GENDER ##########

custom_dictionary <- c(
  `female.` = "woman",
  `femamle` = "woman",
  `-` = NA_character_,
  `gender no longer has a place in modern society. sex is male.` = "man",
  `queer man` = "queer man"
)


# recode gender
df <- df %>%
   mutate(gender = recode_gender(gender, dictionary = c(manylevels_en, custom_dictionary), retain_unmatched = FALSE))

########## EXCLUSION CRITERIA ############

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
                 "phack",
                 "harking",
                 "info_for_rep",
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

# create new variables for each of the eligibility criteria
  # capture the hierarchy of decisions, but forcing previously-excluded cases to
  # be recoded as "N/A" for each subsequent criterion

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



################### CODING QUALITATIVE RESPONSES #############

### ACTUAL RECODING OF QUALITATIVE VARIABLES UNIVERSITY, DEGREE, MAJOR

df <- df %>%
  mutate(
    university_recoded = case_when(
      university == "Adelaide"                                   ~ "University of Adelaide",
      university == "adelaide uni"                               ~ "University of Adelaide",
      university == "Adelaide uni"                               ~ "University of Adelaide",
      university == "adelaide university"                        ~ "University of Adelaide",
      university == "Adelaide university"                        ~ "University of Adelaide",
      university == "the University of Adelaide"                 ~ "University of Adelaide",
      university == "university of  adelaide"                    ~ "University of Adelaide",
      university == "university of adelaide"                     ~ "University of Adelaide",
      university == "the university of adelaide"                 ~ "University of Adelaide",
      university == "The university of Adelaide"                 ~ "University of Adelaide",
      university == "University of Adelaide."                    ~ "University of Adelaide",
      university == "Adelaide University"                        ~ "University of Adelaide",
      university == "Univerisity of Adelaide"                    ~ "University of Adelaide",
      university == "University of Adelaide"                     ~ "University of Adelaide",
      university == "Adelaide Uni"                               ~ "University of Adelaide",
      university == "Adelaide Universtiy"                        ~ "University of Adelaide",
      university == "The University of Adelaide"                 ~ "University of Adelaide" ,
      university == "University of  Adelaide"                    ~ "University of Adelaide" ,
      university == "Bath"                                       ~ "University of Bath",
      university == "Batj"                                       ~ "University of Bath" ,
      university == "University of Bath"                         ~ "University of Bath",
      university == "Bath University"                            ~ "University of Bath" ,
      university == "The University of Bath"                     ~ "University of Bath",
      university == "university of bath"                         ~ "University of Bath",
      university == "Haverford College"                          ~ "Haverford College",
      university == "Massey University"                          ~ "Massey University",
      university == "Massey Albany"                              ~ "Massey University",
      university == "Massy University"                           ~ "Massey University",
      university == "Massey"                                     ~ "Massey University",
      university == "Swinburne University of Technology"         ~ "Swinburne University of Technology",
      university == "Swinburne Hawthorn"                         ~ "Swinburne University of Technology",
      university == "Swinburne"                                  ~ "Swinburne University of Technology",
      university == "Swinburne University"                       ~ "Swinburne University of Technology",
      university == "Swinburne UofT"                             ~ "Swinburne University of Technology",
      university == "Swinburne university"                       ~ "Swinburne University of Technology",
      university == "swinburne"                                  ~ "Swinburne University of Technology",
      university == "Swinburne hawthorn"                         ~ "Swinburne University of Technology",
      university == "Swinburne University Of Technology"         ~ "Swinburne University of Technology",
      university == "Swinburne university of technology"         ~ "Swinburne University of Technology",
      university == "University of Queensland"                   ~ "University of Queensland",
      university == "The University of Queensland"               ~ "University of Queensland",
      university == "University of QLD"                          ~ "University of Queensland",
      university == "University of Queensland Saint Lucia"       ~ "University of Queensland",
      university == "UQ"                                         ~ "University of Queensland",
      university == "the university of Queensland"               ~ "University of Queensland",
      university == "university of Queensland"                   ~ "University of Queensland",
      university == "UQ - University of Queensland"              ~ "University of Queensland",
      university == "university of QLD"                          ~ "University of Queensland",
      university == "university of Queensland"                   ~ "University of Queensland",
      university == "UNIVERSITY OF QUEENSLAND"                   ~ "University of Queensland",
      university == "The University of Queensland (St Lucia)"    ~ "University of Queensland",
      university == "university of queensland"                   ~ "University of Queensland",
      university == "the University of Queensland"               ~ "University of Queensland",
      university == "OPEN UNIVERSITY AUSTRALIA"                  ~ "Open Universities Australia",
      university == "Open Universities Australia"                ~ "Open Universities Australia",
      university == "UWE Bristol"                                ~ "University of the West of England, Bristol",
      university == "Bristol UWE"                                ~ "University of the West of England, Bristol",
      university == "University of the West of England"          ~ "University of the West of England, Bristol",
      university == "University West of England"                 ~ "University of the West of England, Bristol",
      university == "University of the West of England Bristol"  ~ "University of the West of England, Bristol",
      university == "U1" ~ "U1",
      university == "-" ~ NA_character_
    )
  )

## Recode degrees using the rule below
# Note: hons and advanced degrees are collapsed into the more generic degree type (e.g. BPsych and BPsych Hons are both categorised as BPsych)

df <- df %>%
  mutate(degree_recoded = toTitleCase(degree)) %>%
  mutate(
    degree_recoded = case_when(
      degree_recoded == 'Bachelor of Arts' ~ 'Bachelor of Arts',
      degree_recoded == 'Bachelor of Arts (Advanced)' ~ 'Bachelor of Arts',
      degree_recoded == 'Bachelor of Arts & Social Science' ~ 'Bachelor of Arts & Social Science',
      degree_recoded == 'BA Psychology Sciences' ~ 'Bachelor of Arts (Psychological Science)',
      degree_recoded == 'BA-PSY' ~ 'Bachelor of Arts (Psychology)',
      degree_recoded == 'Bachelor of Arts / Bachelor of Business Management' ~ 'Bachelor of Arts/Bachelor of Business Management',
      degree_recoded == 'Bachelor of Arts/Bachelor of Laws' ~ 'Bachelor of Arts/Bachelor of Laws',
      degree_recoded == 'Bachelor of Laws and Bachelor of Arts' ~ 'Bachelor of Arts/Bachelor of Laws',
      degree_recoded == 'Bachelor of Media with a Bachelor of Arts' ~ 'Bachelor of Arts/Bachelor of Media',
      degree_recoded == 'Bachelor of Arts/Bachelor of Education (Secondary)' ~ 'Bachelor of Arts/Bachelor of Teaching',
      degree_recoded == 'Bachelor of Aviation' ~ 'Bachelor of Aviation',
      degree_recoded == 'Bachelor of Behavioural Studies' ~ 'Bachelor of Behavioural Studies',
      degree_recoded == 'Bachelor of Biomedical Science' ~ 'Bachelor of Biomedical Science',
      degree_recoded == 'Bachelor of Commerce' ~ 'Bachelor of Commerce',
      degree_recoded == 'Bachelor of Commerce' ~ 'Bachelor of Commerce',
      degree_recoded == 'Bachelor of computer science' ~ 'Bachelor of Computer Science',
      degree_recoded == 'Bachelor of Computer Science' ~ 'Bachelor of Computer Science',
      degree_recoded == 'Bachelor of Criminology' ~ 'Bachelor of Criminology',
      degree_recoded == 'Bachelor of Criminology & Criminal Justice' ~ 'Bachelor of Criminology and Criminal Justice',
      degree_recoded == 'Bachelor of Criminology and Criminal Justice' ~ 'Bachelor of Criminology and Criminal Justice',
      degree_recoded == 'Bachelor of Criminology and Criminal Justice (Honours)' ~ 'Bachelor of Criminology and Criminal Justice',
      degree_recoded == 'Bachelor of Health and Medical Science' ~ 'Bachelor of Health and Medical Sciences',
      degree_recoded == 'Bachelor of Health and Medical Sciences' ~ 'Bachelor of Health and Medical Sciences',
      degree_recoded == 'Bachelor of Health and Medical Sciences (Advanced)' ~ 'Bachelor of Health and Medical Sciences',
      degree_recoded == 'Bachelor of Health Science' ~ 'Bachelor of Health Sciences',
      degree_recoded == 'Bachelor of Health Sciences' ~ 'Bachelor of Health Sciences',
      degree_recoded == 'Bachelor of Health Sciences/Bachelor of Business' ~ 'Bachelor of Health Sciences/Bachelor of Business',
      degree_recoded == 'Bachelor of International Business' ~ 'Bachelor of International Business',
      degree_recoded == 'Bachelor of Occupational Therapy' ~ 'Bachelor of Occupational Therapy',
      degree_recoded == 'Bachelor of Occupational Therapy Honour' ~ 'Bachelor of Occupational Therapy',
      degree_recoded == 'Bachelor of Occupational Therapy Honours' ~ 'Bachelor of Occupational Therapy',
      degree_recoded == 'Bachelor of Physiotherapy' ~ 'Bachelor of Physiotherapy',
      degree_recoded == 'Bachelor of Physiotherapy (Honour)' ~ 'Bachelor of Physiotherapy',
      degree_recoded == 'Bachelor of Physiotherapy (Honours)' ~ 'Bachelor of Physiotherapy',
      degree_recoded == 'B Psychological Science' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'BACHELOR of PSYCHOLIGICAL SCIENCES' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Science' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Science (Advanced)' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Science (Honours)' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Sciences' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Sciences (Honours)' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychological Sciences (Hons)' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychology Science' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Psychological Science' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Psychological Science (Honours)' ~ 'Bachelor of Psychological Science',
      degree_recoded == 'Bachelor of Psychology' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology (Advanced) (Honours)' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology (Honours)' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology (Honours) (Advanced)' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology (Honours, Advanced)' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology Advanced Honours' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology Advanced with Honours' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology Honours' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor Psychology' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psych (Advanced) Honours' ~ 'Bachelor of Psychology',
      degree_recoded == 'Honours Degree of Bachelor of Psychology (Advanced)' ~ 'Bachelor of Psychology',
      degree_recoded == 'Bachelor of Psychology with Criminology' ~ 'Bachelor of Psychology with Criminology',
      degree_recoded == 'Bachelor of Science' ~ 'Bachelor of Science',
      degree_recoded == 'Bachelor of Science (Advanced)' ~ 'Bachelor of Science',
      degree_recoded == 'Bachelor of Science (Honours)' ~ 'Bachelor of Science',
      degree_recoded == 'Bachelor of Science (Hons)' ~ 'Bachelor of Science',
      degree_recoded == 'BSc' ~ 'Bachelor of Science',
      degree_recoded == 'BSC' ~ 'Bachelor of Science',
      degree_recoded == 'BSC Psychology with Placement Year' ~ 'Bachelor of Science',
      degree_recoded == 'BSc with Placement Year' ~ 'Bachelor of Science',
      degree_recoded == 'BSci' ~ 'Bachelor of Science',
      degree_recoded == 'Bachelor of Science, Animal Behaviour' ~ 'Bachelor of Science (Animal Behaviour)',
      degree_recoded == 'Bachelor of Science (Psychology)' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Bachelor of Science Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Bsc (Hons) Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc Hons Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc in Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSC in Psychology (Hons)' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc of Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Bsc Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc(Hons) Psychology' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Psychology BSc' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Psychology BSc with Placement Year' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Psychology Bsci' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'Psychology, Bachelor of Science' ~ 'Bachelor of Science (Psychology)',
      degree_recoded == 'BSc Psychology with Criminology' ~ 'Bachelor of Science (Psychology) with Criminology',
      degree_recoded == 'Bachelor of Science and Bachelor of Teaching Double Degree' ~ 'Bachelor of Science/Bachelor of Teaching',
      degree_recoded == 'Bachelor of Secondary Teaching and Bachelor of Science' ~ 'Bachelor of Science/Bachelor of Teaching',
      degree_recoded == 'Bachelor of Social Sciences' ~ 'Bachelor of Social Sciences',
      degree_recoded == 'Bachelor of Social Work' ~ 'Bachelor of Social Work',
      degree_recoded == 'Bachelor of Social Work (Honours)' ~ 'Bachelor of Social Work',
      degree_recoded == 'Bachelor of Sociology' ~ 'Bachelor of Sociology',
      degree_recoded == 'Bachelor of Speech Pathology (Hons)' ~ 'Bachelor of Speech Pathology',
      degree_recoded == 'Bachelor of Teaching' ~ 'Bachelor of Teaching',
      degree_recoded == 'Integrated Masters in Science' ~ 'Master of Science',
      degree_recoded == 'MSci' ~ 'Master of Science',
      degree_recoded == 'It will be a Degree in Psychology but We\'re Only Doing the First Two Modules to Get Us into the Course at the Moment.' ~ 'Non-degree study',
      degree_recoded == 'Psy10007' ~ 'Non-degree study',
      degree_recoded == 'SIngle Subjects, Towards a Bachelor of Behavioural Studies' ~ 'Non-degree study',
      degree_recoded == 'Undergraduate Taught Masters in Psychology' ~ 'Undergraduate Taught Masters in Psychology',
      degree_recoded == 'Bachelor\'s Degree' ~ 'Unspecified Bachelor\'s Degree',
      degree_recoded == 'Undergraduate Master\'s Degree' ~ 'Unspecified Master\'s Degree'
    ))

## Create a new variable for degree type
# For participants with a recoded degree that contains the string "sycholog" code as "Psychology"
# For participants with a recoded degree that does not contain the string "sycholog" as "Non-psychology"

df <- df %>%
  mutate(degree_type = case_when
         (
           str_detect(degree_recoded, "sycholog") ~ "Psychology",
           is.na(degree_recoded) ~ NA_character_,
           TRUE ~ "Non-psychology"
         ))

# create a new variable for major type
# For participants who mention a major in psychology, code as "psychology"
# For participants who do not, code as "not psychology"
# Code all students in a psychology degree as having a psychology major

df <- df %>%
  mutate(major_recoded = toTitleCase(major)) %>%
  mutate(
    major_recoded = case_when(
      major == "Accounting"              ~ 'Not psychology',
      major == "Brain and behaviour"       ~ 'Not psychology',
      major == "Chemistry and psychology"      ~ "Psychology" ,
      major == "Computer science"         ~ 'Not psychology',
      major == "computer science"       ~ 'Not psychology',
      major == "Criminology / sociology"    ~ 'Not psychology',
      major == "criminology and psychology"      ~ "Psychology" ,
      major == "criminology"        ~ 'Not psychology',
      major == "Degree does not allow majors"    ~ 'Not psychology',
      major == "genetics"               ~ 'Not psychology',
      major == "It will be a degree in psychology with a minor in criminology."  ~ "Psychology"  ,
      major == "Major Film and television studies and for minor psychology and criminology"  ~ 'Not psychology',
      major == "No major with my course"   ~ 'Not psychology',
      major == "nutrition"         ~ 'Not psychology',
      major == "Our course only allows several electives and does not allow a major"  ~ "Psychology" ,
      major == "psychology and education" ~ 'Psychology',
      major == "PSychology and sociology"   ~ "Psychology"     ,
      major == "psychology" ~ "Psychology"    ,
      major == "Psychology, Political science, Human resources"    ~ "Psychology"    ,
      major == 'Animal Behaviour' ~ 'Not psychology',
      major == 'Anthropology' ~ 'Not psychology',
      major == 'Aviation Technology, Aviation Management' ~ 'Not psychology',
      major == 'Biology' ~ 'Not psychology',
      major == 'Brain and Behaviour' ~ 'Not psychology',
      major == 'Chemestry' ~ 'Not psychology',
      major == 'Chemistry' ~ 'Not psychology',
      major == 'Chemistry and Psychology' ~ 'Psychology',
      major == 'Computer Science' ~ 'Not psychology',
      major == 'Criminology / Sociology' ~ 'Not psychology',
      major == 'Criminology' ~ 'Not psychology',
      major == 'Criminology and Criminal Justice' ~ 'Not psychology',
      major == 'Criminology and Psychology' ~ 'Psychology',
      major == 'Degree Does not Allow Majors' ~ 'Not psychology',
      major == 'Ecology and Sustainability' ~ 'Not psychology',
      major == 'English & Writing' ~ 'Not psychology',
      major == 'French' ~ 'Not psychology',
      major == 'Genetics' ~ 'Not psychology',
      major == 'German and Spanish' ~ 'Not psychology',
      major == 'Introduction to Psychology & Research Methods in Psychology' ~ NA_character_,
      major == 'It will be a Degree in Psychology with a Minor in Criminology.' ~ 'Psychology',
      major == 'Japanese and Linguistics' ~ 'Not psychology',
      major == 'Journalism & Psychology' ~ 'Psychology',
      major == 'Major Film and Television Studies and for Minor Psychology and Criminology' ~ 'Not-psychology',
      major == 'Major in Psychological Studies, Minor in Criminology' ~ 'Psychology',
      major == 'Major in Psychology' ~ 'Psychology',
      major == 'Major in Psychology and Co Major in Cinema and Screen Studies' ~ 'Psychology',
      major == 'Marine Biology' ~ 'Not psychology',
      major == 'Medical Science' ~ 'Not psychology',
      major == 'Microbiology' ~ 'Not psychology',
      major == 'n/a' ~ 'Not psychology',
      major == 'Neurology' ~ 'Not psychology',
      major == 'Neuropsychology' ~ 'Psychology',
      major == 'Neuroscience and or Microbiology' ~ 'Not psychology',
      major == 'Neurosciences' ~ 'Not psychology',
      major == 'No Major with My Course' ~ 'Not psychology',
      major == 'Nutrition' ~ 'Not psychology',
      major == 'Nutrition/Entrepreneurship and Innovation' ~ 'Not psychology',
      major == 'Occupation Therapy' ~ 'Not psychology',
      major == 'Our Course Only Allows Several Electives and Does not Allow a Major' ~ 'Not psychology',
      major == 'Physics' ~ 'Not psychology',
      major == 'Physiotherapy' ~ 'Not psychology',
      major == 'Plant Science' ~ 'Not psychology',
      major == 'Psychological Science' ~ 'Psychology',
      major == 'Psychology (Extended Major)' ~ 'Psychology',
      major == 'Psychology' ~ 'Psychology',
      major == 'Psychology and Creative Writing' ~ 'Psychology',
      major == 'Psychology and Economics' ~ 'Psychology',
      major == 'Psychology and Education' ~ 'Psychology',
      major == 'Psychology and English' ~ 'Psychology',
      major == 'PSychology and Sociology' ~ 'Psychology',
      major == 'Psychology, Political Science, Human Resources' ~ 'Psychology',
      major == 'Psycholohy' ~ 'Psychology',
      major == 'Sociology & Psychology' ~ 'Psychology',
      major == 'Sociology' ~ 'Not psychology',
      major == 'Speech Pathology' ~ 'Not psychology',
      major == 'Sport Studies' ~ 'Not psychology',
      major == "Biology, Anatomy, Psychology, Physiotherapy"  ~ 'Not psychology',
      major == "blank"  ~ 'Not psychology',
      major == "International Relations"  ~ 'Not psychology',
      major == "Marketing"  ~ 'Not psychology',
      major == "Medical science"  ~ 'Not psychology',
      major == "psychological science and law" ~ "Psychology",
      major == "Psychology Extended Major and Philosophy, Spanish" ~ "Psychology",
      major == "PSYCHOLOGY/ENVIRONMENT AND SOCIETY" ~ "Psychology",
      major == "Neuroscience" ~ "Psychology",
      major == "-" ~ "Psychology",
      is.na(major_recoded) ~ NA_character_,
      degree_type == "Psychology" ~ "Psychology",
      TRUE ~ "Uncoded"
    ))

df <- df %>%
  mutate(major_recoded = case_when(degree_type == "Psychology" ~ "Psychology",
         TRUE ~ major_recoded))

################### WRITE DATA TO CSV #############

# when done preprocessing, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "students_processed.csv"), row.names = FALSE)



### CODING REPLICATION CRISIS QUALTITATIVE RESPONSES

# write this one variable to a csv file for coding
write.csv(data.frame("crisis_text" = df$crisis_text[is.na(df$crisis_text)==FALSE]), here::here( "data", "crisis_descriptions.csv"))

#written for manual coding, which is then used in Rmd
