# Cleaning Data ----------------------------------------------------------

## 1. clear up the environment ----------------------------------------------------
graphics.off()
rm(list = ls())
freshr::freshr()

library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(gtsummary, quietly = TRUE)

## 2. loading dataset -------------------------------------------------------------

caregiver_score <- read.csv("new_data/score_caregiver_survey_scored.csv")
caregiver_score_baseline <- caregiver_score %>% 
  filter(time == 0) %>%
  dplyr::select(-c(time:hads))

patient_score <- read.csv("new_data/score_patient_survey_scored.csv") 
patient_score_baseline <- patient_score %>%
  filter(time == 0) %>% 
  dplyr::select(-c(time:hads))



## extract the ids we have at the score datasets 
c_score_id <- caregiver_score$uniqueid2
p_score_id <- patient_score$uniqueid2

## only include the patients and caregivers in the score datasets
caregiver_survey <- read.csv("new_data/survey_comments_caregiver.csv") %>%
  filter(redcap_event_name == "baseline_arm_1",
         uniqueid %in% c_score_id) %>%
  dplyr::select(uniqueid, age, ethnicity, 
                reason, # contains('cancer'), 
                employed, retired, income, household, education,
                caregiver, relationship, hours, contains("race")) 

patient_survey <- read.csv("new_data/survey_comments_patient.csv") %>%
  filter(redcap_event_name == "baseline_arm_1",
       uniqueid %in% p_score_id) %>% 
  dplyr::select(uniqueid, age, # gender, ethnicity, 
                marital.p = marital, 
                # surgery, chemo, radiation, biological, 
                employed, retired, income, household, education) 



## the tracking contains some parts for the demographic information
caregiver_tracking <- read_csv("new_data/tracking_full_caregiver.csv") %>%
  filter(partnerid %in% c_score_id) %>%
  dplyr::select(pid = upi,
                cid = partnerid, age, 
                # language, # gender, gender_clean, 
                ethnicity, ethnicity_clean, 
                race,
                diagnosis, stage, step_baseline)

patient_tracking <- read_csv("new_data/tracking_full_patient.csv") %>%
  filter(upi %in% p_score_id) %>%
  dplyr::select(pid = upi, 
                race,
                # language, # gender, gender_clean, 
                age, # ethnicity, ethnicity_clean, race, hnc, lc, 
                # diagnosis, 
                step,
                contains("partnerid"))


## 3. merge the dataset ------------------------------------------------------------
## there are two parts of the demographic informations in both 
## survey and tracking datasets
patient <- full_join(patient_survey, patient_tracking, 
                     by = c("uniqueid" = "pid"),
                     suffix = c(".psurvey", ".ptracking")) %>%
  full_join(patient_score_baseline, by =c("uniqueid" = "uniqueid2"), 
            suffix = c("", ".score")) %>%
  dplyr::select(-contains("_rand"), -record_id) %>%
  dplyr::select(pid = uniqueid, everything()) %>%
  mutate(pid = as.character(pid))

## clean the dataset for table1, all variables are in the character 
caregiver <- full_join(caregiver_survey, caregiver_tracking,
                       by = c("uniqueid" = "cid"),
                       suffix = c(".survey", ".tracking")) %>%
  full_join(caregiver_score_baseline, by =c("uniqueid" = "uniqueid2"), 
            suffix = c("", ".score")) %>%
  mutate(age = ifelse(is.na(age.survey), age.tracking, age.survey),
         pid = as.character(pid)) %>% 
  dplyr::select(cid = uniqueid, spanish,
                age.caregiver = age, 
                caregiver, 
                contains("race"),
                contains("ethnicity"), 
                diagnosis = diagnosis.score, 
                education, employed, retired, 
                gender_clean, hospital, 
                hours, household, income, race, 
                randomize,
                reason, relationship, stage = stage.score,
                stage2, step_baseline)

## 4. pair the datasets----------------------------------------------------------------------
# ## patient number 286 patients
# p_cid <- c(patient$partnerid1, patient$partnerid2, 
#            patient$partnerid3, patient$partnerid4,
#            patient$partnerid5) 
# ## caregiver number 239 caregivers
# p_cid <- caregiver_id[!is.na(p_cid)]; length(p_cid)
# ## caregiver number 249 caregivers
# nrow(caregiver)
# 
# ## 239 pairs, patient are correct
# p_pair <- patient %>%
#   dplyr::select(pid, contains("partnerid")) %>%
#   pivot_longer(cols = contains("partnerid"), 
#                values_to = "cid",
#                values_drop_na = TRUE) %>%
#   dplyr::select(-name)
# ## 249 pairs, caregiver are correct
# c_pair <- caregiver %>%
#   dplyr::select(cid, pid)
# 
### 4.1 below will be the unpaired ==========================================
# `%notin%` <- Negate(`%in%`)
# ## effective pairs are 216 -> 219
# e_pair <- inner_join(c_pair, p_pair)
# write_csv(e_pair, file = "new_data/perfect_pair_fixed_2023-09-28.csv")
#
# p_unpair1 <- patient %>%
#   filter(is.na(partnerid1)) %>% 
#   dplyr::select(pid, cid = partnerid1)
# nrow(p_unpair1)
# p_unpair2 <- p_pair %>%
#   filter(pid %notin% e_pair$pid) %>%
#   dplyr::select(pid, cid) 
# nrow(p_unpair2); p_unpair2
# ## there are 100 unique 
# p_unpair <- rbind(p_unpair1, p_unpair2) %>% unique()
# print(p_unpair)
# 
### 4.2 problem with the data ==========================================
# ## those are the people with a cid 
# ## but probably not in the caregiver data
# # 4004 6005 6038 6057 4033 6088 8028 
# # 6086    1  7041 6097 6163 6055
# ## extra pair with cid4033 with pid3047
# ## extra pair with cid0001 with pid9003
# ## extra pair with cid6163 with pid5187
# p_error <- caregiver %>% filter(cid %in% p_unpair$cid | pid %in% p_unpair$pid) 
#
### 4.3 manually change the data ==========================================
# 
# c_unpair1 <- c_pair %>%
#   filter(is.na(pid)) %>%
#   dplyr::select(cid, pid) 
# c_unpair2 <- c_pair %>%
#   filter(cid %notin% e_pair$cid) %>%
#   dplyr::select(cid, pid) 
# c_unpair <- rbind(c_unpair1, c_unpair2)  %>% unique()
# c_error <- patient %>% 
#  filter(pid %in% c_unpair$pid |
#  partnerid1 %in% c_unpair$cid |
#  partnerid2 %in% c_unpair$cid | 
#  partnerid3 %in% c_unpair$cid |
#  partnerid4 %in% c_unpair$cid | 
#  partnerid5 %in% c_unpair$cid)

pair <- read_csv("new_data/perfect_pair_fixed_2023-09-28.csv") %>%
  mutate(pid = as.character(pid))


## 5. fill out NAs ----------------------------------------------------------------
# comments_caregiver <- read_csv("new_data/survey_comments_caregiver.csv") %>%
#   dplyr::filter(redcap_event_name == "baseline_arm_1") %>%
#   dplyr::select(reason, other_reason, relationship, contains("other"), speak, read, everything()) 

patient_caregiver <- full_join(caregiver, pair, by = "cid") %>% 
  full_join(patient, by = "pid",
            suffix = c(".caregiver", ".patient")) %>%
  dplyr::select(pid, cid, order(names(.)))

# caregiver_table1 ------------------------------------------------------------
caregiver_table_1 <- patient_caregiver %>%
  filter(cid %in% c_score_id) %>%
  transmute(cid, pid, 
         ##
         age = case_when(is.na(age.caregiver) ~ age.psurvey,
                         !is.na(age.caregiver) ~ age.caregiver),
         ## there is 1 NA but couple with patient 74 and retired
         ##
         gender = gender_clean.caregiver, 
         ## 
         language = case_when(is.na(language) & spanish.caregiver == 1 ~ 1,
                              is.na(language) & spanish.caregiver == 0 ~ 2,
                              .default = language), 
         # spanish = spanish.caregiver,
         ## language is filled out with both the survey data language
         ## also the tracking dataset speak and read, 
         ## also with the spanish and english survey form
         ##
         ### notes Tue Oct 31 20:01:20 2023 ------------------------------
         ethnicity = case_when(is.na(ethnicity_clean) ~ 1,
                               !is.na(ethnicity_clean) ~ ethnicity_clean),
         ## ethnicity 1 NA clean 1 nonhispanic; 2 hispanic
         ## using her husband's ethnicity cannot speak in spanish
         ## 
         race = race.caregiver,
         ##
         primary_caregiver = ifelse(is.na(caregiver), 1, caregiver),
         ## caregiver 2 NAs are daughter and the only caregivers for the patient
         ## so filled out as the primary caregiver
         primary_reason = case_when(!is.na(reason) ~ reason,
                                    is.na(reason) ~ -1),
         ## reason 5 NAs, create new values for missing
         ## cared for the patient for any reason
         ## or illness that is not related to their cancer treatment?
         ## 1 yes; 0 no; -1 missing as factor
         ##
         hours = case_when(is.na(hours) ~ 0,
                           !is.na(hours) ~ hours),
         household = case_when(is.na(household.caregiver) ~ 0,
                               !is.na(household.caregiver) ~ household.caregiver),
         ## household has 6 NAs just left as missing 
         ##
         relationship = case_when(cid == 4033 ~ 2,
                                  .default = relationship),
         ## one caregiver as partner mislabeled as non-relative 
         ##
         employed = employed.caregiver,
         ##
         retired = retired.caregiver,
         ## retired will be removed but keep employed
         ## 
         education = education.caregiver,
         ## education 1 NA the caregiver does not have patient in dataset
         ## age 38 female non-primary employed caregiver 
         ## most other caregivers are with college degree
         ## 
         income = case_when(cid == 2057 ~ 1,
                            pid == 1042 ~ 1,
                            is.na(income.caregiver) ~ 0,
                            !is.na(income.caregiver) ~ income.caregiver),
         ## 
         randomize = randomize.caregiver,
         ## 
         diagnosis = diagnosis.caregiver, 
         ##
         stage = case_when(!is.na(stage.caregiver) ~ stage.caregiver,
                           is.na(stage.caregiver) & is.na(stage.patient) ~ 5,
                           is.na(stage.caregiver) ~ stage.patient),
         ## the stage 10 NAs for missing in the level5
         ## will be converted into binary early and late
         ##
         step = step_baseline,
         ## the step information is not consistent between 
         ## patient and caregiver 
         ##
         hospital = hospital.caregiver)


# caregiver_table2 ------------------------------------------------------------

caregiver_table_2 <- caregiver_table_1 %>%
  transmute(cid = as.character(cid), pid, age, # numeric values 
            age_binary = case_when(age < 60 ~ 2, # young
                                   age >= 60 ~ 1), # old
            gender, # 1 male; 2 female
            language = case_when(language == 1 ~ 1, # spanish only
                                 language == 2 | language == 3  ~ 2), # english or both
            ethnicity, # 1 non-hispanic; 2 hispanic 
            race = case_when(race == 1 ~ 1, ## white
                             race != 1 ~ 2, ## other
                             is.na(race) ~ 0), ## missing
            primary_caregiver = case_when(primary_caregiver == 0 ~ 2, # no
                                          primary_caregiver == 1 ~ 1), # yes  
            primary_reason = case_when(primary_reason == 0 ~ 2, # no
                                       primary_reason == 1 ~ 1, # yes
                                       primary_reason == -1 ~ 0),
            hours = case_when(hours == 0 ~ 0, # missing
                              hours <= 2 ~ 1, # less than 10
                              hours > 2 ~ 2), # more than 10 less than 20
            household = case_when(household == 0 ~ 0, # unknown missing
                                  household == 1 ~ 1, # single 
                                  household == 2 ~ 2, # two 
                                  household > 2 ~ 3), # more than two
            relationship = case_when(relationship %in% c(1, 2) ~ 1, # couple
                                     relationship %in% c(3, 4, 5, 6) ~ 2, # parent_children
                                     relationship %in% c(7, 8, 9, 10) ~ 3,  # others
                                     is.na(relationship) ~ 0), # missing
            employed = case_when(employed == 0 ~ 2,
                                 employed == 1 ~ 1),  
            # retired,
            education = case_when(is.na(education) ~ 0, # missing
                                  education <= 12 ~ 1, # high school
                                  education > 12 ~ 2), # college level
            ## education 1 NA the caregiver does not have patient in dataset
            ## age 38 female non-primary employed caregiver 
            ## most other caregivers are with college degree
            income = case_when(income == 0 ~ 0, # missing
                               income == 1 ~ 1, # lower than 4000
                               income > 1 ~ 2), # higher 4000
            randomize = case_when(randomize == 0 ~ 0, # control
                                  randomize == 1 ~ 1), # treatment
            diagnosis = case_when(diagnosis == 1 ~ 1, # lung cancer
                                  diagnosis %in% c(2, 3, 4) ~ 2), # head neck cancer
            stage = case_when(stage %in% c(0, 1, 2) ~ 1, # early
                              stage %in% c(3, 4) ~ 2, # late 
                              stage == 5 ~ 0), # pending or missing 
            step = case_when(step %in% c(1, 2) ~ 1, # early
                             step %in% c(3, 4) ~ 2, # late
                             step == 5 ~ 0), # pending or missing
            hospital = hospital) %>% # 1 Denver Health; 2 St.Joe’s;
                                     # 3 St.Mary’s; 4 National Jewish; 5 UCCC
  mutate(age_binary = as.factor(age_binary),
         gender = as.factor(gender),
         ethnicity = as.factor(ethnicity),
         language = as.factor(language),
         race = as.factor(race),
         primary_caregiver = as.factor(primary_caregiver),
         primary_reason = as.factor(primary_reason),
         hours = as.factor(hours),
         household = as.factor(household),
         relationship = as.factor(relationship),
         employed = as.factor(employed),
         education = as.factor(education),
         income = as.factor(income),
         randomize = as.numeric(randomize),
         diagnosis = as.factor(diagnosis),
         stage = as.factor(stage),
         step = as.factor(step),
         hospital = as.factor(hospital))


caregiver_table_3 <- caregiver_table_2 %>%
  transmute(cid = as.character(cid), pid, age, # numeric values 
            age_binary = case_when(age_binary == 2 ~ "Young than 60", # young
                                   age_binary == 0 ~ "Missing",
                                   age_binary == 1 ~ "Older than 60"), # old
            gender = case_when(gender == 1 ~ "Male",
                               gender == 0 ~ "Missing",
                               gender == 2 ~ "Female"), # 1 male; 2 female
            language = case_when(language == 1 ~ "Spanish", # spanish only
                                 language == 0 ~ "Missing",
                                 language == 2 ~ "English or Both"), # english or both
            ethnicity = case_when(ethnicity == 1 ~ "Non-Hispanic",
                                  ethnicity == 0 ~ "Missing",
                                  ethnicity == 2 ~ "Hispanic"), # 1 non-hispanic; 2 hispanic 
            race_white = case_when(race == 1 ~ "White", ## white
                                   race == 2 ~ "Other",  ## other
                                   race == 0 ~ "Missing"),
            primary_caregiver = case_when(primary_caregiver == 2 ~ "No",
                                          primary_caregiver == 1 ~ "Yes",
                                          primary_caregiver == 0 ~ "Missing"), 
            primary_reason = case_when(primary_reason == 2 ~ "No",
                                       primary_reason == 1 ~ "Yes",
                                       primary_reason == 0 ~ "Missing"),
            hours = case_when(hours == 0 ~ "Missing", # missing
                              hours == 1 ~ "Less than 10 hours", # less than 10
                              hours == 2 ~ "More than 10 hours"), # more than 10 less than 20
            household = case_when(household == 0 ~ "Missing", # unknown
                                  household == 1 ~ "One", # single 
                                  household == 2 ~ "Two", # two 
                                  household == 3 ~ "More than two"), # more than two
            relationship = case_when(relationship == 1 ~ "Couples or partners", # couple
                                     relationship == 0 ~ "Missing",
                                     relationship == 2 ~ "Parents or Children", # parent_children
                                     relationship == 3 ~ "Other relationships"), # others
            employed = case_when(employed == 2 ~ "No",
                                 employed == 0 ~ "Missing",
                                 employed == 1 ~ "Yes"), 
            # retired,
            education = case_when(education == 0 ~ "Missing", # not in college
                                  education == 1 ~ "High school or lower",
                                  education == 2 ~ "College or graduate school"), # college level
            ## education 1 NA the caregiver does not have patient in dataset
            ## age 38 female non-primary employed caregiver 
            ## most other caregivers are with college degree
            income = case_when(income == 0 ~ "Missing", # missing
                               income == 1 ~ "Less than 4000", # lower than 4000
                               income == 2 ~ "More than 4000"), # higher 4000
            randomize = case_when(randomize == 0 ~ "Usual care",
                                  randomize == 1 ~ "Missing",
                                  randomize == 1 ~ "Intervention"),
            diagnosis = case_when(diagnosis == 1 ~ "Lung cancer", # lung cancer
                                  diagnosis == 0 ~ "Missing",
                                  diagnosis == 2 ~ "Head & Neck cancer"), # head neck cancer
            stage = case_when(stage == 1 ~ "Early", # early
                              stage == 2 ~ "Late", # late 
                              stage == 0 ~ "Missing"), # pending or missing 
            step = case_when(step == 1 ~ "Early", # early
                             step == 2 ~ "Late", # late
                             step == 0 ~ "Missing"), # pending or missing
            hospital = case_when(hospital == 1 ~ "Denver Health",
                                 hospital == 2 ~ "St.Joes",
                                 hospital == 3 ~ "St.Marys",
                                 hospital == 4 ~ "National Jewish",
                                 hospital == 5 ~ "UCCC"))
# View(caregiver_table_1)
# View(caregiver_table_2)
# View(caregiver_table_3)


save(caregiver_table_1, file = paste0("new_data/demo_clean_caregiver_original_1_", 
                                           Sys.Date(), ".Rdata"))
save(caregiver_table_2, file = paste0("new_data/demo_clean_caregiver_merge_2_", 
                                           Sys.Date(), ".Rdata"))
save(caregiver_table_3, file = paste0("new_data/demo_clean_caregiver_factor_2_", 
                                           Sys.Date(), ".Rdata"))


# Old codes --------------------------------------------------------------------
# ## the dataset for standardized difference calculation
# caregiver_std <- caregiver_std %>%
#   mutate(Randomize = case_when(Randomize == "Control" ~ 0,
#                                Randomize == "Intervention" ~ 1),
#          Caregiver = case_when(Caregiver == "Primary" ~ 1,
#                                Caregiver == "Non-primary" ~ 0),
#          `Primary Reason` = case_when(`Primary Reason` == "Yes" ~ 1,
#                                       `Primary Reason` == "No" ~ 0),
#          Employed = case_when(Employed == "Yes" ~ 1,
#                               Employed == "No" ~ 0),
#          Retired = case_when(Retired == "Yes" ~ 1,
#                              Retired == "No" ~ 0),
#          # `Age Group` = case_when(Age < 18 ~ 1,
#          #                          Age <= 65 & Age >= 18 ~ 2,
#          #                          Age > 65 ~ 3),
#          Language = case_when(Language == "English" ~ 0,
#                               Language == "Spanish" ~ 1),
#          Gender = case_when(Gender == "Male" ~ 0,
#                             Gender == "Female" ~ 1),
#          Ethnicity = case_when(Ethnicity == "Hispanic" ~ 0,
#                                Ethnicity == "Non-Hispanic" ~ 1),
#          Race = case_when(Race == "Caucasian" ~ 1,
#                           Race == "Others" ~ 0),
#          Diagnosis = case_when(Diagnosis == "Lung cancer" ~ 1,
#                                Diagnosis == "Head and neck cancer" ~ 2),
#          Stage = case_when(Stage == "Early" ~ 1,
#                            Stage == "Late" ~ 2),
#          Step = case_when(Step == "Step 1&2" ~ 1,
#                           Step == "Step 3&4" ~ 2),
#          Income = case_when(Income == "1. Less than $4,000" ~ 1,
#                             Income == "2. Less than $5,400" ~ 2,
#                             Income == "3. More than $5,400" ~ 3),
#          Education = case_when(Education == "1. High school or less" ~ 1,
#                                Education == "2. College or more" ~ 2),
#          Household = case_when(Household == "1. One" ~ 1,
#                                Household == "2. Two or Three" ~ 2,
#                                Household == "4. Four or more" ~ 3),
#          Relationship = case_when(Relationship == "1. Spouses or Partners" ~ 1,
#                                   Relationship == "2. Parents" ~ 2,
#                                   Relationship == "3. Children" ~ 3,
#                                   Relationship == "4. Relatives" ~ 4,
#                                   Relationship == "5. Others" ~ 5),
#          `Caring Hours` = case_when(`Caring Hours` == "1. Less than 5" ~ 1,
#                                     `Caring Hours` == "2. From 5 to 10" ~ 2,
#                                     `Caring Hours` == "3. From 10 to 20" ~ 3,
#                                     `Caring Hours` == "4. More than 20" ~ 4),
#          Hospital = case_when(Hospital == "Denver Health" ~ 1,
#                               Hospital == "St.Joes" ~ 2,
#                               Hospital == "St.Marys" ~ 3,
#                               Hospital == "National Jewish" ~ 4,
#                               Hospital == "UCCC" ~ 5)) %>%
#   dplyr::select(ID,                 # 1
#                 Randomize,          # 2
#                 Caregiver,          # 3 binary
#                 Primary.Reason,   # 4 binary
#                 Employed,           # 5 binary
#                 Retired,            # 6 binary
#                 Education,          # 7 binary
#                 Language,            # 8 binary
#                 Gender,             # 9 binary
#                 Ethnicity,           # 10 binary
#                 Race,              # 11 binary
#                 Diagnosis,          # 12 binary
#                 Stage,         # 13 binary
#                 Step,          # 14 binary
#                 Age,                # 15 numeric
#                 Income,             # 16 categorical
#                 Household,          # 17 categorical
#                 Relationship,       # 18 categorical
#                 Caring.Hours,     # 19 categorical
#                 # Insurance
#                 Hospital)           # 20 categorical
# caregiver[caregiver == "*missing"] <- NA
# 
# 
# ## this is the working dataset for the paper analysis
# caregiver_all <- caregiver_score %>%
#   dplyr::select(-record_id, -spanish, -randomize, -language,
#                 -stage, -stage2, -diagnosis, -diagn.lc, 
#                 -gender_clean, -hospital, -status2) %>%
#   full_join(caregiver, by = join_by(uniqueid2 == ID)) %>%
#   dplyr::select(ID = uniqueid2, Randomize, everything())
# 
# ## save the datasets in the newdata folder
# 
# write_csv(caregiver_all, file = paste0("new_data/data_table1_caregiver_all_paper_", Sys.Date(), ".csv"))
# write.csv(caregiver, file = paste0("new_data/data_table1_caregiver_binary_", Sys.Date(), ".csv"))
# write.csv(caregiver_std, file = paste0("new_data/data_table1_caregiver_numeric_std_", Sys.Date(), ".csv"))















