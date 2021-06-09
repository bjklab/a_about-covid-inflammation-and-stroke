#' load libraries and set seed
library(tidyverse)
library(readxl)
library(gt)

set.seed(16)



#' read data
rc_sub_dat <- read_csv("../ABOUT-COVID Abstract Data/ABOUTCOVIDSubjectPro_DATA_2021-06-02_1002.csv")

rc_enc_dat <- read_csv("../ABOUT-COVID Abstract Data/ABOUTCOVIDEncounterP_DATA_2021-06-02_1007.csv")

icd_dat <- read_csv("../ABOUT-COVID Abstract Data/about_covid_icd_out_20210521.csv.gz")

demo_dat <- read_csv("../ABOUT-COVID Abstract Data/about_covid_demo_out_20210521.csv.gz")

med_dat <- read_csv("../ABOUT-COVID Abstract Data/about_covid_inpt_meds_out_20210521.csv.gz")

lab_dat <- read_csv("../ABOUT-COVID Abstract Data/about_covid_labs_out_20210521.csv.gz")




#' primary outcome data 
rc_enc_dat %>%
  #count(encounter_profile_complete)
  filter(encounter_profile_complete == 2) %>%
  select(subject_id, index_date, vte_stroke) %>%
  group_by(subject_id) %>%
  summarise(index_date = min(as.Date(index_date, format = "%m/%d/%Y"),na.rm = TRUE),
         stroke = vte_stroke == 1) %>%
  ungroup() %>%
  identity() -> ac_stroke
ac_stroke



#' secondary outcome data 
rc_sub_dat %>%
  select(subject_id, ams, covid_symp_rash, covid_symp_breathing, covid_symp_fever, covid_symp_diarrhea) %>%
  mutate_at(.vars = vars(ams, contains("symp")), .funs = ~ .x == 1) %>%
  identity() -> ac_ams
ac_ams




#' primary exposure data: lab markers of inflammation
lab_dat %>%
  filter(subject_id %in% ac_stroke$subject_id) %>%
  mutate(lab_date = as.Date(ordering_dttm, format = "%Y-%m-%d"),
         index_date = as.Date(index_date, format = "%m/%d/%Y"),
         lab_value = as.numeric(ord_value),
         description = tolower(description)) %>%
  group_by(subject_id) %>%
  mutate(index_day_dif = as.numeric(lab_date - index_date)) %>%
  ungroup() %>%
  filter(abs(index_day_dif) < 7) %>%
  filter(grepl(pattern = "crp|c-reactive protein|esr|sedimentation|d-dimer|ferritin|fibrinogen", x = tolower(description))) %>%
  #count(description) %>% gt::gt()
  #filter(!is.na(ord_value)) %>% count(description)
  select(subject_id, description, lab_value) %>%
  filter(!is.na(lab_value)) %>%
  mutate(lab_name = case_when(grepl("crp",description) ~ "CRP",
                              grepl("dimer",description) ~ "D_dimer",
                              grepl("ferritin",description) ~ "Ferritin",
                              grepl("fibrino",description) ~ "Fibrinogen",
                              grepl("sedimentation",description) ~ "ESR",)) %>%
  select(subject_id, lab_name, lab_value) %>%
  filter(!is.na(lab_name)) %>%
  pivot_wider(id_cols = "subject_id", names_from = "lab_name", values_from = "lab_value", values_fn = max) %>%
  identity() -> ac_labs
ac_labs




#' secondary exposure data: demographics
demo_dat %>%
  select(subject_id, patient_age_years, patient_sex) %>%
  filter(subject_id %in% ac_stroke$subject_id) %>%
  identity() -> ac_demo
ac_demo



#' secondary exposure data: history
icd_dat %>%
  group_by(subject_id) %>%
  summarise(dx_dm2 = grepl("mellitus|dm2", paste0(tolower(dx_name), collapse = " ")),
            dx_htn = grepl("hypertension|htn", paste0(tolower(dx_name), collapse = " ")),
            dx_tobacco = grepl("tobacco|smoking", paste0(tolower(dx_name), collapse = " ")),
            dx_cad = grepl("coronary artery|myocardial infarction", paste0(tolower(dx_name), collapse = " ")),
            dx_pad = grepl("peripheral atery disease|peripheral aterial disease|pad", paste0(tolower(dx_name), collapse = " ")),
            ) %>%
  ungroup() %>%
  select(subject_id, contains("dx")) %>%
  filter(subject_id %in% ac_stroke$subject_id) %>%
  identity() -> ac_hx
ac_hx
  

ac_hx %>%
  summarise_at(.vars = vars(contains("dx")), .funs = ~ sum(.x, na.rm = TRUE))



#' secondary exposure data: antiplatelet and anticoagulant medications

tibble(med = c("aspirin","asa","clopidogrel","plavix","prasugrel","effient","ticagrelor","brilinta","apixaban","eliquis","dabigatran","pradaxa","edoxaban","lixiana","rivaroxaban","xarelto","warfarin","coumadin","heparin","lovenox"),
       med_class = c(rep("antiplatelet",8),rep("anticoagulant",12))) -> ac_med_key
ac_med_key


med_dat %>%
  filter(subject_id %in% ac_stroke$subject_id) %>%
  mutate(med_date = as.Date(taken_time, format = "%Y-%m-%d"),
         index_date = as.Date(index_date, format = "%m/%d/%Y")) %>%
  group_by(subject_id) %>%
  mutate(index_day_dif = as.numeric(med_date - index_date)) %>%
  ungroup() %>%
  filter(abs(index_day_dif) < 7) %>%
  filter(grepl(pattern = paste0(ac_med_key$med, collapse = "|"), x = tolower(med_name))) %>%
  mutate(med = stringr::str_extract(string = tolower(med_name), pattern = paste0(ac_med_key$med, collapse = "|"))) %>%
  left_join(ac_med_key, by = "med") %>%
  select(subject_id, med, med_class) %>%
  group_by(subject_id) %>%
  summarise(any_anticoagulant = sum(med_class == "anticoagulant",na.rm = TRUE) > 0,
            any_antiplatelet = sum(med_class == "antiplatelet",na.rm = TRUE) > 0) %>%
  ungroup() %>%
  identity() -> ac_meds
ac_meds


ac_meds %>%
  summarise_at(.vars = vars(contains("any")), .funs = ~ sum(.x, na.rm = TRUE))



#' join data by subject
ac_stroke %>%
  left_join(ac_ams, by = "subject_id") %>%
  left_join(ac_demo, by = "subject_id") %>%
  left_join(ac_hx, by = "subject_id") %>%
  left_join(ac_labs, by = "subject_id") %>%
  left_join(ac_meds, by = "subject_id") %>%
  rename(lab_ferritin = Ferritin,
         lab_esr = ESR,
         lab_crp = CRP,
         lab_d_dimer = D_dimer,
         lab_fibrinogen = Fibrinogen) %>%
  identity() -> ac_stroke_dat_combined

ac_stroke_dat_combined


ac_stroke_dat_combined %>%
  write_csv(file = "./data/ac_stroke_dat_combined.csv.gz")



