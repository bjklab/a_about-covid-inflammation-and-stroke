#' load libraries and set seed
library(tidyverse)
library(readxl)
library(gt)
library(tidybayes)
library(brms)
library(gtsummary)

set.seed(16)


#' load data

read_csv("./data/ac_stroke_dat_combined.csv.gz") %>%
  identity() -> dat
dat



#' summary table by EXPOSURE
dat %>%
  mutate(stroke = case_when(stroke == TRUE ~ "COVID-19 with Stroke",
                            stroke == FALSE ~ "COVID-19 without Stroke"),
         ferritin_elevated = lab_ferritin >= 300,
         ferritin_elevated = case_when(ferritin_elevated == TRUE ~ "Ferritin Elevated (>= 300 mcg/L)",
                                       ferritin_elevated == FALSE ~ "Ferritin Normal (< 300 mcg/L)")) %>%
  select(-subject_id, -index_date) %>%
  rename(`Age (years)` = patient_age_years,
         `Sex` = patient_sex,
         `Diabetes mellitus, type 2` = dx_dm2,
         `Hypertension` = dx_htn,
         `Tobacco Use Disorder` = dx_tobacco,
         `Coronary Artery Disease (CAD)` = dx_cad,
         `Peripheral Artery Disease (PAD)` = dx_pad,
         `Anticoagulant (within 7 days)` = any_anticoagulant,
         `Antiplatelet (within 7 days)` = any_antiplatelet,
         `Stroke` = stroke,
         CRP = lab_crp,
         ESR = lab_esr,
         Ferritin = lab_ferritin,
         Fibrinogen = lab_fibrinogen,
         `D-dimer` = lab_d_dimer,
         `Altered Mental Status` = ams,
         `Fever` = covid_symp_fever,
         `Diarrhea` = covid_symp_diarrhea,
         `Respiratory Symptoms` = covid_symp_breathing,
         `Rash` = covid_symp_rash) %>%
  tbl_summary(
    data = .,
    by = ferritin_elevated,
    #type = list(`Maximum FiO2 at MV Onset (%)` ~ "continuous",
    #`Maximum PEEP at MV Onset (cm H2O)` ~ "continuous")
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = "Table 1: Characteristics of Enrolled Subjects",
                 #subtitle = "Exposure: Greater Than 7 Subglottic Suction Episodes in First Calendar Day",
  ) -> t1_ferritin

t1_ferritin


t1_ferritin %>%
  gt::as_raw_html() %>%
  write_lines(file = "./tabs/t1_ferritin.html")


t1_ferritin %>%
  gt::as_rtf() %>%
  write_lines(file = "./tabs/t1_ferritin.rtf")

t1_ferritin %>%
  write_rds(file = "./tabs/t1_ferritin.rds")





#' summary table by OUTCOME
dat %>%
  mutate(stroke = case_when(stroke == TRUE ~ "COVID-19 with Stroke",
                            stroke == FALSE ~ "COVID-19 without Stroke"),
         ferritin_elevated = lab_ferritin >= 300,
         ferritin_elevated = case_when(ferritin_elevated == TRUE ~ "Ferritin Elevated (>= 300 mcg/L)",
                                       ferritin_elevated == FALSE ~ "Ferritin Normal (< 300 mcg/L)")) %>%
  select(-subject_id, -index_date) %>%
  rename(`Age (years)` = patient_age_years,
         `Sex` = patient_sex,
         `Diabetes mellitus, type 2` = dx_dm2,
         `Hypertension` = dx_htn,
         `Tobacco Use Disorder` = dx_tobacco,
         `Coronary Artery Disease (CAD)` = dx_cad,
         `Peripheral Artery Disease (PAD)` = dx_pad,
         `Anticoagulant (within 7 days)` = any_anticoagulant,
         `Antiplatelet (within 7 days)` = any_antiplatelet,
         `Stroke` = stroke,
         CRP = lab_crp,
         ESR = lab_esr,
         Ferritin = lab_ferritin,
         Fibrinogen = lab_fibrinogen,
         `D-dimer` = lab_d_dimer,
         `Altered Mental Status` = ams,
         `Fever` = covid_symp_fever,
         `Diarrhea` = covid_symp_diarrhea,
         `Respiratory Symptoms` = covid_symp_breathing,
         `Rash` = covid_symp_rash,
         `Ferritin Elevated` = ferritin_elevated) %>%
  tbl_summary(
    data = .,
    by = Stroke,
    #type = list(`Maximum FiO2 at MV Onset (%)` ~ "continuous",
                #`Maximum PEEP at MV Onset (cm H2O)` ~ "continuous")
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = "Table 1: Characteristics of Enrolled Subjects",
                 #subtitle = "Exposure: Greater Than 7 Subglottic Suction Episodes in First Calendar Day",
  ) -> t1_stroke

t1_stroke
