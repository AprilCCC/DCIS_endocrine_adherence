library(readxl)
library(dplyr)
library(tidyverse)
library(tableone)
library(gtsummary)
library(stringr)
library(readr)

###use the first pharmas dataset
phh1211 <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/MOH_phh1211.csv")

Cohort <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/Cohort.csv")

MOH_dim_form_pack_subsidy <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/MOH_dim_form_pack_subsidy.csv")

ET_name <- MOH_dim_form_pack_subsidy %>%
  filter(TG_NAME1=="Oncology Agents and Immunosuppressants",TG_NAME2=="Endocrine Therapy",
         CHEMICAL_NAME %in% c("Tamoxifen citrate","Letrozole ","Anastrozole","Exemestane","Megestrol acetate","Fulvestrant","Toremifene",
                              "Aminoglutethimide","Stilboestrol"))

phh1211_ET <- ET_name %>%
  left_join(phh1211,by=c("DIM_FORM_PACK_SUBSIDY_KEY"="DIM_FORM_PACK_SUBSIDY_KEY"))

BCFNZ_DCIS_lesion_uni <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/recurrence/BCFNZ_DCIS_lesion_uni.csv")

BCFNZ_DCIS_lesion_uni_endo <- BCFNZ_DCIS_lesion_uni %>%
  left_join(Cohort, by = c("PatientNo" = "PATIENTNO"))%>%
  left_join()
