library(readxl)
library(dplyr)
library(tidyverse)
library(tableone)
library(gtsummary)
library(stringr)
library(readr)

PHH1233_table <- read.table("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH linkage/MOH-DataServices_PHH1233_table.txt", 
                      header = TRUE, 
                      sep = "|", 
                      quote = "\"",  # handle quoted strings properly
                      fill = TRUE)    # fill missing columns with NAs

write.csv(PHH1233_table, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/PHH1233_table.csv",row.names=FALSE)


#### Demographic file
BCFNZ_work <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Workup.xlsx")

## contained unique patient information
BCFNZ_work_1 <- BCFNZ_work %>%
  select(PatientNo,`Date Of Tissue Diagnosis`, `DiagnosisType`,DomicileCodeAtDiagnosis,SourceOfReferral, MenopausalStatus,
         #PreviousBreastCancerSurgery, DiagnosisDateOfPreviousBreastCancer,PreviousBreastCancerSurgeryDate,
  ) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`)%>%
  filter(year(Date_Of_Tissue_Diagnosis)>1999)%>%
  arrange(Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo)%>%
  mutate(    
    ##Detection
    Detection = case_when(
      SourceOfReferral %in% c("BreastScreen Aotearoa") ~ "BSA screen",
      SourceOfReferral %in% c("Screen detected - non BSA") ~ "non-BSA screen",
      SourceOfReferral %in% c("GP (symptomatic)","Private hospital","Other department; same hospital","Emergency department","Other (specify)",
                              "Other", "Other hospital","Private specialist","Unknown")~ "Symptomatic or other",
      TRUE ~ NA_character_),
    ## menopausal status
    MenopausalStatus=case_when(
      MenopausalStatus %in% c("Peri-menopausal","Pre-menopausal") ~"Pre/peri",
      MenopausalStatus %in% c("Post-menopausal") ~"Post",
      MenopausalStatus %in% c("Unknown") | is.na(MenopausalStatus) ~"Unknown",
      TRUE ~ MenopausalStatus)) %>%
  ungroup()%>%
  select(-c(SourceOfReferral,DiagnosisType))


## keep all patients information
BCFNZ_demo <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Demographic Data.xlsx")

BCFNZ_demo_1 <- BCFNZ_demo %>%
  select(PatientNo,`Date Of Tissue Diagnosis`, Gender ,`Age At Diagnosis`, `Ethnicity (Level 1)`,`Date Of Death`, `Cause Of Death`) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`,Age_At_Diagnosis =`Age At Diagnosis`, Ethnicity=`Ethnicity (Level 1)`,
         Date_Of_Death =`Date Of Death`,Cause_Of_Death= `Cause Of Death`) %>%
  filter(year(Date_Of_Tissue_Diagnosis)>1999)%>%
  arrange(Date_Of_Tissue_Diagnosis) %>%
  group_by(PatientNo)%>%
  mutate(
    ##diagnosis year
    Year_Date_of_Tissue_Diagnosis=year(Date_Of_Tissue_Diagnosis),
    year_period = case_when(
      Year_Date_of_Tissue_Diagnosis %in% c ("2000","2001","2002","2003","2004") ~ "2000-2004",
      Year_Date_of_Tissue_Diagnosis %in% c ("2005","2006","2007","2008","2009") ~ "2005-2009",
      Year_Date_of_Tissue_Diagnosis %in% c ("2010","2011","2012","2013","2014") ~ "2010-2014",
      Year_Date_of_Tissue_Diagnosis %in% c ("2015","2016","2017","2018","2019") ~ "2015-2019",
      Year_Date_of_Tissue_Diagnosis %in% c ("2020","2021","2022") ~ "2020-2022",
      TRUE ~ as.character(Year_Date_of_Tissue_Diagnosis)),  
    ##age group
    age_group = case_when(
      Age_At_Diagnosis <45 ~ "<45",
      Age_At_Diagnosis >=45 & Age_At_Diagnosis <=69~ "45-69",
      Age_At_Diagnosis >69 ~ ">69",
      TRUE ~ as.character(Age_At_Diagnosis)),
    ##menopausal based on the age
    #    menopausal = case_when(
    #      Age_At_Diagnosis <45 ~"premenopausal",
    #      Age_At_Diagnosis >=45 & Age_At_Diagnosis <55 ~"perimenopausal",
    #      Age_At_Diagnosis >= 55 ~ "postmenopausal"),
    ##Ethnicity
    Ethnicity = case_when(
      Ethnicity %in% c("Middle Eastern/Latin American/African", "Residual Categories", "Other Ethnicity") ~ "Other or Unknown",
      Ethnicity == "Pacific Peoples" ~ "Pacific",
      TRUE ~ Ethnicity))%>%
  ungroup()


####################################################
#### histopathology
BCFNZ_his <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Histopathology.xlsx")

BCFNZ_his_1 <- BCFNZ_his %>%
  select(PatientNo,`Date Of Tissue Diagnosis`,Side,AxillaryNodesPresent, PathologicalTStageBasedOnPrimaryTumour,
         PathologicalNStage,DCISClosestCircumferentialMargin,DCISClosestCircumferentialMarginType,
         DCISClosestVerticalMargin,DCISClosestVerticalMarginType) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`,
         Side_his=Side)%>%
  filter(year(Date_Of_Tissue_Diagnosis)>1999)%>%
  arrange(Date_Of_Tissue_Diagnosis) %>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%
  filter(
    ###Filter the groups where n() > 1 and check if any of the values in DCISClosestCircumferentialMargin are not NA or if all are NA.
    (n() > 1 & (any(!is.na(DCISClosestCircumferentialMargin)) | all(is.na(DCISClosestCircumferentialMargin)))) |
      n() == 1) %>%
  slice(1)%>%
  ungroup()


###diagnosis type sheet 
BCFNZ_type <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Diagnosis Type.xlsx")%>%
  select(PatientNo,`Date of Tissue Diagnosis`,`New Diagnosis Type`)%>%
  rename(Date_Of_Tissue_Diagnosis = `Date of Tissue Diagnosis`, 
         New_Diagnosis_Type=`New Diagnosis Type`)


## lesion invasive==0 and node as no malignant, only choose all cases diagnosis after 1999
BCFNZ_lesion <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Lesions.xlsx")

BCFNZ_lesion_1 <- BCFNZ_lesion %>%
  select(PatientNo, `Date Of Tissue Diagnosis`, Lesion, BreastSide,BilateralSynchronousBreastCancer,
         CoreBiopsyBreastResult, InvasiveTumourSize, DCISTumourSize, HighestDCISGrade, 
         FNACytologyNodeResult,DCISNecrosis,CoreBiopsyOestrogenResult,CoreBiopsyProgesteroneResult,
         `HP Oestrogen Result`, `HP Progesterone result`) %>%
  rename(Date_Of_Tissue_Diagnosis = `Date Of Tissue Diagnosis`, 
         HP_Oestrogen_Result=`HP Oestrogen Result`, HP_Progesterone_result=`HP Progesterone result`)%>%
  left_join(BCFNZ_type,by = c("PatientNo", "Date_Of_Tissue_Diagnosis"))


##Identify all diagnosis type after 1999, ensuring that the occurrence is included...
BCFNZ_lesion_type <- BCFNZ_lesion_1 %>%
  filter(year(Date_Of_Tissue_Diagnosis)>1999)%>%
  left_join(BCFNZ_his_1 %>%
              select(PatientNo,Date_Of_Tissue_Diagnosis,Side_his,PathologicalTStageBasedOnPrimaryTumour,PathologicalNStage,
                     DCISClosestCircumferentialMargin,DCISClosestCircumferentialMarginType,
                     DCISClosestVerticalMargin,DCISClosestVerticalMarginType),by=c("PatientNo","Date_Of_Tissue_Diagnosis"))%>%
  filter(!if_all(c(BreastSide, InvasiveTumourSize, DCISTumourSize), ~ is.na(.))) %>%  ## exclude the case if all specified columns are NA, 7 patients
  arrange(Date_Of_Tissue_Diagnosis,Lesion) %>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis)%>%
  mutate(
    ###diagnosis type, DCIS or invasive cancer,A response of 999 for DCIS size indicated confirmed DCIS but size was unknown/not confirmed.
    Lesion_type = case_when(
      (is.na(InvasiveTumourSize) | !is.na(InvasiveTumourSize) & InvasiveTumourSize %in%c("0","999")) &
        (is.na(PathologicalTStageBasedOnPrimaryTumour) | 
           !is.na(PathologicalTStageBasedOnPrimaryTumour) & !PathologicalTStageBasedOnPrimaryTumour %in%c ("is (LCIS)","is (Paget's)"))  ~ "DCIS",  # Set to DCIS
      DCISTumourSize %in%c("0","999") & PathologicalTStageBasedOnPrimaryTumour %in%c ("is (LCIS)","is (Paget's)") ~"Other",
      TRUE ~ "Invasive" ),# Default to invasive
    ##laterality
    BreastSide=ifelse(!is.na(BilateralSynchronousBreastCancer) & BilateralSynchronousBreastCancer=="Yes", "Bilateral", first(BreastSide))) %>%
  slice(1)%>%  ##each dignosis choose the first line
  mutate(
    ##tumour size
    DCIS_tumour_size = case_when(
      is.na(DCISTumourSize) | DCISTumourSize %in% c("999") ~ "Unknown",
      DCISTumourSize <= 20 ~ "<=20",
      DCISTumourSize > 20 ~ ">20",
      TRUE ~ "Unknown"),
    ##grade
    HighestDCISGrade = case_when(
      is.na(HighestDCISGrade) | HighestDCISGrade %in% c("Not stated") ~ "unknown",
      TRUE ~ HighestDCISGrade),
    ##necrosis
    DCIS_Necrosis = case_when(
      is.na(DCISNecrosis) | DCISNecrosis %in% c("Unknown") ~ "Unknown",
      TRUE ~ DCISNecrosis),
    ## receptor
    Receptor = case_when(
      # All specified columns are "Negative"
      (CoreBiopsyOestrogenResult == "Negative" & 
         CoreBiopsyProgesteroneResult == "Negative") | 
        (HP_Oestrogen_Result == "Negative" & 
           HP_Progesterone_result == "Negative") ~ "Negative",
      # Any column contains "Positive"
      (CoreBiopsyOestrogenResult == "Positive" | 
         CoreBiopsyProgesteroneResult == "Positive" | 
         HP_Oestrogen_Result == "Positive" | 
         HP_Progesterone_result == "Positive") ~ "Positive",
      # All specified columns are NA
      all(is.na(c(CoreBiopsyOestrogenResult, HP_Oestrogen_Result, CoreBiopsyProgesteroneResult, HP_Progesterone_result))) ~ "Unknown",
      TRUE ~ "Unknown")) %>%
  ungroup() %>%
  select(-c(CoreBiopsyBreastResult,CoreBiopsyOestrogenResult, HP_Oestrogen_Result, CoreBiopsyProgesteroneResult, HP_Progesterone_result,FNACytologyNodeResult))

write.csv(BCFNZ_lesion_type, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_lesion_type.csv",row.names=FALSE)



###cross check with NZCR data 
### choose insitu and invasive breast cancer cases from NZCR
#CAS2258 <- read.csv("D:/UOA/DCIS/NZ data/Data set/MOH-20240209/MOH_cas2258.csv")
CAS2275 <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/CAS2275.csv")%>%
  select(NEW_MASTER_ENCRYPTED_HCU_ID,DIAGNOSIS_DATE,SITE_CODE_10,LATERALITY_CODE)%>%
  mutate(DIAGNOSIS_DATE=as.Date(DIAGNOSIS_DATE,"%Y-%m-%d"),
         LATERALITY_CODE = case_when(
           LATERALITY_CODE == "L" ~ "Left",
           LATERALITY_CODE == "R" ~ "Right",
           TRUE ~ LATERALITY_CODE))

Cohort <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/Cohort.csv")

BCFNZ_lesion_type_2275 <- merge(
  merge(
    BCFNZ_lesion_type, Cohort, by.x = "PatientNo", by.y = "PATIENTNO", all.x = TRUE), 
  CAS2275, by.x = "NEW_MASTER_ENCRYPTED_HCU_ID", by.y = "NEW_MASTER_ENCRYPTED_HCU_ID", all.x = TRUE) %>%
  mutate(Date_Of_Tissue_Diagnosis = as.Date(Date_Of_Tissue_Diagnosis)) %>%
  arrange(Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis) %>%
  filter(
    is.na(DIAGNOSIS_DATE) | 
      ( !is.na(DIAGNOSIS_DATE) & length(DIAGNOSIS_DATE[!is.na(DIAGNOSIS_DATE)]) > 0)) %>%
  ###choose the closest date when merge BCFNZ with NZCR diagnosis
  mutate(
    closest_date = ifelse( !is.na(DIAGNOSIS_DATE),
                           DIAGNOSIS_DATE[which.min(abs(DIAGNOSIS_DATE[!is.na(DIAGNOSIS_DATE)] - Date_Of_Tissue_Diagnosis))],NA)) %>%
  filter( is.na(DIAGNOSIS_DATE) | (DIAGNOSIS_DATE == closest_date)) %>%
  ungroup() %>%
  select(-closest_date)  

write.csv(BCFNZ_lesion_type_2275, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_lesion_type_2275.csv",row.names=FALSE)

###chose the first DCIS of each patients based on the Lesion_type, and diagnosis 2000-2022, #6154 patients
BCFNZ_lesion_type_DCIS <- BCFNZ_lesion_type %>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%
  filter(Lesion_type == "DCIS" & year(Date_Of_Tissue_Diagnosis)>1999) %>%
  arrange(Date_Of_Tissue_Diagnosis) %>%  # Sort before slicing
  group_by(PatientNo) %>%
  slice(1) %>%
  ungroup()

write.csv(BCFNZ_lesion_type_DCIS, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_lesion_type_DCIS.csv",row.names=FALSE)


###check whether some patients had invasive BC before DCIS, 184 patients had invasive BC before DCIS need to be excluded
BCFNZ_DCIS_invasive <- BCFNZ_lesion_type_DCIS %>%
  left_join(
    BCFNZ_lesion_type %>% 
      select(PatientNo, Date_Of_Tissue_Diagnosis, Lesion_type),
    by = "PatientNo",
    relationship = "many-to-many") %>%
  group_by(PatientNo) %>%
  filter(any(Lesion_type.y == "Invasive") & (Date_Of_Tissue_Diagnosis.y < Date_Of_Tissue_Diagnosis.x)) %>%
  ungroup()

write.csv(BCFNZ_DCIS_invasive, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_invasive.csv",row.names=FALSE)


###DCIS without invasive BC history, include subsequent event
BCFNZ_primary_DCIS <- BCFNZ_lesion_type_DCIS %>%
  left_join(
    BCFNZ_lesion_type %>% 
      select(PatientNo, Date_Of_Tissue_Diagnosis, Lesion_type,BreastSide,Side_his)%>%
      filter(Lesion_type !="Other"),  ### choose only DCIS and invasive in the subsequent event
    by = "PatientNo",relationship = "many-to-many") %>%
  group_by(PatientNo) %>%
  filter(!(any(Lesion_type.y == "Invasive") & 
             any(Date_Of_Tissue_Diagnosis.y < Date_Of_Tissue_Diagnosis.x))) %>%
  mutate(BreastSide.x = ifelse(is.na(BreastSide.x), LATERALITY_CODE, BreastSide.x)) %>%
  rename(Date_Of_Tissue_Diagnosis = Date_Of_Tissue_Diagnosis.x,
         BreastSide=BreastSide.x)%>%
  ungroup()

write.csv(BCFNZ_primary_DCIS, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_primary_DCIS.csv",row.names=FALSE)


####new diagnosis from the new_diagnosis type file  
BCFNZ_type_second <- BCFNZ_type %>%
  group_by(PatientNo) %>%
  filter(n() >= 2) %>%  # Keep only patients with at least 2 records
  arrange(PatientNo, Date_Of_Tissue_Diagnosis) %>%  # Ensure chronological order
  filter(New_Diagnosis_Type[row_number() == 1] == "DCIS") %>%  # Keep only patients where the first diagnosis type is DCIS
  slice(1:2) %>%  # Keep only the first two records for each patient
  ungroup() %>% 
  group_by(PatientNo) %>%
  mutate(second = New_Diagnosis_Type[row_number() == 2],  # Extract second diagnosis type
         second_date = Date_Of_Tissue_Diagnosis[row_number() == 2]) %>%  # Extract second diagnosis date
  slice(1)%>%
  ungroup()
###396 patients had new diagnosis of DCIS  in the new diagnosis file

###second diagnosis from lesion file, 404 records, some patients with two recurrences
BCFNZ_DCIS_second <- BCFNZ_primary_DCIS %>%
  #select(PatientNo, `Date Of Tissue Diagnosis`, `Lesion`, `BreastSide`, InvasiveTumourSize, DCISTumourSize, HighestDCISGrade,CoreBiopsyBreastResult) %>%
  arrange(Date_Of_Tissue_Diagnosis.y) %>%
  group_by(PatientNo) %>%
  #filter(n_distinct(Date_Of_Tissue_Diagnosis) > 1,year(Date_Of_Tissue_Diagnosis)>=2000) %>%
  ##lead(BreastSide): This function retrieves the value of BreastSide from the next row within the current group
  mutate(
    second_primary = case_when(
      (BreastSide.y) == lead(BreastSide.y) & (lead(Lesion_type.y) == "DCIS") ~ "DCIS-IBTR",
      (BreastSide.y) == lead(BreastSide.y) & (lead(Lesion_type.y) == "Invasive") ~ "iIBTR",
      ###when second breast side as bilateral, recorded it as IBTR
      (BreastSide.y) != lead(BreastSide.y) & lead(BreastSide.y) == "Bilateral" & (lead(Lesion_type.y) == "DCIS")    ~ "DCIS-IBTR", 
      (BreastSide.y) != lead(BreastSide.y) & lead(BreastSide.y) == "Bilateral" & (lead(Lesion_type.y) == "Invasive") ~ "iIBTR",
      (BreastSide.y) != lead(BreastSide.y) & (lead(Lesion_type.y) == "DCIS") ~ "DCIS-CBC",
      (BreastSide.y) != lead(BreastSide.y) & (lead(Lesion_type.y) == "Invasive") ~ "iCBC",),
    second_primary_date = case_when(!is.na(second_primary) ~ lead(Date_Of_Tissue_Diagnosis.y), ),
    second_primary_side = case_when( !is.na(second_primary) ~ lead(BreastSide.y), )) %>%
  # Filter based on the count of distinct dates
  filter((n_distinct(Date_Of_Tissue_Diagnosis.y) == 2 & row_number() == 1) | 
           (n_distinct(Date_Of_Tissue_Diagnosis.y) == 3 & row_number() <= 2)) %>%
  #filter(n_distinct(Date_Of_Tissue_Diagnosis.y) == 3 ) %>%
  ungroup()%>%
  select(c(PatientNo,Date_Of_Tissue_Diagnosis,BreastSide ,second_primary,second_primary_date,second_primary_side))

write.csv(BCFNZ_DCIS_second, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_second.csv",row.names=FALSE)


##### primary DCIS, 5942  DCIS female patients diagnosed 2000-2022
BCFNZ_DCIS_lesion <- BCFNZ_primary_DCIS %>%
  #left_join(BCFNZ_new_diagnosis_1,by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  #left_join(BCFNZ_his_1,by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  left_join(BCFNZ_demo_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  left_join(BCFNZ_work_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  left_join(BCFNZ_relatives_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  filter(Gender == "Female") %>%
  #filter(New_Diagnosis_Type == "DCIS" | ((PathologicalTStageBasedOnPrimaryTumour == "is (DCIS)" ))) %>%
  #filter(is.na(DiagnosisDateOfPreviousBreastCancer)) %>%
  arrange(Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis)%>%
  ungroup()%>%
  group_by(PatientNo)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(family_history = ifelse(is.na(family_history), "No", family_history))%>%
  select(-c(Date_Of_Tissue_Diagnosis.y,Lesion_type.y,BreastSide.y))

duplicated_patients <- BCFNZ_DCIS_lesion$PatientNo[duplicated(BCFNZ_DCIS_lesion$PatientNo)]

write.csv(BCFNZ_DCIS_lesion, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion.csv",row.names=FALSE)





###use the first pharmas dataset
phh1233 <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/PHH1233.csv")

phh1233_table <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/PHH1233_table.csv")

Cohort <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/Cohort.csv")

#ET_name <- phh1233_table %>% filter(TG_LEVEL1_NAME=="Oncology Agents and Immunosuppressants",TG_LEVEL2_NAME=="Endocrine Therapy")

phh1233_ET <- phh1233_table %>%
  right_join(phh1233, by = c("DIM_FORM_PACK_SUBSIDY_KEY")) %>%
  left_join(Cohort, by = "NEW_MASTER_ENCRYPTED_HCU_ID") %>%
  select(PATIENTNO, everything())  # Move PATIENTNO to the first column


write.csv(phh1233_ET, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/phh1233_ET.csv",row.names=FALSE)



BCFNZ_DCIS_lesion <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion.csv")

BCFNZ_DCIS_lesion_ET <- BCFNZ_DCIS_lesion %>%
  left_join(Cohort, by = c("PatientNo" = "PATIENTNO"))%>%
  left_join(phh1233_ET,by="NEW_MASTER_ENCRYPTED_HCU_ID")


write.csv(BCFNZ_DCIS_lesion_ET, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion_ET.csv",row.names=FALSE)




