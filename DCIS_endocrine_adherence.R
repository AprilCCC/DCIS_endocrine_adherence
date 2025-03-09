library(readxl)
library(dplyr)
library(tidyverse)
library(tableone)
library(gtsummary)
library(stringr)
library(readr)

# convert txt to csv 
#PHH1233_table <- read.table("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH linkage/MOH-DataServices_PHH1233_table.txt", 
#                      header = TRUE,   sep = "|", 
#                      quote = "\"",  # handle quoted strings properly
#                      fill = TRUE)    # fill missing columns with NAs

#write.csv(PHH1233_table, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/PHH1233_table.csv",row.names=FALSE)

# use the first time pharmaceutical collection data phh1211
phh1211 <- read.table("D:/UOA/DCIS/NZ data/Data set/MOH data/phh1211.txt", 
                    header = TRUE,   sep = "|", 
                    quote = "\"",  # handle quoted strings properly
                     fill = TRUE) 

cas2264_nhis <- read.table("F:/Archive/BCFNZ data/MoH linkage/MOH-DataServices_cas2264_nhis.txt", 
                      header = TRUE,   sep = "|", 
                      quote = "\"",  # handle quoted strings properly
                      fill = TRUE)%>%
  group_by(new_master_enc)%>%
  slice(1)%>% ungroup()

MOH_dim_form_pack_subsidy <- read.csv("D:/UOA/DCIS/NZ data/Data set/MOH-20240209/MOH_dim_form_pack_subsidy.csv")

phh1211_ET <- phh1211 %>%
  left_join(MOH_dim_form_pack_subsidy, by="DIM_FORM_PACK_SUBSIDY_KEY")%>%
  left_join(cas2264_nhis, by="new_master_enc")%>%
  filter(TG_NAME2=="Endocrine Therapy")%>%
  select(new_master_enc, AGE_AT_DISPENSING,CHEMICAL_NAME,DATE_DISPENSED, FORMULATION_NAME,
         REPEAT_SEQUENCE_NUMBER,QUANTITY_DISPENSED,QUANTITY_PRESCRIBED,
         DISPENSINGS_PRESCRIBED,DOSE,FREQUENCY,DAILY_DOSE,DAYS_SUPPLY)

write.csv(phh1211_ET, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/phh1211_ET.csv",row.names=FALSE)

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
      Year_Date_of_Tissue_Diagnosis %in% c ("2010","2011","2012","2013","2014") ~ "2010-2014",
      Year_Date_of_Tissue_Diagnosis %in% c ("2000","2001","2002","2003","2004") ~ "2000-2004",
      Year_Date_of_Tissue_Diagnosis %in% c ("2005","2006","2007","2008","2009") ~ "2005-2009",
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
    ## HR receptor
    HRReceptor = case_when(
      #Any column contains "Positive"
      (CoreBiopsyOestrogenResult == "Positive" | 
         CoreBiopsyProgesteroneResult == "Positive" | 
         HP_Oestrogen_Result == "Positive" | 
         HP_Progesterone_result == "Positive") ~ "Positive", 
      # All specified columns are "Negative"
      (CoreBiopsyOestrogenResult == "Negative" & 
         CoreBiopsyProgesteroneResult == "Negative") | 
        (HP_Oestrogen_Result == "Negative" & 
           HP_Progesterone_result == "Negative") ~ "Negative",
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
  left_join(BCFNZ_ET_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  filter(Gender == "Female") %>%
  #filter(New_Diagnosis_Type == "DCIS" | ((PathologicalTStageBasedOnPrimaryTumour == "is (DCIS)" ))) %>%
  #filter(is.na(DiagnosisDateOfPreviousBreastCancer)) %>%
  arrange(Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis)%>%
  ungroup()%>%
  group_by(PatientNo)%>%
  slice(1)%>%
  ungroup()%>%
  select(-c(Date_Of_Tissue_Diagnosis.y,Lesion_type.y,BreastSide.y))

duplicated_patients <- BCFNZ_DCIS_lesion$PatientNo[duplicated(BCFNZ_DCIS_lesion$PatientNo)]

write.csv(BCFNZ_DCIS_lesion, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion.csv",row.names=FALSE)


#### choose the last definitive surgery
BCFNZ_surgery <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Primary Surgery.xlsx")

BCFNZ_surgery_1 <- BCFNZ_surgery %>%
  select(PatientNo, `Date Of Tissue Diagnosis`, DateOfSurgery, Side, SiteHealthFacilityOfSurgery,
         LeftBreastTypeOfAxillarySurgery, RightBreastTypeOfAxillarySurgery,
         LeftBreastTypeOfBreastSurgery, RightBreastTypeOfBreastSurgery) %>%
  rename(surgery_side = Side, Date_Of_Tissue_Diagnosis = `Date Of Tissue Diagnosis`) %>%
  filter(year(Date_Of_Tissue_Diagnosis)>1999)%>%
  arrange(DateOfSurgery) %>%
  mutate(
    DateOfSurgery = case_when(PatientNo == "5676" ~ as.POSIXct("2004-10-17 00:00:00", tz = "UTC"),TRUE ~ DateOfSurgery)) %>% ##patient 5676 surgery date as 2004-10-17
  group_by(PatientNo)%>%
  slice(if_else(PatientNo %in% c("3028","4462","2443","18","2029"), 1, row_number()))%>% ##only keep the first row for some patients 
  filter(year(DateOfSurgery)-year(Date_Of_Tissue_Diagnosis)<5) %>%  ##choose surgery within five years after diagnosis
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%
  mutate(
    ##number of excision, based on the surgery date
    num_surgeries = n_distinct(DateOfSurgery[!is.na(LeftBreastTypeOfBreastSurgery) | !is.na(RightBreastTypeOfBreastSurgery)]),
    number_of_excision =  case_when(
      num_surgeries == 1 ~"1",
      num_surgeries == 2 ~"2-4",
      num_surgeries >=3 ~"2-4"),
    ##surgery facility site
    SiteHealthFacilityOfSurgery = case_when(
      is.na(SiteHealthFacilityOfSurgery) ~ "Unknown",
      TRUE ~ SiteHealthFacilityOfSurgery),
    ##axillary
    axillary = case_when(
      str_detect(LeftBreastTypeOfAxillarySurgery, "Level 1 \\(axillary node sample\\)|Level 2 \\(axillary node dissection\\)|Level 3 \\(axillary node clearance\\)|Targeted Axillary Dissection \\(TAD\\)") |
        str_detect(RightBreastTypeOfAxillarySurgery, "Level 1 \\(axillary node sample\\)|Level 2 \\(axillary node dissection\\)|Level 3 \\(axillary node clearance\\)|Targeted Axillary Dissection \\(TAD\\)") ~ "ALND",
      str_detect(LeftBreastTypeOfAxillarySurgery, "Sentinel node biopsy") |
        str_detect(RightBreastTypeOfAxillarySurgery, "Sentinel node biopsy|Sampling") ~ "SLNB",
      str_detect(LeftBreastTypeOfAxillarySurgery, "No axillary surgery required|Declined|Different through patient choice") |
        str_detect(RightBreastTypeOfAxillarySurgery, "No axillary surgery required|Declined") ~ "No axillary intervention",
      is.na(LeftBreastTypeOfAxillarySurgery) & is.na(RightBreastTypeOfAxillarySurgery) |
        LeftBreastTypeOfAxillarySurgery == "Unknown" | RightBreastTypeOfAxillarySurgery == "Unknown" ~ "No axillary intervention"),
    axillary_date = if_else(axillary != "No axillary intervention", DateOfSurgery, NA_Date_),
    ##surgery
    surgery = case_when(
      str_detect(LeftBreastTypeOfBreastSurgery, "WLE / partial mastectomy|Hookwire localisation excision|Re-excision|Lumpectomy / excision biopsy") |
        str_detect(RightBreastTypeOfBreastSurgery, "WLE / partial mastectomy|Hookwire localisation excision|Re-excision|Lumpectomy / excision biopsy") ~ "BCS",
      str_detect(LeftBreastTypeOfBreastSurgery, "Prophylactic mastectomy|Mastectomy") |
        str_detect(RightBreastTypeOfBreastSurgery, "Prophylactic mastectomy|Mastectomy") ~ "Mastectomy",
      str_detect(LeftBreastTypeOfBreastSurgery, "Axillary surgery only|No breast surgery") |
        str_detect(RightBreastTypeOfBreastSurgery, "Axillary surgery only|No breast surgery") ~ "No surgery",
      LeftBreastTypeOfBreastSurgery == "Unknown" | RightBreastTypeOfBreastSurgery == "Unknown" ~ "Mastectomy",
      is.na(LeftBreastTypeOfBreastSurgery) | is.na(RightBreastTypeOfBreastSurgery) ~ "No surgery"),
    surgery_date = if_else(surgery != "No surgery", DateOfSurgery, NA_Date_),
    ##chose the last surgery
    Axillary = case_when(
      all(axillary == "No axillary intervention") ~ last(axillary),
      TRUE ~ last(axillary[axillary != "No axillary intervention"])),
    Axillary_date = case_when(
      all(axillary == "No axillary intervention") ~ last(axillary_date),
      TRUE ~ last(axillary_date[axillary != "No axillary intervention"])),
    Surgery = case_when(
      all(surgery == "No surgery") ~ last(surgery),
      TRUE ~ last(surgery[surgery != "No surgery"])),
    Surgery_definitive_date = case_when(
      all(surgery == "No surgery") ~ last(surgery_date),
      TRUE ~ last(surgery_date[surgery != "No surgery"])),
    Surgery_first_date = case_when(
      all(surgery == "No surgery") ~ first(surgery_date),
      TRUE ~ first(surgery_date[surgery != "No surgery"]))) %>%
  slice(1) %>%
  select(-c(DateOfSurgery,axillary,axillary_date,surgery,surgery_date,num_surgeries))%>%
  ungroup()


write.csv(BCFNZ_surgery_1, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_surgery_1.csv",row.names=FALSE)

BCFNZ_RT <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Radiotherapy.xlsx")

BCFNZ_RT_1 <- BCFNZ_RT %>%
  select(PatientNo, `Date Of Tissue Diagnosis`, `Side Of Radiation Therapy (Neo-adjuvant / Primary / Adjuvant / LRR)`, 
         `Timing Of Radiation Therapy (Neo-adjuvant / Primary / Adjuvant / LRR)`,
         `RadiationTherapyStartDate (Neo-adjuvant / Primary / Adjuvant / LRR)`) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`,
         RT_adjuvant=`Timing Of Radiation Therapy (Neo-adjuvant / Primary / Adjuvant / LRR)`,
         RT_side=`Side Of Radiation Therapy (Neo-adjuvant / Primary / Adjuvant / LRR)`,
         RT_start_date=`RadiationTherapyStartDate (Neo-adjuvant / Primary / Adjuvant / LRR)`)%>%
  filter(RT_adjuvant == 'Adjuvant') %>%
  arrange(RT_start_date) %>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis)%>%
  slice(1)%>%
  ungroup()


###5778 patients had surgery
BCFNZ_DCIS_lesion_surgery <- BCFNZ_DCIS_lesion %>%
  left_join(BCFNZ_surgery_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis"), relationship = "many-to-many") %>%
  left_join(BCFNZ_RT_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis"), relationship = "many-to-many") %>%
  group_by(PatientNo) %>%
  arrange(Surgery_definitive_date)%>%
  slice_max(order_by = Surgery_definitive_date, n = 1) %>% 
  filter(!is.na(Surgery_first_date))%>%
  mutate(
    #DateOfReconstruction = if(all(is.na(DateOfReconstruction))) NA_Date_ else DateOfReconstruction[which.min(abs(difftime(DateOfReconstruction, Surgery_definitive_date, units = "days")))],
    RT_start_date = ifelse(
      all(is.na(RT_start_date)) | length(RT_start_date) == 0, 
      NA_Date_,  # Use NA_Date_ if no valid date is found
      RT_start_date[which.min(abs(difftime(RT_start_date, Surgery_definitive_date, units = "days")))]
    ),
    #ET_start_date = if(all(is.na(ET_start_date))) NA_Date_ else ET_start_date[which.min(abs(difftime(ET_start_date, Surgery_definitive_date, units = "days")))]
  ) %>%
  slice_min(order_by = Date_Of_Tissue_Diagnosis, n = 1) %>%
  arrange(Surgery_definitive_date) %>%
  group_by(PatientNo) %>%
  slice(if (n() > 1) first(which(Surgery != "No surgery")) else 1) %>%
  mutate(
    ##menopausal
    MenopausalStatus =case_when(
      MenopausalStatus=="Unknown" & Age_At_Diagnosis < 45 ~ "Pre-menopausal",
      MenopausalStatus=="Unknown" & Age_At_Diagnosis >= 55 ~ "Post-menopausal",
      MenopausalStatus=="Unknown" & Age_At_Diagnosis >= 45 & Age_At_Diagnosis < 55~ "Peri-menopausal",
       TRUE ~ MenopausalStatus),
    ##margin
    DCIS_CircumferentialMargin = case_when(
      is.na(DCISClosestCircumferentialMargin) & Surgery %in% c("No surgery") ~ "No surgery",
      !is.na(DCISClosestCircumferentialMargin) & DCISClosestCircumferentialMargin < 2 ~ "<2",
      DCISClosestCircumferentialMargin >= 99 | DCISClosestCircumferentialMarginType == "Clear of margins; no measurement given" ~ ">=2",
      is.na(DCISClosestCircumferentialMargin) & is.na(DCISClosestCircumferentialMargin) ~ "Unknown",
      TRUE ~ ">=2"),  
    DCIS_verticalMargin = case_when(
      is.na(DCISClosestVerticalMargin) & Surgery %in% c("No surgery") ~ "No surgery",
      !is.na(DCISClosestVerticalMargin) & DCISClosestVerticalMargin < 2 ~ "<2",
      DCISClosestVerticalMargin >= 99 | DCISClosestVerticalMarginType == "Clear of margins; no measurement given" ~ ">=2",
      is.na(DCISClosestVerticalMargin) & is.na(DCISClosestVerticalMargin) ~ "Unknown",
      TRUE ~ ">=2"),
    DCIS_margin = case_when(
      DCIS_CircumferentialMargin == "<2" | DCIS_verticalMargin == "<2" ~ "<2",
      DCIS_CircumferentialMargin =="Unknown" & DCIS_verticalMargin =="Unknown" ~ "Unknown",
      TRUE ~  ">=2"),
    ##RT 
    RT_adjuvant = case_when(
      !is.na(RT_adjuvant) ~"Yes",
      TRUE ~"No"),
    ##surgery with RT
    Local = case_when(
      Surgery %in% c("No surgery") & RT_adjuvant=="No" ~ Surgery,
      Surgery %in% c("Mastectomy") ~Surgery,
      RT_adjuvant=="No" & !(Surgery %in% c("No surgery", "Mastectomy")) ~ paste0(last(Surgery[!is.na(Surgery)]), " alone"),
      RT_adjuvant=="Yes" & !(Surgery %in% c("No surgery", "Mastectomy")) ~ paste0(last(Surgery[!is.na(Surgery)]), " with RT"),
      Surgery %in% c("No surgery") & RT_adjuvant=="Yes" ~ "BCS with RT"),  ##patient 2058 no surgery and reconstruction record, but with RT, recorded as BCS with RT
  )%>%
  ungroup() %>%
  select(-c(LeftBreastTypeOfAxillarySurgery, RightBreastTypeOfAxillarySurgery, LeftBreastTypeOfBreastSurgery, RightBreastTypeOfBreastSurgery))

duplicated_patients <- BCFNZ_DCIS_lesion_surgery$PatientNo[duplicated(BCFNZ_DCIS_lesion_surgery$PatientNo)]

NZ_dep <- read_excel("D:/UOA/DCIS/NZ data/Data set/Deprivation_Dom_1991_2018_maps.xlsx")

BCFNZ_DCIS_lesion_surgery <- BCFNZ_DCIS_lesion_surgery %>%
  mutate(
    # Format DomicileCodeAtDiagnosis to 4 digits
    DomicileCodeAtDiagnosis = sprintf("%04d", as.integer(DomicileCodeAtDiagnosis)),
    dep_index = case_when(
      Year_Date_of_Tissue_Diagnosis %in% c(1999, 2000, 2001, 2002, 2003) ~ NZ_dep$Dep01[match(DomicileCodeAtDiagnosis, NZ_dep$dom)],
      Year_Date_of_Tissue_Diagnosis %in% c(2004, 2005, 2006, 2007, 2008, 2009) ~ NZ_dep$Dep06[match(DomicileCodeAtDiagnosis, NZ_dep$dom)],
      Year_Date_of_Tissue_Diagnosis %in% c(2010, 2011, 2012, 2013, 2014, 2015) ~ NZ_dep$Dep13[match(DomicileCodeAtDiagnosis, NZ_dep$dom)],
      Year_Date_of_Tissue_Diagnosis %in% c(2016, 2017, 2018, 2019, 2020, 2021, 2022) ~ NZ_dep$Dep18[match(DomicileCodeAtDiagnosis, NZ_dep$dom)],
      #DOMICILE_CODE %in% c("1510", "2504","3113","1314") ~ NZ_dep$Dep96[match(DOMICILE_CODE, NZ_dep$dom)],
      TRUE ~ NA)) %>%
  mutate(dep_index_c = case_when(
    dep_index %in% c(1, 2, 3, 4) ~ "NZDep1-4",
    dep_index %in% c(5, 6, 7) ~ "NZDep5-7",
    dep_index %in% c(8, 9, 10) ~ "NZDep8-10",
    TRUE ~ "Unknown"))


## urban/rural area, mapping with domicile code, rurality==IUR2021_name
rural <- read_excel("D:/UOA/DCIS/NZ data/Data set//geographic-areas-table-2021.xlsx") 

rural_unique <- unique(rural[c("dom", "IUR2021_name")])
rural_unique$dom <- sprintf("%04d", rural_unique$dom)

BCFNZ_DCIS_lesion_surgery <- BCFNZ_DCIS_lesion_surgery %>%
  mutate(
    IUR2021_name = rural_unique$IUR2021_name[match(DomicileCodeAtDiagnosis, rural_unique$dom)],
    Rurality = as.character(case_when(
      IUR2021_name %in% c("Small urban area", "Medium urban area", "Major urban area", "Large urban area") ~ "Urban",
      IUR2021_name %in% c("Rural other", "Rural settlement") ~ "Rural",
      #IUR2021_name %in% c("Inlet", "Inland water", "Oceanic") ~ "water",
      TRUE ~ "Other or Unknown")))%>%
  select(-c(dep_index,IUR2021_name))

write.csv(BCFNZ_DCIS_lesion_surgery, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion_surgery.csv",row.names=FALSE)


## LRR recurrence, 274 patients had lrr
BCFNZ_lrr <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/LocoRegionalRecurrence.xlsx")

BCFNZ_lrr_1 <-BCFNZ_lrr %>%
  select(PatientNo,`Date Of Tissue Diagnosis`, Side, DateOfRecurrence, SiteOfRecurrence) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`, Recurrence_side=Side) %>%
  filter(!is.na(Recurrence_side))


## surgery recurrence, 256 patients had surgery lrr

BCFNZ_surgery_lrr <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/SurgeryLocoRegionalRecurrence.xlsx")

BCFNZ_surgery_lrr_1 <- BCFNZ_surgery_lrr %>%
  select(PatientNo, `Date Of Tissue Diagnosis`, DateOfSurgery, TypeOfSurgeryForRecurrence, InvasiveCarcinoma,
         `Invasive carcinoma grade`, InvasiveCancerSize, AxillaryNodesPresent, Dcis) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`, Invasive_carcinoma_grade =`Invasive carcinoma grade`) %>%
  filter(!is.na(TypeOfSurgeryForRecurrence))


BCFNZ_surgery_lrr_event <- BCFNZ_lrr_1 %>%
  left_join(BCFNZ_surgery_lrr_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis"),relationship = "many-to-many")%>%
  group_by(PatientNo) %>%
  arrange(DateOfSurgery)%>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis) %>%
  mutate(
    LRR_surgery = case_when(
      (SiteOfRecurrence %in% c("Conserved breast (related to previous excision)","Reconstructed breast","Chest wall - chest wall / elsewhere","Reconstructed breast - local excision",
                               "Chest wall - mastectomy scar") | TypeOfSurgeryForRecurrence %in% c("Conserved breast (related to previous excision)","Reconstructed breast","Chest wall - chest wall / elsewhere",
                                                                                                   "Reconstructed breast - local excision","Chest wall - mastectomy scar"))
      & (InvasiveCarcinoma == "Yes" | !is.na(Invasive_carcinoma_grade) | !is.na(InvasiveCancerSize)) ~ "iIBTR",
      (SiteOfRecurrence %in% c("Conserved breast (related to previous excision)","Reconstructed breast","Chest wall - chest wall / elsewhere","Reconstructed breast - local excision",
                               "Chest wall - mastectomy scar") | TypeOfSurgeryForRecurrence %in% c("Conserved breast (related to previous excision)","Reconstructed breast","Chest wall - chest wall / elsewhere",
                                                                                                   " Reconstructed breast - local excision","Chest wall - mastectomy scar")) & (InvasiveCarcinoma == "No"  | is.na(InvasiveCarcinoma)) ~ "DCIS-IBTR",
      TRUE ~"LRR")) %>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%
  slice(which.min(match(LRR_surgery, c("iIBTR", "DCIS-IBTR", "LRR")))) %>%
  mutate(
    LRR_surgery_date = if_else(LRR_surgery %in% c("iIBTR", "DCIS-IBTR","LRR"),
                               coalesce(DateOfSurgery, DateOfRecurrence), NA_Date_)) %>%
  ungroup()

write.csv(BCFNZ_surgery_lrr_event, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_surgery_lrr_event.csv",row.names=FALSE)


##### Any event from lesion file #########
BCFNZ_lesion_event <- BCFNZ_primary_DCIS %>%
  #rename(Date_Of_Tissue_Diagnosis = `Date Of Tissue Diagnosis`) %>%
  #filter(Lesion == "Lesion 1") %>%
  arrange(Date_Of_Tissue_Diagnosis.y) %>%
  group_by(PatientNo) %>%
  filter(n_distinct(Date_Of_Tissue_Diagnosis.y) > 1,year(Date_Of_Tissue_Diagnosis.y)>=2000) %>%
  ##lead(BreastSide): This function retrieves the value of BreastSide from the next row within the current group
  mutate(
    event = case_when(
      (BreastSide.y) == lead(BreastSide.y) & (lead(Lesion_type.y) == "DCIS") ~ "DCIS-IBTR",
      (BreastSide.y) == lead(BreastSide.y) & (lead(Lesion_type.y) == "Invasive") ~ "iIBTR",
      ###when second breast side as bilateral, prioritized it as IBTR
      (BreastSide.y) != lead(BreastSide.y) & lead(BreastSide.y) == "Bilateral" & (lead(Lesion_type.y) == "DCIS")    ~ "DCIS-IBTR", 
      (BreastSide.y) != lead(BreastSide.y) & lead(BreastSide.y) == "Bilateral" & (lead(Lesion_type.y) == "Invasive") ~ "iIBTR",
      (BreastSide.y) != lead(BreastSide.y) & (lead(Lesion_type.y) == "DCIS")    ~ "DCIS-CBC", 
      (BreastSide.y) != lead(BreastSide.y) & (lead(Lesion_type.y) == "Invasive") ~ "iCBC",
      TRUE ~"No-recurrence"),
    event_date = case_when(!is.na(event) ~ lead(Date_Of_Tissue_Diagnosis.y))) %>%
  # Filter based on the count of distinct dates
  #filter(first(second_invasive) == "non-invasive")
  filter((n_distinct(Date_Of_Tissue_Diagnosis.y) == 2 & row_number() == 1) | 
           (n_distinct(Date_Of_Tissue_Diagnosis.y) == 3 & row_number() <= 2)) %>%
  filter(event_date > Date_Of_Tissue_Diagnosis.y) %>%
  ungroup() 

write.csv(BCFNZ_lesion_event, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_lesion_event.csv",row.names=FALSE)


####identify new diagnosis from NZCR, also compare the date with the last definitive surgery
BCFNZ_new_2275 <- BCFNZ_DCIS_lesion_surgery %>%
  select(PatientNo, Date_Of_Tissue_Diagnosis, BreastSide) %>%
  inner_join(
    CAS2275 %>% 
      select(NEW_MASTER_ENCRYPTED_HCU_ID, DIAGNOSIS_DATE, SITE_CODE_10,LATERALITY_CODE)%>%
      filter(grepl("C50|D05", SITE_CODE_10)) %>%
      left_join(Cohort, by="NEW_MASTER_ENCRYPTED_HCU_ID"),by = c("PatientNo" = "PATIENTNO")) %>%
  left_join(BCFNZ_surgery_1 %>%
              select(PatientNo,Date_Of_Tissue_Diagnosis,Surgery,surgery_side,Surgery_definitive_date),by=c("PatientNo","Date_Of_Tissue_Diagnosis")) %>%
  left_join(BCFNZ_RT_1,by=c("PatientNo","Date_Of_Tissue_Diagnosis")) %>%
  mutate(
    DIAGNOSIS_DATE = as.Date(DIAGNOSIS_DATE), RT_start_date=as.Date(RT_start_date),Surgery_definitive_date = as.Date(Surgery_definitive_date),
    Date_Of_Tissue_Diagnosis = as.Date(Date_Of_Tissue_Diagnosis))%>%
  filter(DIAGNOSIS_DATE>=Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis)%>%
  arrange(DIAGNOSIS_DATE)%>%
  filter(
    ((n() > 1 & row_number() == 2 & DIAGNOSIS_DATE > Surgery_definitive_date) | 
       (n() == 1 & DIAGNOSIS_DATE > Surgery_definitive_date)) & 
      ((is.na(RT_start_date) & (DIAGNOSIS_DATE - Surgery_definitive_date > 89)) |
         (!is.na(RT_start_date) & (DIAGNOSIS_DATE - pmax(Surgery_definitive_date, RT_start_date) > 89))))%>%  
  mutate(new_event_2275=case_when(
    BreastSide == LATERALITY_CODE & grepl("C50", SITE_CODE_10) ~ "iIBTR",
    BreastSide == LATERALITY_CODE & grepl("D05", SITE_CODE_10) ~ "DCIS-IBTR",
    ###when laterality code as unknown, categorize the recurrence as ipsilateral
    LATERALITY_CODE=="U" &  grepl("C50", SITE_CODE_10) ~ "iIBTR",  
    LATERALITY_CODE=="U" &  grepl("D05", SITE_CODE_10) ~ "DCIS-IBTR",
    BreastSide != LATERALITY_CODE & grepl("C50", SITE_CODE_10) ~ "iCBC",
    BreastSide != LATERALITY_CODE & grepl("D05", SITE_CODE_10) ~ "DCIS-CBC",))%>%
  ungroup()%>%
  select(-c(NEW_MASTER_ENCRYPTED_HCU_ID,BreastSide,SITE_CODE_10,LATERALITY_CODE,Surgery,surgery_side,Surgery_definitive_date,RT_side,RT_adjuvant,RT_start_date))

write.csv(BCFNZ_new_2275, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_new_2275.csv",row.names=FALSE)


BCFNZ_metas <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/MetastaticDisease.xlsx")
## contained unique patient information
BCFNZ_metas_1 <- BCFNZ_metas %>%
  select(PatientNo,`Date Of Tissue Diagnosis` ,`Date of metastatic disease`, `Site Of Metastatic Disease`) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`,Date_of_metastatic_disease =`Date of metastatic disease`, Site_Of_Metastatic_Disease=`Site Of Metastatic Disease`)%>%
  filter(!is.na(Date_of_metastatic_disease), Date_of_metastatic_disease > Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis)%>%
  arrange(Date_of_metastatic_disease)%>%
  slice(1)%>%
  ungroup()  

##### any event, combine LRR, surgery LRR, NZCR_2275 and metastasis####
BCFNZ_lrr_surgery_lrr_event <- BCFNZ_surgery_lrr_event %>%
  full_join(BCFNZ_lesion_event, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  full_join(BCFNZ_metas_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  full_join(BCFNZ_new_2275,by = c("PatientNo", "Date_Of_Tissue_Diagnosis"))%>%
  arrange(Date_Of_Tissue_Diagnosis) %>%
  group_by(PatientNo) %>%
  mutate(
    ###Recurrence == locoregional recurrence > IBTR from lesion type > new_event_2275 from NZCR > metastasis
    Recurrence = case_when(
      !is.na(LRR_surgery_date) & LRR_surgery_date == Date_Of_Tissue_Diagnosis ~ "No-recurrence",
      is.na(LRR_surgery_date) & !is.na(event_date) ~ event,
      !is.na(LRR_surgery_date) & !is.na(event_date) & (event_date < LRR_surgery_date) ~ event,
      !is.na(LRR_surgery_date) & !is.na(event_date) & (event_date >= LRR_surgery_date) ~ LRR_surgery,
      LRR_surgery == "LRR" & !is.na(new_event_2275) ~ new_event_2275,
      !is.na(LRR_surgery_date) & is.na(event_date) ~ LRR_surgery,
      !is.na(new_event_2275) ~ new_event_2275,
      is.na(LRR_surgery_date) & is.na(event_date) & is.na(new_event_2275) & !is.na(Date_of_metastatic_disease) ~ "metastasis"),
    Recurrence_Date = case_when(
      is.na(LRR_surgery_date) & !is.na(event_date) ~ event_date,
      !is.na(LRR_surgery_date) & !is.na(event_date) & (event_date < LRR_surgery_date) ~ event_date,
      !is.na(LRR_surgery_date) & !is.na(event_date) & (event_date >= LRR_surgery_date) ~ LRR_surgery_date,
      LRR_surgery=="LRR" & !is.na(new_event_2275) ~ DIAGNOSIS_DATE,
      !is.na(LRR_surgery_date) & is.na(event_date)  ~ LRR_surgery_date,
      !is.na(DIAGNOSIS_DATE) ~ DIAGNOSIS_DATE,
      is.na(LRR_surgery_date) & is.na(event_date) & is.na(new_event_2275) & !is.na(Date_of_metastatic_disease) ~ Date_of_metastatic_disease,)) %>%
  slice(1) %>%
  ungroup()

write.csv(BCFNZ_lrr_surgery_lrr_event, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_lrr_surgery_lrr_event.csv",row.names=FALSE)



##follow-up
BCFNZ_follow <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/Followup.xlsx")
BCFNZ_follow_1 <- BCFNZ_follow %>%
  select(PatientNo,`Date Of Tissue Diagnosis` ,DateOfFollowup, `Current Status of Disease`) %>%
  rename(Date_Of_Tissue_Diagnosis=`Date Of Tissue Diagnosis`,Current_Status_of_Disease =`Current Status of Disease`)%>%
  filter(!is.na(DateOfFollowup), DateOfFollowup > Date_Of_Tissue_Diagnosis)%>%
  group_by(PatientNo,Date_Of_Tissue_Diagnosis)%>%
  arrange(DateOfFollowup)%>%
  slice(n())%>%
  ungroup()  

####### any event, further combine with follow-up
BCFNZ_DCIS_lesion_surgery_lrr_event <- BCFNZ_DCIS_lesion_surgery %>%
  left_join(BCFNZ_lrr_surgery_lrr_event %>%
              select(PatientNo,Date_Of_Tissue_Diagnosis,DIAGNOSIS_DATE,new_event_2275,Recurrence,Recurrence_Date) , by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  left_join(BCFNZ_follow_1, by = c("PatientNo", "Date_Of_Tissue_Diagnosis")) %>%
  filter(Local != "No surgery") %>%
  mutate(
    Recurrence = case_when(
      is.na(Recurrence) & !is.na(Cause_Of_Death) & Cause_Of_Death %in% c("Other", "Unknown") ~ "Death from other",
      is.na(Recurrence) & !is.na(Cause_Of_Death) & Cause_Of_Death == "Breast cancer" ~ "Death from BC",
      is.na(Recurrence) ~ "No-recurrence",
      TRUE ~ Recurrence),
    Recurrence_Date = case_when(
      Recurrence == "No-recurrence" & !is.na(Surgery_definitive_date) ~ pmax(Date_Of_Tissue_Diagnosis,DateOfFollowup, Surgery_definitive_date,RT_start_date, na.rm = TRUE),  
      ##If DateOfSurgery is later than DateOfFollowup, use DateOfSurgery as the LRR_Date.
      is.na(Recurrence_Date) & !is.na(Cause_Of_Death) ~ Date_Of_Death,
      is.na(Recurrence_Date) & is.na(Cause_Of_Death) & !is.na(DateOfFollowup) ~ DateOfFollowup,
      TRUE ~ Recurrence_Date))

write.csv(BCFNZ_DCIS_lesion_surgery_lrr_event, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion_surgery_lrr_event.csv",row.names=FALSE)






# adjuvant ET, used to cross-check with hormone recetpor postive patients
BCFNZ_ET <- read_excel("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/De identify/HormoneTherapy.xlsx")

BCFNZ_ET_1 <- BCFNZ_ET %>%
  select(PatientNo, `Date Of Tissue Diagnosis`, `Timing of Hormone Therapy`,HormoneTherapy,`Start Date of Hormone Therapy`,StopDateHormoneTherapy) %>%
  rename(Date_Of_Tissue_Diagnosis = `Date Of Tissue Diagnosis`, 
         TimingofHormoneTherapy=`Timing of Hormone Therapy`, StartDateofHormoneTherapy=`Start Date of Hormone Therapy`)%>%
  filter(TimingofHormoneTherapy=="Adjuvant" & year(Date_Of_Tissue_Diagnosis)>1999 & !is.na(HormoneTherapy))%>%
  group_by(PatientNo)%>%
  slice(1)






# use the first pharmas dataset
Cohort <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/BCFNZ-MOH/Cohort.csv")

BCFNZ_DCIS_lesion_surgery_lrr_event <- read.csv("D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion_surgery_lrr_event.csv")


BCFNZ_DCIS_lesion_surgery_lrr_event_ET <- BCFNZ_DCIS_lesion_surgery_lrr_event %>%
  left_join(Cohort, by = c("PatientNo"="PATIENTNO")) %>%
  left_join(phh1211_ET, by = c("NEW_MASTER_ENCRYPTED_HCU_ID" = "new_master_enc"))%>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%  # Group by PatientNo and Date_Of_Tissue_Diagnosis
  filter(
    is.na(DATE_DISPENSED) |
      # Keep the min DATE_DISPENSED within 3 years of Date_Of_Tissue_Diagnosis
      (min(DATE_DISPENSED) >= Date_Of_Tissue_Diagnosis & 
         min(DATE_DISPENSED) <= (Date_Of_Tissue_Diagnosis + years(2))))%>%
  ungroup()

write.csv(BCFNZ_DCIS_lesion_surgery_lrr_event_ET, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_lesion_surgery_lrr_event_ET.csv",row.names=FALSE)


BCFNZ_DCIS_ET_MPR <- BCFNZ_DCIS_lesion_surgery_lrr_event_ET %>%
  mutate(
    Date_Of_Tissue_Diagnosis = as.Date(Date_Of_Tissue_Diagnosis),  
    DATE_DISPENSED = as.Date(DATE_DISPENSED, format = "%d/%m/%Y"))%>%
  group_by(PatientNo, Date_Of_Tissue_Diagnosis) %>%  # Group by PatientNo and Date_Of_Tissue_Diagnosis
  filter(!is.na(DATE_DISPENSED)) %>%
  # Keep the min DATE_DISPENSED within 3 years of Date_Of_Tissue_Diagnosis
  arrange(DATE_DISPENSED)%>%
  mutate(
    # Only calculate min and max if there are non-NA DATE_DISPENSED values
    Date_first_despensed = ifelse(all(is.na(DATE_DISPENSED)), NA, min(DATE_DISPENSED, na.rm = TRUE)),
    Date_last_despensed = ifelse(all(is.na(DATE_DISPENSED)), NA, max(DATE_DISPENSED, na.rm = TRUE)),
    Date_first_despensed = as.Date(Date_first_despensed, format = "%d/%m/%Y"),
    Date_last_despensed = as.Date(Date_last_despensed, format = "%d/%m/%Y"),
    consumption=sum(QUANTITY_DISPENSED),
    MPR = consumption / (as.numeric(Date_last_despensed - Date_first_despensed)+last(QUANTITY_DISPENSED))) %>%
  slice(1) %>%  # Keep only the first row per group
  filter(as.numeric(difftime(Date_last_despensed, Date_first_despensed, units = "days")) / 362.25 >= 2 ) %>%
  ungroup() 



write.csv(BCFNZ_DCIS_ET_MPR, file="D:/UOA/DCIS/NZ data/Data set/BCFNZ_240729/Result/Endocrine/BCFNZ_DCIS_ET_MPR.csv",row.names=FALSE)




