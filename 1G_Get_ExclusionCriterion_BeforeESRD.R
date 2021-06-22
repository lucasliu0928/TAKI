library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1.Age
Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)
no_demo_indexes <- which(is.na(Demo_df$GENDER)== T |is.na(Demo_df$RACE)== T | is.na(Demo_df$AGE)==T)
Demo_df <- Demo_df[-no_demo_indexes,]

#3. baseline eGFR
Baseline_EGFR_df <-read.csv(paste0(outdir,"Baseline_EGFR.csv"),stringsAsFactors = F)

#4. kidney Transplant
KidneyTransplant_df <-read.csv(paste0(outdir,"KidneyTransplant.csv"),stringsAsFactors = F)
#Add before or during column
KidneyTransplant_df[,"KidneyTrans_BEFORE_Or_DURING"] <- 0 
before_during_idxes <- which(KidneyTransplant_df$KidneyTrans_BEFORE== 1 | KidneyTransplant_df$KidneyTrans_DURING == 1)
KidneyTransplant_df[before_during_idxes,"KidneyTrans_BEFORE_Or_DURING"] <- 1

#5. Scr num in D0-D3
Src_df <-read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)

#6. KIDGO in D0-D3
KIDGO_df <-read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)

#7. died in D0-D4
All_Mortality_df <-read.csv(paste0(outdir,"All_Mortality.csv"),stringsAsFactors = F)

#8. ICU_LOS
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df$ICU_LOS_Hours <- NA
All_time_df$ICU_LOS_Hours <- as.numeric(difftime(ymd_hms(All_time_df[,"Updated_ICU_DISCHARGE_DATE"]),ymd_hms(All_time_df[,"Updated_ICU_ADMIT_DATE"]),units = "hours"))

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time and has all demo info
##########################################################################################
inclusion_ID <- intersect(unique(All_time_df[,"STUDY_PATIENT_ID"]), unique(Demo_df[,"STUDY_PATIENT_ID"]))

##########################################################################################
#2.Exclusion
##########################################################################################
exclude_pts_func <-function(analysis_IDs,exclusion_IDs){
    remove_idx <- which(analysis_IDs %in% exclusion_IDs) 
    if (length(remove_idx) > 0 ){ #if analysis_IDs has any ID qualifies exclusion
      acutal_exclusion_ID <- analysis_IDs[remove_idx] #The IDs in anlaysis Id qualifies exlucsion criteria
      updated_analysis_IDs <- analysis_IDs[-remove_idx]
    }else{ #remove nothing
      acutal_exclusion_ID <-NULL
      updated_analysis_IDs <- analysis_IDs
    }
    return(list(acutal_exclusion_ID,updated_analysis_IDs))
  }

#Exclude 1- <18 years old
ExclusionID1 <- Demo_df[which(Demo_df[,"AGE"] < 18),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(inclusion_ID,ExclusionID1)
actual_exclusion_IDs1 <- res[[1]] 
updated_inclusion_IDs1 <- res[[2]]
length(actual_exclusion_IDs1) #3
length(updated_inclusion_IDs1) #36091

#Exclude 2- Baseline eGFR <15
ExclusionID2 <- Baseline_EGFR_df[which(Baseline_EGFR_df[,"Baseline_eGFR"] < 15),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs1,ExclusionID2)
actual_exclusion_IDs2 <- res[[1]] 
updated_inclusion_IDs2 <- res[[2]]
length(actual_exclusion_IDs2) #331
length(updated_inclusion_IDs2) #35760

#Exclude 3- Kidney transplant before or during hospitalization
ExclusionID3 <- KidneyTransplant_df[which(KidneyTransplant_df[,"KidneyTrans_BEFORE_Or_DURING"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs2,ExclusionID3)
actual_exclusion_IDs3 <- res[[1]] 
updated_inclusion_IDs3 <- res[[2]]
length(actual_exclusion_IDs3) #82
length(updated_inclusion_IDs3) # 35678

#Exclude 4- <1 SCr measurement in the first 3 days of ICU admission (D0 to D3)
ExclusionID4 <- Src_df[which(Src_df[,"NUM_SCr_inICU_D0_D3"] < 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs3,ExclusionID4)
actual_exclusion_IDs4 <- res[[1]] 
updated_inclusion_IDs4 <- res[[2]]
length(actual_exclusion_IDs4) #3155
length(updated_inclusion_IDs4) #32523
  
#Exclude 5- No AKI in the first 3 days of ICU admission (D0 to D3)
ExclusionID5 <- KIDGO_df[which(KIDGO_df[,"MAX_KDIGO_ICU_D0toD3"] == 0),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs4,ExclusionID5)
actual_exclusion_IDs5 <- res[[1]] 
updated_inclusion_IDs5 <- res[[2]]
length(actual_exclusion_IDs5) #22850
length(updated_inclusion_IDs5) #9673

#Exclude 6- <24 hours of ICU stay
ExclusionID6 <- All_time_df[which(All_time_df[,"ICU_LOS_Hours"] < 24),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs5,ExclusionID6)
actual_exclusion_IDs6 <- res[[1]] 
updated_inclusion_IDs6 <- res[[2]]
length(actual_exclusion_IDs6) #1060
length(updated_inclusion_IDs6) #8613

#Exclude 7- Died in the first 4 days (D0 to D4) of ICU admission
ExclusionID7 <- All_Mortality_df[which(All_Mortality_df[,"Death_ICU_D0toD4"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs6,ExclusionID7)
actual_exclusion_IDs7 <- res[[1]] 
updated_inclusion_IDs7 <- res[[2]]
length(actual_exclusion_IDs7) #577
length(updated_inclusion_IDs7) #8036

#analysis ID before exlusion of ESRD before and at
Final_Anlaysis_ID <-as.data.frame(updated_inclusion_IDs7)
colnames(Final_Anlaysis_ID) <- "STUDY_PATIENT_ID"
nrow(Final_Anlaysis_ID) #8036
write.csv(Final_Anlaysis_ID,paste0(outdir,"Final_Analysis_ID_BeforeExclusionOfESRD.csv"),row.names = F)


