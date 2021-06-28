library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#1.Age
Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

#3. baseline eGFR
Baseline_EGFR_df <-read.csv(paste0(outdir,"Baseline_EGFR.csv"),stringsAsFactors = F)

#4. kidney Transplant
KidneyTransplant_df <-read.csv(paste0(outdir,"KidneyTransplant.csv"),stringsAsFactors = F)

#5. Scr num in D0-D3
Src_df <-read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)

#6. KIDGO in D0-D3
KIDGO_df <-read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)


#7. ICU_LOS
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df$ICU_LOS_Hours <- NA
All_time_df$ICU_LOS_Hours <- as.numeric(difftime(ymd_hms(All_time_df[,"Updated_ICU_DISCHARGE_DATE"]),ymd_hms(All_time_df[,"Updated_ICU_ADMIT_DATE"]),units = "hours"))

#8. died in D0-D3
All_Mortality_df <-read.csv(paste0(outdir,"All_Mortality.csv"),stringsAsFactors = F)

#'@NOTE: we do not exclude these patient for now
# #9.Patient has no outpatient sCr after hospital discharge within 120 days (n_OutptScr_AfterHOSP_Before120d)
# eGFR_df <-read.csv(paste0(outdir,"EGFR_120.csv"),stringsAsFactors = F)


##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time and has all demo info
##########################################################################################
inclusion_ID <- Inclusion_df$STUDY_PATIENT_ID

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
length(updated_inclusion_IDs1) #36014

#Exclude 2- Baseline eGFR <15
ExclusionID2 <- Baseline_EGFR_df[which(Baseline_EGFR_df[,"Baseline_eGFR"] < 15),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs1,ExclusionID2)
actual_exclusion_IDs2 <- res[[1]] 
updated_inclusion_IDs2 <- res[[2]]
length(actual_exclusion_IDs2) #328
length(updated_inclusion_IDs2) #35686

#Exclude 3- Kidney transplant before or during hospitalization
ExclusionID3 <- KidneyTransplant_df[which(KidneyTransplant_df[,"KidneyTrans_BEFOREorDURING"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs2,ExclusionID3)
actual_exclusion_IDs3 <- res[[1]] 
updated_inclusion_IDs3 <- res[[2]]
length(actual_exclusion_IDs3) #274
length(updated_inclusion_IDs3) # 35412

#Exclude 4- <1 SCr measurement in the first 3 days of ICU admission (D0 to D3)
ExclusionID4 <- Src_df[which(Src_df[,"NUM_SCr_inICU_D0_D3"] < 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs3,ExclusionID4)
actual_exclusion_IDs4 <- res[[1]] 
updated_inclusion_IDs4 <- res[[2]]
length(actual_exclusion_IDs4) #3145
length(updated_inclusion_IDs4) #32267
  
#Exclude 5- No AKI in the first 3 days of ICU admission (D0 to D3)
ExclusionID5 <- KIDGO_df[which(KIDGO_df[,"MAX_KDIGO_ICU_D0toD3"] == 0 |is.na(KIDGO_df[,"MAX_KDIGO_ICU_D0toD3"])==T),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs4,ExclusionID5)
actual_exclusion_IDs5 <- res[[1]] 
updated_inclusion_IDs5 <- res[[2]]
length(actual_exclusion_IDs5) #22971
length(updated_inclusion_IDs5) # 9296

#Exclude 6- <24 hours of ICU stay
ExclusionID6 <- All_time_df[which(All_time_df[,"ICU_LOS_Hours"] < 24),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs5,ExclusionID6)
actual_exclusion_IDs6 <- res[[1]] 
updated_inclusion_IDs6 <- res[[2]]
length(actual_exclusion_IDs6) #1025
length(updated_inclusion_IDs6) #8271

#Exclude 7- Died in the first 3 days (D0 to D3) of ICU admission
ExclusionID7 <- All_Mortality_df[which(All_Mortality_df[,"Death_ICU_D0toD3"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs6,ExclusionID7)
actual_exclusion_IDs7 <- res[[1]] 
updated_inclusion_IDs7 <- res[[2]]
length(actual_exclusion_IDs7) #470
length(updated_inclusion_IDs7) #7801


# #Exclude 8- Patient has no outpatient sCr after hospital discharge within 120 days
# ExclusionID8 <- eGFR_df[which(eGFR_df[,"n_OutptScr_AfterHOSP_Before120d"] == 0),"STUDY_PATIENT_ID"]
# res <- exclude_pts_func(updated_inclusion_IDs7,ExclusionID8)
# actual_exclusion_IDs8 <- res[[1]] 
# updated_inclusion_IDs8 <- res[[2]]
# length(actual_exclusion_IDs8) #5969
# length(updated_inclusion_IDs8) #1832

#analysis ID before exlusion of ESRD before and at
Final_Anlaysis_ID <-as.data.frame(updated_inclusion_IDs7)
colnames(Final_Anlaysis_ID) <- "STUDY_PATIENT_ID"
nrow(Final_Anlaysis_ID) #7801
write.csv(Final_Anlaysis_ID,paste0(outdir,"Final_Analysis_ID_BeforeExclusionOfESRD.csv"),row.names = F)
