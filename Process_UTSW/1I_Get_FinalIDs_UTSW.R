library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#1.Age
Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

#2.ESRD
Final_ESRD_BEFORE_AT_df <- read.csv(paste0(outdir,"ESRD_Before_AT.csv"),stringsAsFactors = F)

#3. baseline eGFR
Baseline_EGFR_df <-read.csv(paste0(outdir,"Baseline_EGFR.csv"),stringsAsFactors = F)

#4. kidney Transplant
KidneyTransplant_df <-read.csv(paste0(outdir,"KidneyTransplant.csv"),stringsAsFactors = F)

#5. Scr num in D0-D3
Src_df <-read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)

#6. KIDGO in D0-D3
KDIGO_df <-read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)


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
#Compare to xilong file 
#'@NOTE: This section can be delete later
##########################################################################################
xilong_exclusion_df <- read.xlsx("/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/xilong_extracted/Patient list of deleting 07062021.xlsx",sheet= 1)
IDs_inxilong <- unique(xilong_exclusion_df$PATIENT_NUM)

exclusionFeature_df <- as.data.frame(matrix(NA, nrow = length(IDs_inxilong), ncol = 10))
colnames(exclusionFeature_df) <- c("PATIENT_NUM","AGE","ESRD_BEOFRE","Baseline_SCr","Baseline_eGFR","KidneyTrans_BEFOREorDURING",
                                   "NUM_SCr_inICU_D0toD3","MAX_KDIGO_ICU_D0toD3","ICU_LOS_Hours","Death_ICU_D0toD3")
for (i in  1:length(IDs_inxilong)){
  if(i %% 1000 == 0){print(i)}
  curr_id <- IDs_inxilong[i]
  exclusionFeature_df[i,"PATIENT_NUM"] <- curr_id
  
  #Add exclusion features
  if (curr_id %in% Inclusion_df$STUDY_PATIENT_ID){
      exclusionFeature_df[i,"AGE"] <- Demo_df[which(Demo_df[,"STUDY_PATIENT_ID"] == curr_id),"AGE"]
      exclusionFeature_df[i,"ESRD_BEOFRE"] <- Final_ESRD_BEFORE_AT_df[which(Final_ESRD_BEFORE_AT_df[,"STUDY_PATIENT_ID"] == curr_id),"ESRD_BEFORE_AT"]
      exclusionFeature_df[i,"Baseline_SCr"] <- Src_df[which(Src_df[,"STUDY_PATIENT_ID"] == curr_id),"Baseline_SCr"]
      exclusionFeature_df[i,"Baseline_eGFR"] <- Baseline_EGFR_df[which(Baseline_EGFR_df[,"STUDY_PATIENT_ID"] == curr_id),"Baseline_eGFR"]
      exclusionFeature_df[i,"KidneyTrans_BEFOREorDURING"] <- KidneyTransplant_df[which(KidneyTransplant_df[,"STUDY_PATIENT_ID"] == curr_id),"KidneyTrans_BEFOREorDURING"]
      
      exclusionFeature_df[i,"NUM_SCr_inICU_D0toD3"] <- Src_df[which(Src_df[,"STUDY_PATIENT_ID"] == curr_id),"NUM_SCr_inICU_D0_D3"]
      exclusionFeature_df[i,"ICU_LOS_Hours"]<- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),"ICU_LOS_Hours"]
      
      exclusionFeature_df[i,"Death_ICU_D0toD3"]<- All_Mortality_df[which(All_Mortality_df[,"STUDY_PATIENT_ID"] == curr_id),"Death_ICU_D0toD3"]
      
      exclusionFeature_df[i,"MAX_KDIGO_ICU_D0toD3"]<- KDIGO_df[which(KDIGO_df[,"STUDY_PATIENT_ID"] == curr_id),"MAX_KDIGO_ICU_D0toD3"]
      
  }

}


#This pateint are excluded before this process:
#1.CRRT/HD start = CRRT/HD end
#2.ICU dates does not cover CRRT date 
#3.HOSP date does not cover CRRT/HD/ICU date
#4.DOD dates before HOSP admission/ICU admission
exclusionFeature_df$Time_Exclusion <- 0
time_exclusion_indxes <- which(is.na(exclusionFeature_df$AGE) == T) #108
exclusionFeature_df$Time_Exclusion[time_exclusion_indxes] <- 1

write.csv(exclusionFeature_df,paste0(outdir,"Exclusion_Feature_Final.csv"),row.names = F)

Final_ID_df <- exclusionFeature_df[-which(exclusionFeature_df$Time_Exclusion == 1),] #-108
exclusion1<- which(Final_ID_df$AGE < 18 & Final_ID_df$AGE > 0) #0
if(length(exclusion1) > 0){
Final_ID_df <- Final_ID_df[-exclusion1,]
}

exclusion2 <- which(Final_ID_df[,"Baseline_eGFR"] < 15) #222
if(length(exclusion2) > 0){
  Final_ID_df <- Final_ID_df[-exclusion2,] 
}

exclusion3 <- which(Final_ID_df[,"KidneyTrans_BEFOREorDURING"] ==1) #12
if(length(exclusion3) > 0){
  Final_ID_df <- Final_ID_df[-exclusion3,] 
}

exclusion4 <- which(Final_ID_df[,"NUM_SCr_inICU_D0toD3"] < 1) #0
print(length(exclusion4))
if(length(exclusion4) > 0){
  Final_ID_df <- Final_ID_df[-exclusion4,] 
}

exclusion5 <- which(Final_ID_df[,"MAX_KDIGO_ICU_D0toD3"] == 0) #7513
print(length(exclusion5))
if(length(exclusion5) > 0){
  Final_ID_df <- Final_ID_df[-exclusion5,] 
}

exclusion6 <- which(Final_ID_df[,"ICU_LOS_Hours"] < 24) #0
print(length(exclusion6))
if(length(exclusion6) > 0){
  Final_ID_df <- Final_ID_df[-exclusion6,] 
}

exclusion7 <- which(Final_ID_df[,"Death_ICU_D0toD3"] == 1) #137
print(length(exclusion7))
if(length(exclusion7) > 0){
  Final_ID_df <- Final_ID_df[-exclusion7,] 
}

exclusion8 <- which(Final_ID_df[,"ESRD_BEOFRE"] == 1) #260
print(length(exclusion8))
if(length(exclusion8) > 0){
  Final_ID_df <- Final_ID_df[-exclusion8,] 
}

(nrow(Final_ID_df)) #2233

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
length(actual_exclusion_IDs1) #0
length(updated_inclusion_IDs1) #10502

#Exclude 2- Baseline eGFR <15
ExclusionID2 <- Baseline_EGFR_df[which(Baseline_EGFR_df[,"Baseline_eGFR"] < 15),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs1,ExclusionID2)
actual_exclusion_IDs2 <- res[[1]] 
updated_inclusion_IDs2 <- res[[2]]
length(actual_exclusion_IDs2) #222
length(updated_inclusion_IDs2) #10280

#Exclude 3- Kidney transplant before or during hospitalization
ExclusionID3 <- KidneyTransplant_df[which(KidneyTransplant_df[,"KidneyTrans_BEFOREorDURING"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs2,ExclusionID3)
actual_exclusion_IDs3 <- res[[1]] 
updated_inclusion_IDs3 <- res[[2]]
length(actual_exclusion_IDs3) #12
length(updated_inclusion_IDs3) #  10268

#Exclude 4- <1 SCr measurement in the first 3 days of ICU admission (D0 to D3)
ExclusionID4 <- Src_df[which(Src_df[,"NUM_SCr_inICU_D0_D3"] < 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs3,ExclusionID4)
actual_exclusion_IDs4 <- res[[1]] 
updated_inclusion_IDs4 <- res[[2]]
length(actual_exclusion_IDs4) #215
length(updated_inclusion_IDs4) #10053

#Exclude 5- No AKI in the first 3 days of ICU admission (D0 to D3)
ExclusionID5 <- KDIGO_df[which(KDIGO_df[,"MAX_KDIGO_ICU_D0toD3"] == 0 |is.na(KDIGO_df[,"MAX_KDIGO_ICU_D0toD3"])==T),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs4,ExclusionID5)
actual_exclusion_IDs5 <- res[[1]] 
updated_inclusion_IDs5 <- res[[2]]
length(actual_exclusion_IDs5) #7515
length(updated_inclusion_IDs5) # 2540

#Exclude 6- <24 hours of ICU stay
ExclusionID6 <- All_time_df[which(All_time_df[,"ICU_LOS_Hours"] < 24),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs5,ExclusionID6)
actual_exclusion_IDs6 <- res[[1]] 
updated_inclusion_IDs6 <- res[[2]]
length(actual_exclusion_IDs6) #0
length(updated_inclusion_IDs6) # 2540

#Exclude 7- Died in the first 3 days (D0 to D3) of ICU admission
ExclusionID7 <- All_Mortality_df[which(All_Mortality_df[,"Death_ICU_D0toD3"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs6,ExclusionID7)
actual_exclusion_IDs7 <- res[[1]] 
updated_inclusion_IDs7 <- res[[2]]
length(actual_exclusion_IDs7) # 91
length(updated_inclusion_IDs7) # 2449

#Exclude 8 - ESRD (ESKD) diagnosis before hospitalization
ExclusionID8 <- Final_ESRD_BEFORE_AT_df[which(Final_ESRD_BEFORE_AT_df[,"ESRD_BEFORE_AT"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(updated_inclusion_IDs7,ExclusionID8)
actual_exclusion_IDs8 <- res[[1]] 
updated_inclusion_IDs8 <- res[[2]]
length(actual_exclusion_IDs8) # 216
length(updated_inclusion_IDs8) #2233


#analysis ID before exlusion of ESRD before and at
Final_Anlaysis_ID <-as.data.frame(updated_inclusion_IDs8)
colnames(Final_Anlaysis_ID) <- "STUDY_PATIENT_ID"
nrow(Final_Anlaysis_ID) #2233
write.csv(Final_Anlaysis_ID,paste0(outdir,"Final_Analysis_ID.csv"),row.names = F)
