library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#Load data
##########################################################################################
#1. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#2.Load SCR
raw_SCR_df <- read.csv(paste0(raw_dir,"SCR_ALL_VALUES.csv"),stringsAsFactors = F)
raw_SCR_df <- raw_SCR_df[-which(is.na(raw_SCR_df$SCR_VALUE) == T),] #remove NA values
raw_SCR_df <- raw_SCR_df[!duplicated(raw_SCR_df[,c("STUDY_PATIENT_ID","SCR_ENTERED")]),] #remove duplicated entry

#3. Load demo for resolve MDRD
All_RACE_GENDER_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

##########################################################################################
#anlaysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#Features to extract :
#1.Baseline Scr (The outpatient sCr value closest to 1 day before hospital admission up to 1 year. 
#                If no outpatient sCr, use the inpatient sCr value closet to 7 days before index hospital admission up to 1 year. 
#                If no both inpt and outpt baseline sCr,  resolved the eGFR by MDRD equation for 75 mL/min/1.73m2 to determine a baseline SCr value.)
#2.Admit SCr (First SCr after ICU admission)
#3.Peak SCr (Highest Value ICU D0-D3)
#4.Num Scr  (ICU D0-D3)
#Note:  ICU D0 refers to ICU admit time to the same day at 23:59:59
##########################################################################################
#1.Get baseline Scr and other Scr
options(warn=2)
Final_SCR_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 6))
colnames(Final_SCR_df) <- c("STUDY_PATIENT_ID","Baseline_SCr","AdmitICU_SCr","AdmitICU_SCr_TIME",
                            "Peak_SCr_inICU_D0_D3","NUM_SCr_inICU_D0_D3")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Final_SCR_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd_hms(curr_time_df[,"Updated_HOSP_ADMIT_DATE"])
  curr_hosp_end   <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end   <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  
  #All SCr df
  curr_scr_df <- raw_SCR_df[which(raw_SCR_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #baseline Scr
  curr_bl_scr <- get_baseline_scr_func(curr_hosp_start,curr_scr_df)
  Final_SCR_df[i,"Baseline_SCr"] <- curr_bl_scr
  
  
  #Get ICU D0 to D3 start and end time
  curr_ICU_D0D3_time_df <- get_ICUD0_D3_dates_func(curr_icu_start,curr_icu_end)
  
  #Curr Scr in ICUD0_D3
  last_ICU_time <- ymd_hms(max(curr_ICU_D0D3_time_df[,"Day_End"],na.rm = T)) #it could be the end of ICU or the 3day of ICU
  #it could be less than 3 days if ICU duration is < 3 days
  curr_scr_inICUD0D3 <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,last_ICU_time,"SCR_ENTERED")
  
  if (nrow(curr_scr_inICUD0D3) > 0 ){ #if any value in ICU D0 to D3
    #admit Scr
    curr_1st_scr_afterICU_idx <- which(curr_scr_inICUD0D3[,"SCR_ENTERED"] == min(curr_scr_inICUD0D3[,"SCR_ENTERED"]))
    curr_admit_scr <- curr_scr_inICUD0D3[curr_1st_scr_afterICU_idx,"SCR_VALUE"]
    curr_admit_scr_time <- curr_scr_inICUD0D3[curr_1st_scr_afterICU_idx,"SCR_ENTERED"]
    Final_SCR_df[i,"AdmitICU_SCr"] <- curr_admit_scr
    Final_SCR_df[i,"AdmitICU_SCr_TIME"] <- curr_admit_scr_time
    
    #peak Scr in ICU D0 -D3
    curr_peak_scr <- max(curr_scr_inICUD0D3[,"SCR_VALUE"])
    curr_num_scr  <- nrow(curr_scr_inICUD0D3)
    Final_SCR_df[i,"Peak_SCr_inICU_D0_D3"] <- curr_peak_scr
    Final_SCR_df[i,"NUM_SCr_inICU_D0_D3"] <- curr_num_scr
    
  }else{
    Final_SCR_df[i,"NUM_SCr_inICU_D0_D3"] <- 0
  }
  
}


#2. resolve baseline Scr by MDRD
ct <- 1
no_bl_scr_IDs <- NA
for (i in 1:nrow(Final_SCR_df)){
  curr_id <- Final_SCR_df[i,"STUDY_PATIENT_ID"]
  curr_scr <- Final_SCR_df[i,"Baseline_SCr"]
  if (is.na(curr_scr) == T){
    no_bl_scr_IDs[ct] <-  curr_id
    ct <- ct+ 1
    curr_demo_df <- All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"STUDY_PATIENT_ID"] == curr_id),]
    curr_age <- curr_demo_df[,"AGE"]
    curr_gender <- curr_demo_df[,"GENDER"]
    curr_race <- curr_demo_df[,"RACE"]
    
    if (is.na(curr_age) == F & is.na(curr_gender) == F & is.na(curr_race) == F){
      curr_bl_val <- SolveScr_reverse_MDRD_equation(curr_age,curr_gender,curr_race)
      Final_SCR_df[i,"Baseline_SCr"] <- curr_bl_val
    }
    
  }
}

write.csv(Final_SCR_df,paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"))


length(no_bl_scr_IDs) ##N of Resolved baseline by MRDR: 25195
#Indxes of No Baseline Scr after MDRD due to age, gender and race missing
no_bl_scr_idxes <- which(is.na(Final_SCR_df[,"Baseline_SCr"])==T) #435
no_bl_scr_idxes


##########################################################################################
#KDIGO
#1.Admit KDIGO
#2.Maximum KDIGO 	(Maximum KDIGO score in ICU D0 to D3)
#3.Last    KDIGO  (Last KDIGO score    in ICU D0 to D3)
##########################################################################################
KDIGO_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 4))
colnames(KDIGO_df) <- c("STUDY_PATIENT_ID","Admit_KDIGO_ICU","MAX_KDIGO_ICU_D0toD3","LAST_KDIGO_ICU_D0toD3")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  KDIGO_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #baseline Scr
  curr_baseline_scr <- Final_SCR_df[which(Final_SCR_df[,"STUDY_PATIENT_ID"] ==curr_id),"Baseline_SCr"]
  #admit Scr
  curr_admit_scr <- Final_SCR_df[which(Final_SCR_df[,"STUDY_PATIENT_ID"] ==curr_id),"AdmitICU_SCr"]
  #admit Scr time
  curr_admit_scr_time <- Final_SCR_df[which(Final_SCR_df[,"STUDY_PATIENT_ID"] ==curr_id),"AdmitICU_SCr_TIME"]
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  curr_crrt_start <- ymd_hms(curr_time_df[,"Updated_CRRT_Start"])
  curr_crrt_end <- ymd_hms(curr_time_df[,"Updated_CRRT_End"])
  curr_hd_start <- ymd_hms(curr_time_df[,"Updated_HD_Start"])
  curr_hd_end <- ymd_hms(curr_time_df[,"Updated_HD_End"])
  
  #All SCr df
  curr_scr_df <- raw_SCR_df[which(raw_SCR_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Get ICU D0 to D3 start and end time
  curr_ICU_D0D3_time_df <- get_ICUD0_D3_dates_func(curr_icu_start,curr_icu_end)
  
  #Curr Scr in ICUD0_D3
  last_ICU_time <- ymd_hms(max(curr_ICU_D0D3_time_df[,"Day_End"],na.rm = T)) #it could be the end of ICU or the 3day of ICU
  #it could be less than 3 days if ICU duration is < 3 days
  curr_scr_inICUD0D3 <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,last_ICU_time,"SCR_ENTERED")
  
  if (nrow(curr_scr_inICUD0D3) > 0 & is.na(curr_baseline_scr) == F){
    #current KIDGO for all Scr in window
    curr_KDIGO_df    <- get_KDIGO_Score_forScrdf_func(curr_baseline_scr,curr_scr_inICUD0D3)
    #update if each time step is in RRT duration, if so, update the KDIGO score to 4
    if (is.na(curr_crrt_start) == F | is.na(curr_hd_start) == F){
      curr_updated_KDIGO_df <- update_KDIGO_df_forRRT_func(curr_KDIGO_df,curr_crrt_start,curr_crrt_end,curr_hd_start,curr_hd_end,48)
    }else{
      curr_updated_KDIGO_df <-  curr_KDIGO_df #no updates
    }
    
    last_scr_time_point  <- max(curr_scr_inICUD0D3[,"SCR_ENTERED"])
    
    KDIGO_df[i,"MAX_KDIGO_ICU_D0toD3"]   <- max(curr_updated_KDIGO_df[,"KDIGO"])
    KDIGO_df[i,"LAST_KDIGO_ICU_D0toD3"]  <- curr_updated_KDIGO_df[which(curr_updated_KDIGO_df[,"Scr_Time"] == last_scr_time_point),"KDIGO"]
    KDIGO_df[i,"Admit_KDIGO_ICU"] <- curr_updated_KDIGO_df[which(curr_updated_KDIGO_df[,"Scr_Time"] == curr_admit_scr_time),"KDIGO"]
    
  }
  
}

write.csv(KDIGO_df,paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"))


########################################################################################
# Baseline EGFR
##########################################################################################
#1.Load Baseline SCR df
Final_SCR_df <- read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)
Final_SCR_df <- Final_SCR_df[,-1]

#2.Conpute basleine EGFR
Baseline_EGFR_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Baseline_EGFR_df) <- c("STUDY_PATIENT_ID","Baseline_eGFR")

for (p in 1:length(analysis_ID)){
  if (p %% 100 == 0){
    print(p)
  }
  curr_id <- analysis_ID[p]
  Baseline_EGFR_df[p,"STUDY_PATIENT_ID"] <- curr_id
  
  curr_baseline_scr <- Final_SCR_df[which(Final_SCR_df[,"STUDY_PATIENT_ID"] == curr_id),"Baseline_SCr"]
  curr_demo_df <- All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"STUDY_PATIENT_ID"]==curr_id),]
  curr_age <- curr_demo_df[,"AGE"]
  curr_race <-  curr_demo_df[,"RACE"]
  curr_gender <-  curr_demo_df[,"GENDER"]
  
  if (is.na(curr_age)== T | is.na(curr_gender)== T |is.na(curr_race)== T ){
    Baseline_EGFR_df[p,"Baseline_eGFR"] <- NA
  }else{
    Baseline_EGFR_df[p,"Baseline_eGFR"] <- MDRD_equation(curr_baseline_scr,curr_age,curr_gender,curr_race)
  }
}
write.csv(Baseline_EGFR_df,paste0(outdir,"Baseline_EGFR.csv"),row.names=FALSE)


##########################################################################################
#Get all Outpatient Scr df
##########################################################################################
All_OutptSCr_df <- raw_SCR_df[which(grepl("Outpatient",raw_SCR_df[,"SCR_ENCOUNTER_TYPE"]) == T),]

########################################################################################
##### Compute EGFR at (ICU DC to 120 days)
##### 1. return Scr used for computation (All Scr values which within 30 days to the cloest value, and cloest value)
##### 2. EGFR is using the median of all Scr in 1.
########################################################################################
time_window_start <- days(120) #120 days after ICU discharge
time_window_expansion <- days(30) #30 days before (the cloest record time to 180 days )
res <- compute_EGFR_inWindow_func2(time_window_start,time_window_expansion,analysis_ID,All_time_df,All_RACE_GENDER_df,All_OutptSCr_df)
EGFR_120_df <- res[[1]]
write.csv(EGFR_120_df,paste0(outdir,"EGFR_120.csv"),row.names=FALSE)
ScrUsed_120_df <- res[[2]]
write.csv(ScrUsed_120_df,paste0(outdir,"EGFR_ScrUsed_120.csv"),row.names=FALSE) #All Scr in Time Window less than 30 days apar



########################################################################################
##           5. EGFR drop > 50% ICU_DC to 120 days
########################################################################################
eGFR_120_df <- read.csv(paste0(outdir,"EGFR_120.csv"), stringsAsFactors = F)
bl_eGFR_df <- read.csv(paste0(outdir,"Baseline_EGFR.csv"), stringsAsFactors = F)
eGFR_colname <- "EGFR_120d" 
drop_cutoff <- 0.5

EGFR_drop <- NA
for (p in 1:length(analysis_ID)){
  curr_id <- analysis_ID[p]
  curr_bl_egfr <- bl_eGFR_df[which(bl_eGFR_df[,"STUDY_PATIENT_ID"]==curr_id),"Baseline_eGFR"]
  curr_tw_egfr <- eGFR_120_df[which(eGFR_120_df[,"STUDY_PATIENT_ID"]==curr_id),eGFR_colname]
  

  if (is.na(curr_bl_egfr) == F & is.na(curr_tw_egfr) == F){
    if ( curr_tw_egfr< curr_bl_egfr*(1-drop_cutoff)){
      EGFR_drop[p] <- 1
    }else{
      EGFR_drop[p] <- 0
    }
  }else{
    EGFR_drop[p] <- NA
  }
  
}

EGFR_Drop_df <- cbind.data.frame(analysis_ID,EGFR_drop)
colnames(EGFR_Drop_df) <- c("STUDY_PATIENT_ID",paste0("EGFR_drop",drop_cutoff*100))

write.csv(EGFR_Drop_df,paste0(outdir,"EGFR_Drop50_df.csv"),row.names=FALSE)
