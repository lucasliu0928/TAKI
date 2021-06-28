library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#2. Corrected Time df
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#3.Load SCR
raw_SCR_df <- read.csv(paste0(raw_dir,"SCR_ALL_VALUES.csv"),stringsAsFactors = F)
raw_SCR_df <- raw_SCR_df[-which(is.na(raw_SCR_df$SCR_VALUE) == T),] #remove NA values
raw_SCR_df <- raw_SCR_df[!duplicated(raw_SCR_df[,c("STUDY_PATIENT_ID","SCR_ENTERED")]),] #remove duplicated entry

#3. Load demo for resolve EPI
All_RACE_GENDER_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

#Recode Race and Gender
All_RACE_GENDER_df$Gender_Male <- NA
male_idxes <- which(All_RACE_GENDER_df[,"GENDER"] == "M")
All_RACE_GENDER_df$Gender_Male[male_idxes] <- 1
All_RACE_GENDER_df$Gender_Male[-male_idxes] <- 0

All_RACE_GENDER_df$Race_Black <- NA
black_idxes <- which(All_RACE_GENDER_df[,"RACE"] == "BLACK/AFR AMERI")
All_RACE_GENDER_df$Race_Black[black_idxes] <- 1
All_RACE_GENDER_df$Race_Black[-black_idxes] <- 0
##########################################################################################
#anlaysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(Inclusion_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#Features to extract :
#1.Baseline Scr (The outpatient sCr value closest to 7 day before hospital admission up to 1 year. 
#                If no outpatient sCr, use the inpatient sCr value closet to 7 days before index hospital admission up to 1 year. 
#                If no both inpt and outpt baseline sCr,  resolved the eGFR by EPI equation for 75 mL/min/1.73m2 to determine a baseline SCr value.)
#2.Admit SCr (First SCr after ICU admission)
#3.Peak SCr (Highest Value ICU D0-D3)
#4.Num Scr  (ICU D0-D3)
#Note:  ICU D0 refers to ICU admit time to the same day at 23:59:59
##########################################################################################
#1.Get baseline Scr and other Scr
options(warn=2)
Final_SCR_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 7))
colnames(Final_SCR_df) <- c("STUDY_PATIENT_ID","Baseline_SCr","AdmitICU_SCr","AdmitICU_SCr_TIME",
                            "Peak_SCr_inICU_D0_D3","NUM_SCr_inICU_D0_D3","Lowest_SCr_inICU_D0_D3")
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
  
  #Get actual days/times in ICU D0-D3
  #it could be ICU end time (e.g, if ICU stays < 3 days) or the end of ICU D3
  curr_actual_ICU_time_idxes <- which(colnames(curr_time_df) %in% c("Actual_D0_End","Actual_D1_End","Actual_D2_End","Actual_D3_End"))
  curr_actual_ICU_time <- curr_time_df[,curr_actual_ICU_time_idxes]
  curr_last_ICU_time <- max(ymd_hms(curr_actual_ICU_time),na.rm = T)
  
  #Get curr scr in ICU D0-D3, could be less than 3 days
  curr_scr_inICUD0D3 <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,curr_last_ICU_time,"SCR_ENTERED")
  
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
    
    #Lowest Scr in ICU D0-D3
    curr_lowest_scr <- min(curr_scr_inICUD0D3[,"SCR_VALUE"])
    Final_SCR_df[i,"Lowest_SCr_inICU_D0_D3"] <- curr_lowest_scr
    
  }else{
    Final_SCR_df[i,"NUM_SCr_inICU_D0_D3"] <- 0
  }
  
}


#2. resolve baseline Scr by EPI
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
    curr_gender_male <- curr_demo_df[,"Gender_Male"]
    curr_race_black <- curr_demo_df[,"Race_Black"]
    
    if (is.na(curr_age) == F & is.na(curr_gender_male) == F & is.na(curr_race_black) == F){
      curr_bl_val <- SolveScr_reverse_EPI_equation(curr_age,curr_gender_male,curr_race_black)
      Final_SCR_df[i,"Baseline_SCr"] <- curr_bl_val
    }
    
  }
}

write.csv(Final_SCR_df,paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),row.names = F)


length(no_bl_scr_IDs) ##N of Resolved baseline by EPI: 25488


##########################################################################################
#KDIGO
#1.Admit KDIGO
#2.Maximum KDIGO 	(Maximum KDIGO score in ICU D0 to D3)
#3.Last    KDIGO  (Last KDIGO score    in ICU D0 to D3)
##########################################################################################
aval_ids <- Final_SCR_df$STUDY_PATIENT_ID[which(Final_SCR_df$NUM_SCr_inICU_D0_D3>0)]
aval_ids2 <- All_time_df$STUDY_PATIENT_ID[which(is.na(All_time_df$Updated_CRRT_Start)==F)]
intersect(aval_ids2,aval_ids)[1]

KDIGO_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 4))
colnames(KDIGO_df) <- c("STUDY_PATIENT_ID","Admit_KDIGO_ICU","MAX_KDIGO_ICU_D0toD3","LAST_KDIGO_ICU_D0toD3")
for (i in 1:length(analysis_ID)){
  i <- which(analysis_ID == 92)
  
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  KDIGO_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #baseline Scr
  curr_baseline_scr <- Final_SCR_df[which(Final_SCR_df[,"STUDY_PATIENT_ID"] ==curr_id),"Baseline_SCr"]

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
  
  #Get actual days/times in ICU D0-D3
  #it could be ICU end time (e.g, if ICU stays < 3 days) or the end of ICU D3
  curr_actual_ICU_time_idxes <- which(colnames(curr_time_df) %in% c("Actual_D0_End","Actual_D1_End","Actual_D2_End","Actual_D3_End"))
  curr_actual_ICU_time <- curr_time_df[,curr_actual_ICU_time_idxes]
  curr_last_ICU_time <- max(ymd_hms(curr_actual_ICU_time),na.rm = T)
  
  #Get curr scr in ICU D0-D3, could be less than 3 days
  curr_scr_inICUD0D3 <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,curr_last_ICU_time,"SCR_ENTERED")
  
  #1.Use scr in ICU D0-D3 to compute KDIGO
  if (nrow(curr_scr_inICUD0D3) > 0 & is.na(curr_baseline_scr) == F){
    #current KIDGO for all Scr in window
    curr_SCR_KDIGO_df    <- get_KDIGO_Score_forScrdf_func(curr_baseline_scr,curr_scr_inICUD0D3)
  }
  
  # #update KDIGO for each time step for Scr if each time step is in RRT duration, update the KDIGO score to 4
  # if (is.na(curr_crrt_start) == F | is.na(curr_hd_start) == F){
  #   curr_updated_KDIGO_df <- update_KDIGO_df_forRRT_func(curr_SCR_KDIGO_df,curr_crrt_start,curr_crrt_end,curr_hd_start,curr_hd_end,48)
  # }else{
  #   curr_updated_KDIGO_df <-  curr_SCR_KDIGO_df #no updates
  # }
  
  #2.get idxes of Scr dates is within RRT start and RRT end +  48 hours
  if (is.na(curr_crrt_start)== F ){
    idxes_inCRRT <- get_DateIndxes_inInterval(curr_SCR_KDIGO_df[,"Scr_Time"],curr_crrt_start,curr_crrt_end + hours(48))
  }else{
    idxes_inCRRT <- NULL
  }
  
  if (is.na(curr_hd_start)== F){
    idxes_inHD <- get_DateIndxes_inInterval(curr_SCR_KDIGO_df[,"Scr_Time"],curr_hd_start,curr_hd_end + hours(48))
  }else{
    idxes_inHD <- NULL
  }
  comb_idxes <- unique(c(idxes_inCRRT,idxes_inHD))
  
  #3. exclude the Scr KDIGO_Df with the dates on RRT, becuase this dates will be covered in RRT_KDIGO_df
  if (length(comb_idxes) > 0 ){
    curr_SCR_KDIGO_df_filtered <- curr_SCR_KDIGO_df[-comb_idxes,]
    colnames(curr_SCR_KDIGO_df_filtered)[1] <- "Time"
  }else{
    curr_SCR_KDIGO_df_filtered <- NULL
  }
  
  
  #4.Use RRT time to check if KDIGO-3D in ICU D0-D3 
  crrt_kdigo_df <- get_RRT_KDIGO_df_ICUD0D3(curr_crrt_start,curr_crrt_end,48,curr_icu_start,curr_last_ICU_time)
  crrt_kdigo_time_steps <-  unique(crrt_kidgo_df[,"Time"])
  
  hd_kdigo_df <- get_RRT_KDIGO_df_ICUD0D3(curr_hd_start,curr_hd_end,48,curr_icu_start,curr_last_ICU_time)
  hd_kdigo_time_steps <-  unique(hd_kdigo_df[,"Time"])
  
  #5.Combine all available time steps which has KDIGO score
  Comb_KDIGO_df <- rbind(curr_SCR_KDIGO_df_filtered,crrt_kdigo_df,hd_kdigo_df)
  
  last_time_point  <- max(ymd_hms(Comb_KDIGO_df[,"Time"]))
  curr_time_cloest_toICUadmit <- min(ymd_hms(Comb_KDIGO_df[,"Time"]))

  KDIGO_df[i,"MAX_KDIGO_ICU_D0toD3"]   <- max(Comb_KDIGO_df[,"KDIGO"])
  KDIGO_df[i,"LAST_KDIGO_ICU_D0toD3"]  <- Comb_KDIGO_df[which(Comb_KDIGO_df[,"Scr_Time"] == last_time_point),"KDIGO"]
  KDIGO_df[i,"Admit_KDIGO_ICU"] <- Comb_KDIGO_df[which(Comb_KDIGO_df[,"Scr_Time"] == curr_time_cloest_toICUadmit),"KDIGO"]
  
}

write.csv(KDIGO_df,paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"))

########################################################################################
# Baseline EGFR
##########################################################################################
#1.Load Baseline SCR df
Final_SCR_df <- read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)

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
    Baseline_EGFR_df[p,"Baseline_eGFR"] <- EPI_equation(curr_baseline_scr,curr_age,curr_gender,curr_race)
  }
}
write.csv(Baseline_EGFR_df,paste0(outdir,"Baseline_EGFR.csv"),row.names=FALSE)


########################################################################################
##### Compute EGFR at (HOSP DC to 120 days)
##### 1. return Scr used for computation (All Scr values which within 30 days to the cloest value, and cloest value)
##### 2. EGFR is using the median of all Scr in 1.
########################################################################################
#Get all Outpatient Scr df
All_OutptSCr_df <- raw_SCR_df[which(grepl("Outpatient",raw_SCR_df[,"SCR_ENCOUNTER_TYPE"]) == T),]

#Compute EGFR at hosp120 days
time_window_start <- days(120) #120 days after HOSP discharge
time_window_expansion <- days(30) #30 days before (the cloest record time to 120 days )
res <- compute_EGFR_inWindow_func2(time_window_start,time_window_expansion,analysis_ID,All_time_df,All_RACE_GENDER_df,All_OutptSCr_df)
EGFR_120_df <- res[[1]]
write.csv(EGFR_120_df,paste0(outdir,"EGFR_120.csv"),row.names=FALSE)
ScrUsed_120_df <- res[[2]]
write.csv(ScrUsed_120_df,paste0(outdir,"EGFR_ScrUsed_120.csv"),row.names=FALSE) #All Scr in Time Window less than 30 days apar

length(which(is.na(EGFR_120_df$EGFR_120d)==T))
length(which(EGFR_120_df$n_OutptScr_AfterHOSP_Before120d==0)) ##28135

########################################################################################
##           5. EGFR drop > 50% and 30% of baseline HOSP_DC to 120 days
########################################################################################
#1.Load data
eGFR_120_df <- read.csv(paste0(outdir,"EGFR_120.csv"), stringsAsFactors = F)
bl_eGFR_df <- read.csv(paste0(outdir,"Baseline_EGFR.csv"), stringsAsFactors = F)

#2.Compute EGFR drop
EGFR_Drop_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(EGFR_Drop_df) <- c("STUDY_PATIENT_ID","eGFR_Drop50","eGFR_Drop30")

for (p in 1:length(analysis_ID)){
  if (p %% 1000 == 0){print(p)}
  curr_id <- analysis_ID[p]
  EGFR_Drop_df[p,"STUDY_PATIENT_ID"] <- curr_id
  
  curr_bl_egfr <- bl_eGFR_df[which(bl_eGFR_df[,"STUDY_PATIENT_ID"]==curr_id),"Baseline_eGFR"]
  curr_tw_egfr <- eGFR_120_df[which(eGFR_120_df[,"STUDY_PATIENT_ID"]==curr_id),"EGFR_120d"]

  if (is.na(curr_bl_egfr) == F & is.na(curr_tw_egfr) == F){
    
    #Drop >=50%
    if ((curr_bl_egfr - curr_tw_egfr) >= curr_bl_egfr*0.5){
      EGFR_Drop_df[p,"eGFR_Drop50"] <- 1
    }else{
      EGFR_Drop_df[p,"eGFR_Drop50"] <- 0
    }
    #Drop >=30%
    if ( (curr_bl_egfr - curr_tw_egfr) >= curr_bl_egfr*0.3){ #curr_bl_egfr - curr_tw_egfr>= curr_bl_egfr*0.3
      EGFR_Drop_df[p,"eGFR_Drop30"] <- 1
    }else{
      EGFR_Drop_df[p,"eGFR_Drop30"] <- 0
    }
    
  }else{
    EGFR_Drop_df[p,"eGFR_Drop50"] <- NA
    EGFR_Drop_df[p,"eGFR_Drop30"] <- NA
    
  }
  
}

table(EGFR_Drop_df$eGFR_Drop50) #7422  460 
table(EGFR_Drop_df$eGFR_Drop30) #6842 1040 
length(which(is.na(EGFR_Drop_df$eGFR_Drop50)==T)) #28135

write.csv(EGFR_Drop_df,paste0(outdir,"EGFR_Drop_120_df.csv"),row.names=FALSE)
