library(lubridate)
library(data.table)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#2. Corrected Time df
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#3.Load SCR
raw_SCR_df <- read.csv(paste0(raw_dir,"all_scr_data.csv"),stringsAsFactors = F)
#change col name
cols_name_tochange <- which(colnames(raw_SCR_df) %in% c("PATIENT_NUM","RESULT_TIME","ORD_VALUE"))
colnames(raw_SCR_df)[cols_name_tochange] <- c("STUDY_PATIENT_ID","SCR_ENTERED","SCR_VALUE")
##remove duplicated entry
#raw_SCR_df <- raw_SCR_df[!duplicated(raw_SCR_df[,c("STUDY_PATIENT_ID","SCR_ENTERED")]),] #remove duplicated entry

#3. Load demo for resolve EPI
All_RACE_GENDER_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

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
Final_SCR_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 9))
colnames(Final_SCR_df) <- c("STUDY_PATIENT_ID","Baseline_SCr","AdmitICU_SCr","AdmitICU_SCr_TIME",
                            "Peak_SCr_inICU_D0_D3","NUM_SCr_inICU_D0_D3","Lowest_SCr_inICU_D0_D3",
                            "LastSCr_inICU_D0_D3","LastSCr_inICU_D0_D3_TIME")
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
  curr_bl_scr <- get_baseline_scr_func(curr_hosp_start,curr_scr_df,"IP_FLAG")
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
    Final_SCR_df[i,"AdmitICU_SCr"] <- mean(curr_admit_scr,na.rm = T) #if multiple at the same time, take the mean
    Final_SCR_df[i,"AdmitICU_SCr_TIME"] <- unique(curr_admit_scr_time)
    
    #last Scr
    curr_last_scr_afterICU_idx <- which(curr_scr_inICUD0D3[,"SCR_ENTERED"] == max(curr_scr_inICUD0D3[,"SCR_ENTERED"]))
    curr_last_scr <- curr_scr_inICUD0D3[curr_last_scr_afterICU_idx,"SCR_VALUE"]
    curr_last_scr_time <- curr_scr_inICUD0D3[curr_last_scr_afterICU_idx,"SCR_ENTERED"]
    Final_SCR_df[i,"LastSCr_inICU_D0_D3"] <- mean(curr_last_scr,na.rm = T) #if multiple at the same time, take the mean
    Final_SCR_df[i,"LastSCr_inICU_D0_D3_TIME"] <- unique(curr_last_scr_time)
    
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

#'@TODO
#'@ADDED 080821
length(no_bl_scr_IDs) ##N of Resolved baseline by EPI: 5212
no_bl_scr_IDs_df <- as.data.frame(no_bl_scr_IDs)
write.csv(no_bl_scr_IDs_df,paste0(outdir,"NO_Measured_BaselineScr_IDs.csv"),row.names = F)


write.csv(Final_SCR_df,paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df_AddedLastScr.csv"),row.names = F)


##########################################################################################
#KDIGO
#1.Admit KDIGO
#2.Maximum KDIGO 	(Maximum KDIGO score in ICU D0 to D3)
#3.Last    KDIGO  (Last KDIGO score    in ICU D0 to D3)
##########################################################################################
KDIGO_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 6))
colnames(KDIGO_df) <- c("STUDY_PATIENT_ID","Admit_KDIGO_ICU","MAX_KDIGO_ICU_D0toD3","LAST_KDIGO_ICU_D0toD3","All_Unique_KDIGO_SCORE_D0toD3","KDIGO_4")
for (i in  1:length(analysis_ID)){
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
    colnames(curr_SCR_KDIGO_df)[1] <- "Time"
    
    #make sure time has the correct format for converting later
    time_indxes <- which(nchar(curr_SCR_KDIGO_df$Time) == 10)
    if (length(time_indxes) > 0){
      curr_SCR_KDIGO_df[time_indxes,"Time"] <- paste(curr_SCR_KDIGO_df[time_indxes,"Time"],"00:00:00")
    }
    
  }else {
    curr_SCR_KDIGO_df <- NULL
  }
  
  #2. Use RRT time to check if KDIGO-3D in ICU D0-D3 
  if (is.na(curr_crrt_start)== F | is.na(curr_hd_start)== F) { #if ever on CRRT or HD
    if (is.na(curr_crrt_start)== F){
      #1. Get CRRT KDIGO df
      crrt_kdigo_df <- get_RRT_KDIGO_df_ICUD0D3(curr_crrt_start,curr_crrt_end,48,curr_icu_start,curr_last_ICU_time)
      #2. get idxes of Scr dates is within RRT start and RRT end +  48 hours, these idxes will be excluded later
      SCR_idxes_inCRRT <- get_DateIndxes_inInterval(curr_SCR_KDIGO_df[,"Time"],curr_crrt_start,curr_crrt_end + hours(48))
    }else{
      crrt_kdigo_df <- NULL
      SCR_idxes_inCRRT <- NULL
    }
    
    if (is.na(curr_hd_start)== F){
      #1. Get HD KDIGO df
      hd_kdigo_df <- get_RRT_KDIGO_df_ICUD0D3(curr_hd_start,curr_hd_end,48,curr_icu_start,curr_last_ICU_time)
      #2. get idxes of Scr dates is within RRT start and RRT end +  48 hours, these idxes will be excluded later
      SCR_idxes_inHD <- get_DateIndxes_inInterval(curr_SCR_KDIGO_df[,"Time"],curr_hd_start,curr_hd_end + hours(48))
    }else{
      hd_kdigo_df <- NULL
      SCR_idxes_inHD <- NULL
    }
    comb_idxes <- unique(c(SCR_idxes_inCRRT,SCR_idxes_inHD))
    
  }else{
    crrt_kdigo_df <- NULL
    hd_kdigo_df <- NULL
    comb_idxes <- NULL #if never on CRRT or HD
  }
  
  #2.2 Another way for RRT KDIGO check (this should be the same as 2.)
  if (is.na(curr_crrt_start)== F){
     if ( curr_crrt_start <= curr_last_ICU_time & (curr_crrt_end + hours(48)) >= curr_icu_start ){
       curr_kdigo_crrt <- 1
     }else{
       curr_kdigo_crrt <- 0
     }
    
  }else{
      curr_kdigo_crrt <- 0
  }
  
  #2.2 
  if (is.na(curr_hd_start)== F){
    if ( curr_hd_start <= curr_last_ICU_time & (curr_hd_end + hours(48)) >= curr_icu_start ){
      curr_kdigo_hd <- 1
    }else{
      curr_kdigo_hd <- 0
    }
  }else{
    curr_kdigo_hd <- 0
  }
  
  KDIGO_df[i,"KDIGO_4"] <- max(curr_kdigo_crrt,curr_kdigo_hd,na.rm = T)
    
  #combine and record current SCR KDIGO and RRT KDIGO score
  KDIGO_df[i,"All_Unique_KDIGO_SCORE_D0toD3"] <- paste0(c(sort(unique(curr_SCR_KDIGO_df[,"KDIGO"])),unique(crrt_kdigo_df[,"KDIGO"]),unique(hd_kdigo_df[,"KDIGO"])),collapse = "$$$")
  
  
  #3. exclude the Scr_KDIGO_Df with the dates on RRT if any, becuase this dates will be covered in RRT_KDIGO_df
  if (length(comb_idxes) > 0 ){
    curr_SCR_KDIGO_df_filtered <- curr_SCR_KDIGO_df[-comb_idxes,]
  }else{ #do not exclude anything
    curr_SCR_KDIGO_df_filtered <- curr_SCR_KDIGO_df
  }
  

  #4.Combine KDIGO from Scr and CRRT and HD (If any one of them is ampty is fine, just add an empty)
  Comb_KDIGO_df <- rbind(curr_SCR_KDIGO_df_filtered,crrt_kdigo_df,hd_kdigo_df)
  
  #make sure time has the correct format for converting later
  time_indxes <- which(nchar(Comb_KDIGO_df$Time) == 10)
  if (length(time_indxes) > 0){
   Comb_KDIGO_df[time_indxes,"Time"] <- paste(Comb_KDIGO_df[time_indxes,"Time"],"00:00:00")
  }
  
  if (is.null(Comb_KDIGO_df)== F){
    #Remove duplicated time step
    #this duplicaed time point is due to the extension of 48 hours of CRRT and HD, so that these two may have dupicated time, if the gap between these two is e.g, 24 hours
    dup_idxes <- which(duplicated(Comb_KDIGO_df$Time)==T)
    if (length(dup_idxes)>0){
      Comb_KDIGO_df <- Comb_KDIGO_df[-dup_idxes,] 
    }
    
    last_time_point  <- max(ymd_hms(Comb_KDIGO_df[,"Time"]))
    curr_time_cloest_toICUadmit <- min(ymd_hms(Comb_KDIGO_df[,"Time"]))
    
    KDIGO_df[i,"MAX_KDIGO_ICU_D0toD3"]   <- max(Comb_KDIGO_df[,"KDIGO"])
    KDIGO_df[i,"LAST_KDIGO_ICU_D0toD3"]  <- Comb_KDIGO_df[which(ymd_hms(Comb_KDIGO_df[,"Time"]) == last_time_point),"KDIGO"]
    KDIGO_df[i,"Admit_KDIGO_ICU"] <- Comb_KDIGO_df[which(ymd_hms(Comb_KDIGO_df[,"Time"]) == curr_time_cloest_toICUadmit),"KDIGO"]
  }
}

#the following two should be the same, the 2nd one using every 5min for RRT_KDIGO
#write.csv(KDIGO_df,paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"))
write.csv(KDIGO_df,paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df_011522.csv"))

#check
KDIGO_df_old <- read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)
KDIGO_df_new <- read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df_011522.csv"),stringsAsFactors = F)

KDIGO_df_old[which(is.na(KDIGO_df_old)==T,arr.ind = T)] <- 0
KDIGO_df_new[which(is.na(KDIGO_df_new)==T,arr.ind = T)] <- 0
identical(KDIGO_df_old,KDIGO_df_new)

final_id <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
check_df <- KDIGO_df_new[which(KDIGO_df_new$STUDY_PATIENT_ID %in% final_id$STUDY_PATIENT_ID),]

table(check_df$MAX_KDIGO_ICU_D0toD3)

##########################################################################################
#'@Added 011322
#'KDIGO -D0 to D7
#1.Maximum KDIGO 	(Maximum KDIGO score in ICU D0 to D7)
##########################################################################################
Final_SCR_df <- read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df_AddedLastScr.csv"),stringsAsFactors = F)
All_time_df <- read.csv(paste0(outdir,"All_Corrected_Timeinfo_ADD_D4toD7.csv"),stringsAsFactors = F)

KDIGO_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 6))
colnames(KDIGO_df) <- c("STUDY_PATIENT_ID",
                        "MAX_KDIGO_ICU_D0toD3",
                        "MAX_KDIGO_ICU_D0toD7",
                        "MAX_KDIGO_ICU_D0toD1",
                        "MAX_KDIGO_ICU_D2toD3",
                        "MAX_KDIGO_ICU_D4toD7")
for (i in  1:length(analysis_ID)){
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
  
  #For ID = 2399381, has duplicated time point which cause difference between current algorithm and this algorithm
  #To keep consistence, drop the 2nd duplicates for this pts.
  #'@NOTE: The algorithms works the same, except for this pts because of duplicating drops
  if (curr_id == 2399381){
     curr_scr_df <- curr_scr_df[duplicated(curr_scr_df$SCR_ENTERED)==F,] #drop duplicated time point
  }
  
  #Get actual days/times in ICU D0-D7
  curr_actual_ICU_time_idxes <- which(colnames(curr_time_df) %in% 
                                        c("Actual_D0_End","Actual_D1_End",
                                          "Actual_D2_End","Actual_D3_End",
                                          "Actual_D4_End","Actual_D5_End",
                                          "Actual_D6_End","Actual_D7_End"))
  curr_actual_ICU_time <- curr_time_df[,curr_actual_ICU_time_idxes]
  curr_last_ICU_time <- max(ymd_hms(curr_actual_ICU_time),na.rm = T)
  
  #Get curr scr in ICU D0-D7, could be less than 7 days
  curr_scr_inICUD0D7 <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,curr_last_ICU_time,"SCR_ENTERED")
  
  #1.Use scr in ICU D0-D7 to compute KDIGO
  if (nrow(curr_scr_inICUD0D7) > 0 & is.na(curr_baseline_scr) == F){
    #current KIDGO for all Scr in window
    curr_SCR_KDIGO_df    <- get_KDIGO_Score_forScrdf_func(curr_baseline_scr,curr_scr_inICUD0D7)
    colnames(curr_SCR_KDIGO_df)[1] <- "Time"
  }else {
    curr_SCR_KDIGO_df <- NULL
  }
  
  #reformat time
  ref_idxes <- which(nchar(curr_SCR_KDIGO_df[,"Time"]) !=19)
  if (length(ref_idxes) > 0){
    curr_SCR_KDIGO_df[ref_idxes,"Time"] <- paste(curr_SCR_KDIGO_df[ref_idxes,"Time"],"00:00:00")
  }
  
  #2.Get MAX KDIGO score per day ICU D0 - D7
  day_start_cols <- paste0("Actual_D",seq(0,7),"_Start") 
  day_end_cols   <- paste0("Actual_D",seq(0,7),"_End")
  
  KDIGO_perDay_df <- as.data.frame(matrix(NA, nrow = 8, ncol = 4))
  colnames(KDIGO_perDay_df) <- c("ICU_DAY","Actual_Start","Actual_End","MAX_KDIGO")
  for (t in 1:8){
    curr_d_start <- ymd_hms(curr_time_df[,day_start_cols[t]])
    curr_d_end   <- ymd_hms(curr_time_df[,day_end_cols[t]])
    
    if (is.na(curr_d_start) == F){ #if patient is in ICU in current day
          #1. get KDIGO score in current day using sCr
          curr_kdigo_using_sCr <- get_maxKDIGO_usingScr_inOneDay(curr_SCR_KDIGO_df,curr_d_start,curr_d_end)
          
          #2. check if current day on RRT (with 48 hours extenstion), if so, KDIGO score = 4
          if (is.na(curr_crrt_start)== F){
            curr_on_crrt <- check_overlapp_manually_func(curr_crrt_start,curr_crrt_end + hours(48),curr_d_start,curr_d_end)
          }else{
            curr_on_crrt <- 0
          }
          if (is.na(curr_hd_start)== F){
            curr_on_hd <- check_overlapp_manually_func(curr_hd_start,curr_hd_end + hours(48),curr_d_start,curr_d_end)
          }else{
            curr_on_hd <- 0
          }
           
          #if on RRT, then kdigo score = 4
          if (is.na(curr_on_crrt) == F & curr_on_crrt!=0){
            curr_kidgo_score <- 4
          }else if (is.na(curr_on_hd) == F & curr_on_hd!=0){
            curr_kidgo_score <- 4
          }else if (is.na(curr_kdigo_using_sCr) == F){ #if sCr is avaialble 
            curr_kidgo_score <- curr_kdigo_using_sCr
          }else{ #if not on RRT, no Scr avaiable
            curr_kidgo_score <- NA
          }
 
        KDIGO_perDay_df[t,"ICU_DAY"]      <- paste0("D",t-1)
        KDIGO_perDay_df[t,"Actual_Start"] <- as.character(curr_d_start)
        KDIGO_perDay_df[t,"Actual_End"]   <- as.character(curr_d_end)
        KDIGO_perDay_df[t,"MAX_KDIGO"]    <- curr_kidgo_score
    }else {
      KDIGO_perDay_df[t,"ICU_DAY"]      <- paste0("D",t-1)
      KDIGO_perDay_df[t,"Actual_Start"] <- as.character(curr_d_start)
      KDIGO_perDay_df[t,"Actual_End"]   <- as.character(curr_d_end)
      KDIGO_perDay_df[t,"MAX_KDIGO"]    <- NA
    }
    
  }
  
  #3.Find max KDIGO D0-D1, D2-D3, D4-D7, D0-D3, D0-D7
  max_KDIGO_D0D1 <- get_MAX_KDIGO_inMultiple_days(KDIGO_perDay_df, c("D0","D1"))
  max_KDIGO_D2D3 <- get_MAX_KDIGO_inMultiple_days(KDIGO_perDay_df, c("D2","D3"))
  max_KDIGO_D4D7 <- get_MAX_KDIGO_inMultiple_days(KDIGO_perDay_df, c("D4","D5","D6","D7"))
  
  #4.Double check with previous results
  max_KDIGO_D0D3 <- get_MAX_KDIGO_inMultiple_days(KDIGO_perDay_df, c("D0","D1","D2","D3"))
  max_KDIGO_D0D7 <- get_MAX_KDIGO_inMultiple_days(KDIGO_perDay_df, c("D0","D1","D2","D3","D4","D5","D6","D7"))
  

  KDIGO_df[i,"MAX_KDIGO_ICU_D0toD3"]   <- max_KDIGO_D0D3
  KDIGO_df[i,"MAX_KDIGO_ICU_D0toD7"]   <- max_KDIGO_D0D7
  KDIGO_df[i,"MAX_KDIGO_ICU_D0toD1"]   <- max_KDIGO_D0D1
  KDIGO_df[i,"MAX_KDIGO_ICU_D2toD3"]   <- max_KDIGO_D2D3
  KDIGO_df[i,"MAX_KDIGO_ICU_D4toD7"]   <- max_KDIGO_D4D7
  

}

write.csv(KDIGO_df,paste0(outdir,"KDIGO_MAX_ICU_D0D7_df.csv"))

#onset AKI numbers
#2322
AKI_onset_d0d1 <- length(which(KDIGO_df$MAX_KDIGO_ICU_D0toD1 %in% c(1,2,3,4)))
#435
AKI_onset_d2d3 <- length(which((is.na(KDIGO_df$MAX_KDIGO_ICU_D0toD1)==T |
                                  KDIGO_df$MAX_KDIGO_ICU_D0toD1 == 0) &
                                 KDIGO_df$MAX_KDIGO_ICU_D2toD3 %in% c(1,2,3,4)))
#215
AKI_onset_d4d7 <- length(which((is.na(KDIGO_df$MAX_KDIGO_ICU_D0toD1)==T | KDIGO_df$MAX_KDIGO_ICU_D0toD1 == 0) &
                                 (is.na(KDIGO_df$MAX_KDIGO_ICU_D2toD3)==T | KDIGO_df$MAX_KDIGO_ICU_D2toD3 == 0) &
                                 KDIGO_df$MAX_KDIGO_ICU_D4toD7 %in% c(1,2,3,4)))


#Compare with previous version
KDIGO_df_old <- read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df_011522.csv"),stringsAsFactors = F)
KDIGO_df_new <- read.csv(paste0(outdir,"KDIGO_MAX_ICU_D0D7_df.csv"),stringsAsFactors = F)

KDIGO_df_old[which(is.na(KDIGO_df_old)==T,arr.ind = T)] <- 0
KDIGO_df_new[which(is.na(KDIGO_df_new)==T,arr.ind = T)] <- 0

identical(KDIGO_df_old$MAX_KDIGO_ICU_D0toD3,KDIGO_df_new$MAX_KDIGO_ICU_D0toD3)

table(KDIGO_df_old$MAX_KDIGO_ICU_D0toD3)
table(KDIGO_df_new$MAX_KDIGO_ICU_D0toD3)

####################################################################################
#'@Updated 021122, compute the AKI incidence in entire ICU
####################################################################################
Final_SCR_df <- read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df_AddedLastScr.csv"),stringsAsFactors = F)
All_time_df <- read.csv(paste0(outdir,"All_Corrected_Timeinfo_ADD_D4toD7.csv"),stringsAsFactors = F)

KDIGO_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(KDIGO_df) <- c("STUDY_PATIENT_ID","MAX_KDIGO_ICU_AllTime")
for (i in  1:length(analysis_ID)){
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
  #curr_scr_df <- curr_scr_df[duplicated(curr_scr_df$SCR_ENTERED)==F,] #drop duplicated time point
  
  #Get curr scr in ICU 
  curr_scr_inICU <- get_value_df_inWindow_func(curr_scr_df,curr_icu_start,curr_icu_end,"SCR_ENTERED")
  
  #1.Use scr in ICU to compute KDIGO
  if (nrow(curr_scr_inICU) > 0 & is.na(curr_baseline_scr) == F){
    #current KIDGO for all Scr in window
    curr_SCR_KDIGO_df    <- get_KDIGO_Score_forScrdf_func(curr_baseline_scr,curr_scr_inICU)
    colnames(curr_SCR_KDIGO_df)[1] <- "Time"
    curr_kdigo_using_sCr <-  max(curr_SCR_KDIGO_df["KDIGO"],na.rm = T)
  }else {
    curr_SCR_KDIGO_df <- NULL
    curr_kdigo_using_sCr <-  0
  }
  
  #2. check if on RRT (with 48 hours extenstion), if so, KDIGO score = 4
  if (is.na(curr_crrt_start)== F){
    curr_on_crrt <- check_overlapp_manually_func(curr_crrt_start,curr_crrt_end + hours(48),curr_icu_start,curr_icu_end)
  }else{
    curr_on_crrt <- 0
  }
  if (is.na(curr_hd_start)== F){
    curr_on_hd <- check_overlapp_manually_func(curr_hd_start,curr_hd_end + hours(48),curr_icu_start,curr_icu_end)
  }else{
    curr_on_hd <- 0
  }
  
  #if on RRT, then kdigo score = 4, otherwise use sCr kdigo score, otherwise NA
  if (is.na(curr_on_crrt) == F & curr_on_crrt!=0){
    curr_kidgo_score <- 4
  }else if (is.na(curr_on_hd) == F & curr_on_hd!=0){
    curr_kidgo_score <- 4
  }else if (is.na(curr_kdigo_using_sCr) == F){ #if sCr is avaialble 
    curr_kidgo_score <- curr_kdigo_using_sCr
  }else{ #if not on RRT, no Scr avaiable
    curr_kidgo_score <- NA
  }
  
  KDIGO_df[i,"MAX_KDIGO_ICU_AllTime"]   <- curr_kidgo_score
  
}

#Get how many patient have AKI in ICU duration
#  0    1    2    3    4 
#7354 1533  546  489  580
table(KDIGO_df$MAX_KDIGO_ICU_AllTime) 

#output max KDIGO in ICU entire duration
write.csv(KDIGO_df,paste0(outdir,"KDIGO_MAX_ICU_AllTime_df.csv"))

########################################################################################
#'@NOTE_CHECK: Check all on RRT inICU_D0_D3 must has KDIGO=4, not on RRT IDs must not have KDIGO=4
#'#NOTE: All patient on RRT in ICU_D0_D3 have KDIGO = 4
#      one patient (ID  47832) has KDIGO = 4 , but not acutally on RRT in ICU D0_D3, becuase the 48 hours extension effect for KDIGO
#      e.g, HD end 2009-09-19,  2009-09-19 is not in ICU D0_D3, but 2009-09-19+  48 hours is in ICU_D0_D3, so KDIGO score is 4
#'########################################################################################
# onRRT_df <- read.csv(paste0(outdir,"All_onRRT_ICUD0toD3.csv"),stringsAsFactors = F)
# KDIGO_df_analysis_ID <- KDIGO_df[which(KDIGO_df$STUDY_PATIENT_ID %in% onRRT_df$STUDY_PATIENT_ID),]
# 
# onRRT_D0D3_IDs <- onRRT_df$STUDY_PATIENT_ID[which(onRRT_df$onRRT_ICUD0toD3 %in% c(1,2))]
# KDIGO4_IDs <- KDIGO_df_analysis_ID[which(KDIGO_df_analysis_ID$MAX_KDIGO_ICU_D0toD3==4),"STUDY_PATIENT_ID"]
# 
# #check on RRT but not KDIGO= 4 (none)
# which(!onRRT_D0D3_IDs %in% KDIGO4_IDs)
# #check  KDIGO= 4 but not on RRT, because the 48 hours extension for KDIGO score
# which(!KDIGO4_IDs %in% onRRT_D0D3_IDs)

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
All_OutptSCr_df <- raw_SCR_df[which(grepl("Outpatient",raw_SCR_df[,"IP_FLAG"],ignore.case = T) == T),]

#Compute EGFR at hosp120 days
time_window_start <- days(120) #120 days after HOSP discharge
time_window_expansion <- days(30) #30 days before (the cloest record time to 120 days )
res <- compute_EGFR_inWindow_func2(time_window_start,time_window_expansion,analysis_ID,All_time_df,All_RACE_GENDER_df,All_OutptSCr_df)
EGFR_120_df <- res[[1]]
write.csv(EGFR_120_df,paste0(outdir,"EGFR_120.csv"),row.names=FALSE)
ScrUsed_120_df <- res[[2]]
write.csv(ScrUsed_120_df,paste0(outdir,"EGFR_ScrUsed_120.csv"),row.names=FALSE) #All Scr in Time Window less than 30 days apar

length(which(is.na(EGFR_120_df$EGFR_120d)==T))
length(which(EGFR_120_df$n_OutptScr_AfterHOSP_Before120d==0)) ##  6331

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

table(EGFR_Drop_df$eGFR_Drop50) #4010  161
table(EGFR_Drop_df$eGFR_Drop30) #3648  523
length(which(is.na(EGFR_Drop_df$eGFR_Drop50)==T)) # 6331

write.csv(EGFR_Drop_df,paste0(outdir,"EGFR_Drop_120_df.csv"),row.names=FALSE)
