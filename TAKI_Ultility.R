library(pROC)
library(caret)
library(xgboost)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(caTools)
##############  Time correction functions ############## 
reformat_10char_dates_func <- function(date_to_modify){
  date_to_modify <- as.character(date_to_modify)
  if (nchar(date_to_modify) == 10){
    updated_date <- paste(date_to_modify, "00:00:00")
  }else{
    updated_date <- as.character(date_to_modify)
  }
  return(updated_date)
}

check_NA_dates_func <- function(dates_tocheck){
  if (is.na(dates_tocheck) ==F){
    HAS_Dates <- 1
  }else {
    HAS_Dates <- 0
  }
  return(HAS_Dates)
}

#Check if time duration 1 is within time duration 2  with extend hours 
check_T1_in_T2<-function(T1_Start,T1_End,T2_Start,T2_End,event_name1,event_name2,hours_to_extend){
  if(ymd_hms(T1_Start) >= (ymd_hms(T2_Start) - hours(hours_to_extend)) &  
     ymd_hms(T1_End)   <= (ymd_hms(T2_End)   + hours(hours_to_extend))){
    t1_in_t2_flag<-paste(event_name1,"in",event_name2)
  }else{
    t1_in_t2_flag<-paste(event_name1,"not in",event_name2)
  }
  return(t1_in_t2_flag)
}

#check if two event has any overlapped time
check_overlapTime_betweenTwoEvents <- function(T1_Start,T1_End,T2_Start,T2_End,event_name1,event_name2){
  intCRRT<-interval(ymd_hms(T1_Start),ymd_hms(T1_End))
  intHD<-interval(ymd_hms(T2_Start),ymd_hms(T2_End))
  
  if(int_overlaps(intCRRT, intHD)==T){
    overlap_flag <- paste(event_name1,"and", event_name2, "overlap") 
    
  }else{
    overlap_flag<- paste(event_name1,"and", event_name2, "not overlapped") 
  }
  
  
  return(overlap_flag)
  
}

#Check if pateints has dates
check_has_dates <- function(Time_df){
  IDs_toCheck <- Time_df[,"STUDY_PATIENT_ID"]
  
  has_dates_df <- as.data.frame(matrix(NA, nrow = length(IDs_toCheck) ,ncol = 5))
  colnames(has_dates_df) <- c("STUDY_PATIENT_ID", "HAS_CRRT","HAS_HD","HAS_ICU","HAS_HOSP")
  
  for (p in 1:length(IDs_toCheck)){
    if(p %% 1000==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", p, "\n"))
    }
    curr_id <- IDs_toCheck[p]
    curr_time_df  <- Time_df[which(Time_df[,"STUDY_PATIENT_ID"] == curr_id),]
    
    CRRT_Start <- curr_time_df[ ,"CRRT_START_DATE"]
    CRRT_End <- curr_time_df[ ,"CRRT_STOP_DATE"]
    HD_Start <- curr_time_df[ ,"HD_START_DATE"]
    HD_End <- curr_time_df[ ,"HD_STOP_DATE"]
    ICU_Start <- curr_time_df[ ,"First_ICU_ADMIT_DATE"]
    ICU_End <- curr_time_df[ ,"First_ICU_DISCHARGE_DATE"]
    
    HOSP_Start <- curr_time_df[,"HOSP_ADMIT_DATE"]
    HOSP_End <- curr_time_df[,"HOSP_DISCHARGE_DATE"]
    
    #check if pts has these dates
    has_dates_df[p, "STUDY_PATIENT_ID"] <- curr_id
    has_dates_df[p, "HAS_CRRT"] <- check_NA_dates_func(CRRT_Start)
    has_dates_df[p, "HAS_HD"] <- check_NA_dates_func(HD_Start)
    has_dates_df[p, "HAS_ICU"] <- check_NA_dates_func(ICU_Start)
    has_dates_df[p, "HAS_HOSP"] <- check_NA_dates_func(HOSP_Start)
  }
  return(has_dates_df)
}

#check if pateint has valid dates
check_valid_dates <- function(Time_df,hours_toextend){
  IDs_toCheck <- Time_df[,"STUDY_PATIENT_ID"]
  
  valids_dates_df <- as.data.frame(matrix(NA, nrow = length(IDs_toCheck) ,ncol = 6))
  colnames(valids_dates_df) <- c("STUDY_PATIENT_ID", "HD_inHOSP","CRRT_inHOSP","ICU_inHOSP","CRRT_inICU","CRRT_HD_Overlap")
  
  for (p in 1:length(IDs_toCheck)){
    if(p %% 1000==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", p, "\n"))
    }
    curr_id <- IDs_toCheck[p]
    valids_dates_df[p, "STUDY_PATIENT_ID"] <- curr_id
    curr_time_df  <- Time_df[which(Time_df[,"STUDY_PATIENT_ID"] == curr_id),]
    
    CRRT_Start <- curr_time_df[ ,"CRRT_START_DATE"]
    CRRT_End <- curr_time_df[ ,"CRRT_STOP_DATE"]
    HD_Start <- curr_time_df[ ,"HD_START_DATE"]
    HD_End <- curr_time_df[ ,"HD_STOP_DATE"]
    ICU_Start <- curr_time_df[ ,"First_ICU_ADMIT_DATE"]
    ICU_End <- curr_time_df[ ,"First_ICU_DISCHARGE_DATE"]
    HOSP_Start <- curr_time_df[,"HOSP_ADMIT_DATE"]
    HOSP_End <- curr_time_df[,"HOSP_DISCHARGE_DATE"]
    
    #If HD in HOSP
    if (is.na(HD_Start) ==F & is.na(HOSP_Start)==F){
      valids_dates_df[p, "HD_inHOSP"] <- check_T1_in_T2(HD_Start,HD_End,HOSP_Start,HOSP_End,"HD","HOSP",hours_toextend)
    }else {
      valids_dates_df[p, "HD_inHOSP"] <- NA
    }
    #If CRRT in HOSP
    if (is.na(CRRT_Start) ==F & is.na(HOSP_Start)==F){
      valids_dates_df[p, "CRRT_inHOSP"] <- check_T1_in_T2(CRRT_Start,CRRT_End,HOSP_Start,HOSP_End,"CRRT","HOSP",hours_toextend)
    }else {
      valids_dates_df[p, "CRRT_inHOSP"] <- NA
    }
    #If ICU in HOSP
    if (is.na(ICU_Start) ==F & is.na(HOSP_Start)==F){
      valids_dates_df[p, "ICU_inHOSP"] <- check_T1_in_T2(ICU_Start,ICU_End,HOSP_Start,HOSP_End,"ICU","HOSP",hours_toextend)
    }else {
      valids_dates_df[p, "ICU_inHOSP"] <- NA
    }
    
    #If CRRT in ICU
    if (is.na(CRRT_Start) ==F & is.na(ICU_Start)==F){
      valids_dates_df[p, "CRRT_inICU"] <- check_T1_in_T2(CRRT_Start,CRRT_End,ICU_Start,ICU_End,"CRRT","ICU",hours_toextend)
    }else {
      valids_dates_df[p, "CRRT_inICU"] <- NA
    }
    
    #IF CRRT and HD overlap
    if (is.na(CRRT_Start) ==F & is.na(HD_Start)==F){
      valids_dates_df[p, "CRRT_HD_Overlap"] <- check_overlapTime_betweenTwoEvents(CRRT_Start,CRRT_End,HD_Start,HD_End,"CRRT","HD")
    }else {
      valids_dates_df[p, "CRRT_HD_Overlap"]
    }
    
  }
  
  return(valids_dates_df)
}

#correct CRRT and Hd overlappes, 
correct_overlap_CRRTHD <- function(CRRT_Start,CRRT_End,HD_Start,HD_End){
  # CRRT_Start <- curr_CRRT_Start
  # CRRT_End <- curr_CRRT_End
  # HD_Start <- curr_HD_Start
  # HD_End <- curr_HD_End
  
  #First check if ther is any  overlap 
  #Cuz there might no be overlap even if the following conditions are met 
  #for example : cond 4: with CRRT_START 12-10, CRRT_End 12-24, HD_START 12-28, HD_End 12-30
  overlap_flag <- check_overlapTime_betweenTwoEvents(CRRT_Start,CRRT_End,HD_Start,HD_End,"CRRT","HD")
  
  if (overlap_flag == "CRRT and HD overlap"){
    #convert to time objects
    CRRT_Start <- ymd_hms(CRRT_Start)
    CRRT_End <- ymd_hms(CRRT_End)
    HD_Start <- ymd_hms(HD_Start)
    HD_End <- ymd_hms(HD_End)
    
    #Compute event duration
    CRRT_duration<-CRRT_End-CRRT_Start
    HD_duration<-HD_End-HD_Start
    
    
    if (CRRT_Start == HD_Start & CRRT_End < HD_End){ #1. If CRRT Start = HD_Start and CRRT_End < HD_End
      status_flag <- "Cond1"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_End <- NA
        updated_CRRT_Start <- NA 
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- CRRT_End + days(1)
        updated_HD_End <- HD_End #no change
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start == HD_Start & CRRT_End == HD_End){#2. If CRRT Start = HD_Start and CRRT_End = HD_End
      status_flag <- "Cond2"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_End <- NA
        updated_CRRT_Start <- NA 
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- NA
        updated_HD_End <- NA
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start == HD_Start & CRRT_End > HD_End){#3. If CRRT Start = HD_Start and CRRT_End > HD_End
      status_flag <- "Cond3"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- HD_End +days(1)
        updated_CRRT_End <- CRRT_End #no change
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- NA
        updated_HD_End <- NA
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start < HD_Start & CRRT_End < HD_End){#4. If CRRT Start < HD_Start and CRRT_End < HD_End
      status_flag <- "Cond4"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- HD_Start - days(1) 
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- CRRT_End + days(1)
        updated_HD_End <- HD_End #no change
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start < HD_Start & CRRT_End == HD_End){#5. If CRRT Start < HD_Start and CRRT_End = HD_End
      status_flag <- "Cond5"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_End <- HD_Start - days(1) 
        updated_CRRT_Start <- CRRT_Start #no change
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- NA
        updated_HD_End <- NA
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start < HD_Start & CRRT_End > HD_End){#6. If CRRT Start < HD_Start and CRRT_End > HD_End
      status_flag <- "Cond6"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- HD_Start - days(1) 
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- NA
        updated_HD_End <- NA
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start > HD_Start & CRRT_End < HD_End){#7. If CRRT Start > HD_Start and CRRT_End < HD_End
      status_flag <- "Cond7"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- NA
        updated_CRRT_End <- NA
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- HD_Start
        updated_HD_End <- CRRT_Start - days(1)
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start > HD_Start & CRRT_End == HD_End){#8. If CRRT Start > HD_Start and CRRT_End = HD_End
      status_flag <- "Cond8"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- NA
        updated_CRRT_End <- NA
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- HD_Start
        updated_HD_End <- CRRT_Start - days(1)
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }else if (CRRT_Start > HD_Start & CRRT_End > HD_End){#9. If CRRT Start > HD_Start and CRRT_End > HD_End
      status_flag <- "Cond9"
      if (HD_duration > 1){ #HD dominates, remove overlap part of CRRT
        updated_CRRT_Start <- HD_End + days(1)
        updated_CRRT_End <- CRRT_End
        updated_HD_Start <- HD_Start #no change
        updated_HD_End <- HD_End #no change
      }else{ #CRRT dominates , remove overlap part of HD
        updated_HD_Start <- HD_Start
        updated_HD_End <- CRRT_Start - days(1)
        updated_CRRT_Start <- CRRT_Start #no change
        updated_CRRT_End <- CRRT_End #no change
      }
    }
    
    updated_df <- cbind.data.frame(updated_CRRT_Start, updated_CRRT_End,updated_HD_Start,updated_HD_End,status_flag)
  }else{
    status_flag <- "No Overlap"
    updated_df <- cbind.data.frame(CRRT_Start, CRRT_End,HD_Start,HD_End,status_flag)
  }
  
  #reformat cuz NA produced by above steps
  for (j in 1:(ncol(updated_df) - 1)){
    if (is.na(updated_df[,j]) == F){
      updated_df[,j] <- reformat_10char_dates_func(updated_df[,j])
    }
  }
  
  return(updated_df)
}

check_T1start_T2start_status <- function(t1_time,t2_time){
  cond1 <-  ymd_hms(t1_time) < ymd_hms(t2_time)
  cond2 <-  ymd_hms(t1_time) >= (ymd_hms(t2_time) - days(1))
  
  if (ymd_hms(t1_time) < (ymd_hms(t2_time) - days(1))){ #T1 starts more than 24 hours before T2, need to exclude
    res <- "T1 starts 24h before T2 starts"
  }else if (cond1 & cond2){  #T1 starts before T2 Start but less than/equals 24 hours, need to correct
    res <- "T1 starts before T2 starts But Less Than/equals 24h"
  }else if (ymd_hms(t1_time) >= ymd_hms(t2_time)){ #T1 Starts after T2, keep original
    res <- "T1 starts after/at T2 Start"
  }
  return(res)

}


check_T1end_T2end_status <- function(t1_time,t2_time){
  cond1 <-  ymd_hms(t1_time) > ymd_hms(t2_time)
  cond2 <-  ymd_hms(t1_time) <= (ymd_hms(t2_time) + days(1))
  
  if (ymd_hms(t1_time) > (ymd_hms(t2_time) + days(1))){ #T1 ends more than 24 hours after T2 ends
    res <- "T1 ends 24h after T2 ends"
  }else if (cond1 & cond2){  #T1 ends after T2 Start but less than/equals 24 hours
    res <- "T1 ends after T2 ends But Less Than/equals 24h"
  }else if (ymd_hms(t1_time) <= ymd_hms(t2_time)){ #T1 ends before/at T2
    res <- "T1 ends before/at T2 ends"
  }
  return(res)
  
}

############## Extract Raw data functions ############## 
get_raw_var_values_1option_func <- function(analysis_df,analysis_ID,feature_name,colname_1st_choice){
  #analysis_df <- raw_Vitals_df
  #analysis_ID <- analysis_ID
  #feature_name <- "Temperature_D1_LOW"
  #colname_1st_choice <- "TEMPERATURE_D1_LOW_VALUE"
  
  
  value_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
  colnames(value_df) <- c("STUDY_PATIENT_ID",feature_name)
  for (i in 1:length(analysis_ID)){
    if (i %% 1000 ==0){
      print(i)
    }
    curr_id <- analysis_ID[i]
    curr_idxes <- which(analysis_df[,"STUDY_PATIENT_ID"] == curr_id)
    
    if (length(curr_idxes) >0){ #if ID is in analysis_df
      curr_1st_choice_val <- analysis_df[curr_idxes,colname_1st_choice]
      curr_val <- curr_1st_choice_val
    }else{
      curr_val <- NA
    }
    
    value_df[i,"STUDY_PATIENT_ID"] <- curr_id
    value_df[i,feature_name] <- curr_val
    
  }
  return(value_df)
}

get_raw_var_values_2options_func <- function(analysis_df,analysis_ID,feature_name,colname_1st_choice,colname_2nd_choice){
  #analysis_df <- raw_Vitals_df
  #analysis_ID <- analysis_ID
  #feature_name <- "MAP_D1_LOW"
  #colname_1st_choice <- "ART_MEAN_D1_LOW_VALUE"
  #colname_2nd_choice <- "CUFF_MEAN_D1_LOW_VALUE"
  
  
  value_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
  colnames(value_df) <- c("STUDY_PATIENT_ID",feature_name)
  for (i in 1:length(analysis_ID)){
    if (i %% 1000 ==0){
      print(i)
    }
    curr_id <- analysis_ID[i]
    curr_idxes <- which(analysis_df[,"STUDY_PATIENT_ID"] == curr_id)
    
    if (length(curr_idxes) >0){ #if ID is in analysis_df
      curr_1st_choice_val <- analysis_df[curr_idxes,colname_1st_choice]
      curr_2nd_choice_val <- analysis_df[curr_idxes,colname_2nd_choice]
      
      if (is.na(curr_1st_choice_val) == F){
        curr_val <- curr_1st_choice_val
      }else if (is.na(curr_2nd_choice_val) == F) {
        curr_val <- curr_2nd_choice_val
      }else{
        curr_val <- NA
      }
    }else{
      curr_val <- NA
    }
    
    value_df[i,"STUDY_PATIENT_ID"] <- curr_id
    value_df[i,feature_name] <- curr_val
    
  }
  return(value_df)
}

get_vars_for_analysisId_func <- function(ananlysis_df, analysis_ID){
  updated_analysis_df <- ananlysis_df[which(ananlysis_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
  return(updated_analysis_df)
}

#Get ICU D0 and D3 day start and end time
#ICU D0 refer to ICU admit time to the same day at 23:59:59
get_sameDay_lasttime_func <- function(input_time){
  ymd_part <- strsplit(as.character(input_time),split = " ")[[1]][1]
  same_day_last_timepoint <- ymd_hms(paste(ymd_part,"23:59:59"))
  return(same_day_last_timepoint)
}

get_ICUD0_D3_dates_func <- function(icu_start,icu_end){
  ICU_D0D3_df <- as.data.frame(matrix(NA, nrow = 4, ncol = 3))
  colnames(ICU_D0D3_df) <- c("Day","Day_start","Day_End")
  ICU_D0D3_df$Day <- seq(0,3)
  for (i in 1:4){
    if (i == 1){
      if (nchar(as.character(icu_start)) == 10){
      ICU_D0D3_df[i, "Day_start"] <- paste(as.character(icu_start),"00:00:00")
      }else{    
        ICU_D0D3_df[i, "Day_start"] <- as.character(icu_start)
      }
      ICU_D0D3_df[i, "Day_End"]   <- as.character(get_sameDay_lasttime_func(icu_start))
    }else{
      ICU_D0D3_df[i, "Day_start"] <- paste(as.character(ymd_hms(ICU_D0D3_df[i-1, "Day_End"]) + seconds(1)),"00:00:00")
      ICU_D0D3_df[i, "Day_End"]   <- as.character(get_sameDay_lasttime_func(ymd_hms(ICU_D0D3_df[i, "Day_start"])))
    }
  }
  
  #Fiter out the start time after ICU end
  idx_startAfterICUend <- which(ymd_hms(ICU_D0D3_df[,"Day_start"]) >= icu_end)
  ICU_D0D3_df[idx_startAfterICUend,c("Day_start","Day_End")] <- NA
  
  #If Day start before ICU end, but Day end after ICU end, Set Day_end as ICU end time
  idx_endAfterICUend <- which(ymd_hms(ICU_D0D3_df[,"Day_End"]) > icu_end)
  ICU_D0D3_df[idx_endAfterICUend,"Day_End"] <- as.character(icu_end)
  
  
  ICU_D0D3_df[,"Hours_InOneDay"] <- round(as.numeric(difftime(ICU_D0D3_df[, "Day_End"] ,ICU_D0D3_df[, "Day_start"] ,units = "hours")),2)
  
  #Reformat Add 00:00:00
  aval_n <- length(which(is.na(ICU_D0D3_df[,"Day_start"])==F))
  for (i in 1:aval_n){
    curr_start <- ICU_D0D3_df[i,"Day_start"]
    if (nchar(curr_start) == 10){
      ICU_D0D3_df[i,"Day_start"] <- paste(ICU_D0D3_df[i,"Day_start"],"00:00:00")
    }
    curr_end <- ICU_D0D3_df[i,"Day_End"]
    if (nchar(curr_end) == 10){
      ICU_D0D3_df[i,"Day_End"] <- paste(ICU_D0D3_df[i,"Day_End"],"00:00:00")
    }
  }
  return(ICU_D0D3_df)
}


get_ICUD0_D4_dates_func <- function(icu_start,icu_end){
  ICU_D0D4_df <- as.data.frame(matrix(NA, nrow = 5, ncol = 3))
  colnames(ICU_D0D4_df) <- c("Day","Day_start","Day_End")
  ICU_D0D4_df$Day <- seq(0,4)
  for (i in 1:5){
    if (i == 1){
      if (nchar(as.character(icu_start)) == 10){
        ICU_D0D4_df[i, "Day_start"] <- paste(as.character(icu_start),"00:00:00")
      }else{    
        ICU_D0D4_df[i, "Day_start"] <- as.character(icu_start)
      }
      ICU_D0D4_df[i, "Day_End"]   <- as.character(get_sameDay_lasttime_func(icu_start))
    }else{
      ICU_D0D4_df[i, "Day_start"] <- paste(as.character(ymd_hms(ICU_D0D4_df[i-1, "Day_End"]) + seconds(1)),"00:00:00")
      ICU_D0D4_df[i, "Day_End"]   <- as.character(get_sameDay_lasttime_func(ymd_hms(ICU_D0D4_df[i, "Day_start"])))
    }
  }
  
  #Fiter out the start time after ICU end
  idx_startAfterICUend <- which(ymd_hms(ICU_D0D4_df[,"Day_start"]) >= icu_end)
  ICU_D0D4_df[idx_startAfterICUend,c("Day_start","Day_End")] <- NA
  
  #If Day start before ICU end, but Day end after ICU end, Set Day_end as ICU end time
  idx_endAfterICUend <- which(ymd_hms(ICU_D0D4_df[,"Day_End"]) > icu_end)
  ICU_D0D4_df[idx_endAfterICUend,"Day_End"] <- as.character(icu_end)
  
  
  ICU_D0D4_df[,"Hours_InOneDay"] <- round(as.numeric(difftime(ICU_D0D4_df[, "Day_End"] ,ICU_D0D4_df[, "Day_start"] ,units = "hours")),2)
  
  #Reformat Add 00:00:00
  aval_n <- length(which(is.na(ICU_D0D4_df[,"Day_start"])==F))
  for (i in 1:aval_n){
    curr_start <- ICU_D0D4_df[i,"Day_start"]
    if (nchar(curr_start) == 10){
      ICU_D0D4_df[i,"Day_start"] <- paste(ICU_D0D4_df[i,"Day_start"],"00:00:00")
    }
    curr_end <- ICU_D0D4_df[i,"Day_End"]
    if (nchar(curr_end) == 10){
      ICU_D0D4_df[i,"Day_End"] <- paste(ICU_D0D4_df[i,"Day_End"],"00:00:00")
    }
  }
  return(ICU_D0D4_df)
}


#exclude the values if patient does not in ICU for that day
#e.g, the pt ICU duration includes D0, D1, so BUND3 value should be exclude. cuz BUND3 refers to ICU start + 3days
remove_featureValue <- function(analysis_df,ICU_D0toD3_df){
  for (i in 1:nrow(analysis_df)){
    if (i %% 1000 == 0){print(i)}
    curr_id <- analysis_df[i,"STUDY_PATIENT_ID"]
    curr_ICU_days <- ICU_D0toD3_df[which(ICU_D0toD3_df[,"STUDY_PATIENT_ID"] == curr_id),"ICU_Stays_inDays"]
    curr_ICU_days <- unlist(strsplit(curr_ICU_days,split = "$$",fixed = T))
    search_string <- paste0(c(curr_ICU_days,"STUDY_PATIENT_ID"),collapse = "|") #columns of ID and corresponding Day feature
    colIndxes_toexclude <- which(grepl(search_string,colnames(analysis_df)) == F)
    if (length(colIndxes_toexclude) > 0 ){
      analysis_df[i,colIndxes_toexclude] <- NA
    }
  }
  return(analysis_df)
}


#get Scr df in window
get_value_df_inWindow_func <- function(pt_df,window_start, window_end,time_col){
  inWindow_idxes <- which(ymd_hms(pt_df[,time_col]) >= window_start & 
                            ymd_hms(pt_df[,time_col]) <= window_end)
  pt_df_inWindow <- pt_df[inWindow_idxes,]
  pt_df_inWindow <- pt_df_inWindow[order(pt_df_inWindow[,time_col]),] #ordered
  
  return(pt_df_inWindow)
}

#get baseline Scr (Without revolve MDRD=75)
get_baseline_scr_func <- function(hosp_start_time,pt_scr_df){
  #hosp_start_time <- curr_hosp_start
  #pt_scr_df <- curr_scr_df
  
  #1.Baseline Scr (The outpatient sCr value closest to 1 day before hospital admission up to 1 year. 
  #                If no outpatient sCr, use the inpatient sCr value closet to 7 days before index hospital admission up to 1 year. 
  curr_window_start <- hosp_start_time - years(1)
  curr_window_end <- hosp_start_time - days(1)
  curr_scrdf_inWindow <- get_value_df_inWindow_func(pt_scr_df,curr_window_start,curr_window_end,"SCR_ENTERED")
  curr_scr_inWindow_outpt_df <- curr_scrdf_inWindow[which(grepl("Outpatient",curr_scrdf_inWindow[,"SCR_ENCOUNTER_TYPE"])==T),]
  
  if (nrow(curr_scr_inWindow_outpt_df) > 0){
    cloest_out_indx <- which(curr_scr_inWindow_outpt_df[,"SCR_ENTERED"] == max(curr_scr_inWindow_outpt_df[,"SCR_ENTERED"]))
    cloest_out_val <- curr_scr_inWindow_outpt_df[cloest_out_indx,"SCR_VALUE"]
  }else{
    cloest_out_val <- NA
  }
  
  #inpteint Scr before hosp admit (from hosp_start - 7 day to hosp_start-1 year)
  curr_window_start <- hosp_start_time - years(1)
  curr_window_end <- hosp_start_time - days(7)
  curr_scrdf_inWindow <- get_value_df_inWindow_func(pt_scr_df,curr_window_start,curr_window_end,"SCR_ENTERED")
  curr_scr_inWindow_inpt_df <- curr_scrdf_inWindow[which(grepl("Inpatient",curr_scrdf_inWindow[,"SCR_ENCOUNTER_TYPE"])==T),]
  if (nrow(curr_scr_inWindow_inpt_df) > 0 ){
    cloest_in_indx <- which(curr_scr_inWindow_inpt_df[,"SCR_ENTERED"] == max(curr_scr_inWindow_inpt_df[,"SCR_ENTERED"]))
    cloest_in_val <- curr_scr_inWindow_inpt_df[cloest_in_indx,"SCR_VALUE"]
  }else{
    cloest_in_val <- NA
  }
  
  #resolve scr 
  if (is.na(cloest_out_val) == F){ #if outptient avail
    bl_val <- cloest_out_val
  }else if(is.na(cloest_in_val) == F){
    bl_val <- cloest_in_val
  }else{
    bl_val <- NA
  }
  return(bl_val)
}

#EGFR MDRD equation
MDRD_equation<-function(Scr,age,gender,race){
  #GFR (mL/min/1.73 m²) = 175 × (SCr)-1.154 × (Age)-0.203 × (0.742 if female) × (1.212 if African American) (conventional units)
  
  a<-1
  b<-1
  
  #if qualities the tfollowing 2, updated a and b
  if(race=="BLACK/AFR AMERI"){
    b<-1.210
  }
  if(gender=="F"){
    a<-0.742
  }
  #removed 88.4 cuz unit
  score<-175*((Scr)^(-1.154))*((age)^(-0.203))*a*b  #first half part is the same for all
  
  return(score)
  
}

#reverse EGFR equation
SolveScr_reverse_MDRD_equation<-function(age,gender,race){
  a<-1
  b<-1
  
  #if qualities the tfollowing 2, updated a and b
  if(race=="BLACK/AFR AMERI"){
    b<-1.210
  }
  if(gender=="F"){
    a<-0.742
  }
  GFR <- 75
  scr <- (175*a*b/(GFR*(age^(0.203))))^(1/1.154)
  return(scr)
  
}

#Compute KDIGO score for each time in Scr_data
get_KDIGO_Score_forScrdf_func <- function(bl_scr,scr_data){
  kidgo_score_df <- as.data.frame(matrix(NA, nrow = nrow(scr_data), ncol = 2))
  colnames(kidgo_score_df) <-c("Scr_Time","KDIGO")
  for (t in 1:nrow(scr_data)){
    curr_KIDGO_score <- NA
    curr_t_scr <- scr_data[t,"SCR_VALUE"]
    curr_t     <- ymd_hms(scr_data[t,"SCR_ENTERED"])
    kidgo_score_df[t, "Scr_Time"] <- as.character(curr_t)
    
    idxes_within48hours_before_t <- which(scr_data[,"SCR_ENTERED"] >= curr_t - hours(48)
                                          & scr_data[,"SCR_ENTERED"] < curr_t )
    
    
    if (curr_t_scr >= 3*bl_scr | curr_t_scr >= 4 ){  #Stage 3
      curr_KIDGO_score <- 3
    }else if (curr_t_scr >= 2*bl_scr & curr_t_scr < 3*bl_scr ){  #Stage 2
      curr_KIDGO_score <- 2
    } else if (curr_t_scr >= 1.5*bl_scr & curr_t_scr < 2*bl_scr ){  #Stage 1
      curr_KIDGO_score <- 1
    }else if (length(idxes_within48hours_before_t) > 0){ #Stage 1
      #compute all increase for preious t
      all_previous_scr <- scr_data[idxes_within48hours_before_t, "SCR_VALUE"]
      all_increase <- curr_t_scr - all_previous_scr
      increase_lg03_idxes <- which(all_increase >= 0.3)
      if (length(increase_lg03_idxes) > 0){
        curr_KIDGO_score <- 1
      }else{
        curr_KIDGO_score<-0
      }
    }else{
      curr_KIDGO_score<-0
    }
    
    
    kidgo_score_df[t, "KDIGO"] <- curr_KIDGO_score
  }
  
  return(kidgo_score_df)
  
}

#update if each time step is in RRT duration, if so, update the KDIGO score to 4
#'@ADDED jun09 21: 48 hours extention of RRT end (If curr time is within 48 hours after RRT, it counted as 4)
update_KDIGO_df_forRRT_func<- function(KIDGO_df,CRRT_start,CRRT_end,HD_start,HD_end,extend_hours){
  for (i in 1:nrow(KIDGO_df)){
    curr_time <- ymd_hms(KIDGO_df[i,"Scr_Time"])
    
    if (is.na(CRRT_start) == F & is.na(HD_start) == T){ #if has CRRT , no HD
      cond1 <- (curr_time >= CRRT_start & curr_time <= (CRRT_end + hours(extend_hours))) #if in CRRT
      if (cond1 == T){ 
        KIDGO_df[i,"KDIGO"] <- 4
      }
    } else if (is.na(CRRT_start) == T & is.na(HD_start) == F){ #if has HD , no CRRT
      cond2 <- (curr_time >= HD_start & curr_time <= (HD_end + hours(extend_hours))) #if in HD
      if (cond2 == T){
        KIDGO_df[i,"KDIGO"] <- 4
      }
    }else if (is.na(CRRT_start) == F & is.na(HD_start) == F){ #if has both info, check both
      cond1 <- (curr_time >= CRRT_start & curr_time <= (CRRT_end + hours(extend_hours))) #if in CRRT
      cond2 <- (curr_time >= HD_start & curr_time <= (HD_end + hours(extend_hours))) #if in HD
      if (cond1 == T | cond2 == T){
        KIDGO_df[i,"KDIGO"] <- 4
      }
    }

  }
  return(KIDGO_df)
}


#Compute EGFR only if pt has at least one outpatient SCr after ICU discharge within time_window_start (eg.120 days)
#using the outpatient SCr closest but before (ICU discharge + 120 days) 
#if more than 2 outpatient SCr less than 30 days apart and all before 120 days post-discharge
# use the median of all these values
compute_EGFR_inWindow_func2 <- function(time_window_start,time_window_expansion,Analysis_Ids,time_data,demo_info_df,Outpt_Scr_df){
  # time_window_start <- days(120)
  # time_window_expansion <- days(30)
  # Analysis_Ids <- analysis_ID
  # time_data <- All_time_df
  # demo_info_df <- All_RACE_GENDER_df
  # Outpt_Scr_df <- All_OutptSCr_df


  EGFR_inWindow_df <- as.data.frame(matrix(NA, nrow = length(Analysis_Ids), ncol = 5))
  colnames(EGFR_inWindow_df) <- c("STUDY_PATIENT_ID","EGFR_120d","n_Scr_afterICU","n_Scr_AfterICU_Before120d","N_SCr_USED")

  scr_used_df_list <- list()
  for (p in 1:length(Analysis_Ids)){
    if (p %% 500 == 0){
      print(p)
    }
    
    curr_id <- Analysis_Ids[p]
    EGFR_inWindow_df[p, "STUDY_PATIENT_ID"] <- curr_id
    
    #time data
    curr_pt_time_data <- time_data[which(time_data[,"STUDY_PATIENT_ID"]==curr_id),]
    curr_ICU_START <-  ymd_hms(curr_pt_time_data[,"Updated_ICU_ADMIT_DATE"])
    curr_ICU_STOP  <-  ymd_hms(curr_pt_time_data[,"Updated_ICU_DISCHARGE_DATE"])
    
    #demo data
    curr_pt_demo_data <- demo_info_df[which(demo_info_df[,"STUDY_PATIENT_ID"]==curr_id),]
    curr_age <- curr_pt_demo_data[,"AGE"]
    curr_race <-  curr_pt_demo_data[,"RACE"]
    curr_gender <-  curr_pt_demo_data[,"GENDER"]
    
    #outpatient Scr
    curr_outpt_Scr_df <- Outpt_Scr_df[which(Outpt_Scr_df[,"STUDY_PATIENT_ID"]==curr_id),]
    
    #outpatient Scr after ICU 
    curr_outpt_scr_afterICU_df  <- curr_outpt_Scr_df[which(ymd_hms(curr_outpt_Scr_df[,"SCR_ENTERED"]) > curr_ICU_STOP),]
    EGFR_inWindow_df[p, "n_Scr_afterICU"]  <- nrow(curr_outpt_scr_afterICU_df)

    #outpatient Scr after ICU but before time_window_start (120 days)
    curr_ICU_STOP_plusTWdays <- curr_ICU_STOP + time_window_start
    curr_afterICU_beforeTW_data <- curr_outpt_scr_afterICU_df[curr_outpt_scr_afterICU_df[,"SCR_ENTERED"] > curr_ICU_STOP & 
                                                              curr_outpt_scr_afterICU_df[,"SCR_ENTERED"] < curr_ICU_STOP_plusTWdays,]
    EGFR_inWindow_df[p, "n_Scr_AfterICU_Before120d"] <- nrow(curr_afterICU_beforeTW_data)

    #must have demo data to compute egfr
    if (is.na(curr_age)== T | is.na(curr_gender)== T |is.na(curr_race)== T ){
      EGFR_inWindow_df[p, "EGFR_120d"] <- NA
    }else{
      #must have at least 1 outpts data in window
      if(EGFR_inWindow_df[p, "n_Scr_AfterICU_Before120d"] == 0 ){ 
        EGFR_inWindow_df[p, "EGFR_120d"] <- NA
      }else { #if there are outpts values in this window
        #make sure to add dates when no 00:00:00 so we can use ymd_hms in next step
        for (t_i in nrow(curr_afterICU_beforeTW_data)){
          curr_afterICU_beforeTW_data[t_i, "SCR_ENTERED"] <- reformat_10char_dates_func(curr_afterICU_beforeTW_data[t_i, "SCR_ENTERED"])
        }
        #reformat time cols to ymd_hms
        curr_afterICU_beforeTW_data[,"SCR_ENTERED"] <- ymd_hms(curr_afterICU_beforeTW_data[,"SCR_ENTERED"])
        #reordered by time 
        ordered_outpts_data <- curr_afterICU_beforeTW_data[order(curr_afterICU_beforeTW_data[,"SCR_ENTERED"]),]
        
        #outpatient data after ICU dischrage cloest to ICU discharge +TW days
        cloest_idx <- nrow(ordered_outpts_data) #the last one since it is ordered
        cloest_time <- ordered_outpts_data[cloest_idx,"SCR_ENTERED"]
        cloest_scr_value <- ordered_outpts_data[cloest_idx,"SCR_VALUE"]
        
        #cloest time - 30 days
        cloest_time_minus30days <- cloest_time - time_window_expansion
   
        #check if there is any value 30 days before cloest_time
        days30_before_idxes <- which(ordered_outpts_data[,"SCR_ENTERED"] > cloest_time_minus30days & 
                                     ordered_outpts_data[,"SCR_ENTERED"] < cloest_time)
        cloest_time_idx <- which(ordered_outpts_data[,"SCR_ENTERED"] == cloest_time)
        
        if(length(days30_before_idxes) > 0 ){ 
          #if there are multiple, take the median of all
          final_scr_value <- median(ordered_outpts_data[c(days30_before_idxes,cloest_time_idx),"SCR_VALUE"])
          scr_used_df <- ordered_outpts_data[c(days30_before_idxes,cloest_time_idx),c("STUDY_PATIENT_ID","SCR_VALUE","SCR_ENTERED")]
        }else{
          final_scr_value <- cloest_scr_value #take the clost value only
          scr_used_df <- ordered_outpts_data[cloest_time_idx,c("STUDY_PATIENT_ID","SCR_VALUE","SCR_ENTERED")]
          
        }
        EGFR_inWindow_df[p, "N_SCr_USED"] <- nrow(scr_used_df)
          
        EGFR_inWindow_df[p, "EGFR_120d"] <- MDRD_equation(final_scr_value,curr_age,curr_gender,curr_race)
        #EGFR_inWindow_df[p, "EGFR_120d"] <- EPI_equation(final_scr_value,curr_age,curr_gender,curr_race)
        
        scr_used_df_list[[p]] <- scr_used_df
      }
      
    }
    
    
  }
  

  all_scr_used_df <- do.call(rbind,scr_used_df_list)
  return(list(EGFR_inWindow_df,all_scr_used_df))
}

#################################################################################
#Data repreocess
#1.Generate one score for SOFA and APACHE by taking the sum of SOFA/APACHE component
sum_score_func <- function(analysis_df,score_name){
  #Get score sum
  score_col_index <- which(grepl(score_name,colnames(analysis_df))) #find corresponding columns
  score_SUM <- rowSums(analysis_df[,score_col_index]) #take the row sum
  
  #get Id
  IDs <- analysis_df[,"STUDY_PATIENT_ID"]
  
  #return dataframe
  score_df <- cbind.data.frame(IDs,score_SUM)
  colnames(score_df) <- c("STUDY_PATIENT_ID",paste0(score_name,"_SUM"))
  return(score_df)
}


min_max_func <- function(feautre_col){
  normed_col <- (feautre_col-min(feautre_col,na.rm = T))/(max(feautre_col,na.rm = T)-min(feautre_col,na.rm = T))
  return(normed_col)
}


change_feature_name_func <- function(input_df,tochange_colname,target_name){
  col_idx <- which(colnames(input_df) == tochange_colname)
  colnames(input_df)[col_idx] <- target_name
  return(input_df)
}


#####
code_Label_YN_func <- function(input_df,label_colname){
  #Recode label as Y for 1, N for 0, cuz for caret package, we cannot use 1 or 0 for outcome
  
  input_df[,label_colname][which(input_df[,label_colname] == 1)] <- "Y"
  input_df[,label_colname][which(input_df[,label_colname] == 0)] <- "N"
  input_df[,label_colname] <-as.factor(input_df[,label_colname])
  return(input_df)
}


code_Label_10_func <- function(input_df,label_colname){
  #Reocde label from YN back to 10 for performance computation
  input_df[,label_colname] <- as.character(input_df[,label_colname])
  input_df[,label_colname][which(input_df[,label_colname] == "Y")] <- 1
  input_df[,label_colname][which(input_df[,label_colname] == "N")] <- 0
  return(input_df)
}

create_fold_func <- function(analysis_df){
  #this function returns row index of analysis_df for each fold
  #create 10 folds
  set.seed(123)
  Idx_Folds <- createFolds(1:nrow(analysis_df), k = 10)
  Fold_list <- list()
  for (i in 1:10){
    curr_ids <- Idx_Folds[[i]]
    curr_fold_name <- rep(i,length(curr_ids))
    Fold_list[[i]] <- cbind(curr_ids,curr_fold_name)
  }
  
  Idxes_fold_df <- do.call(rbind.data.frame,Fold_list)
  colnames(Idxes_fold_df) <- c("Row_indxes","Fold")
  
  return(Idxes_fold_df)
}

#Sampling function
Model_sampling_func <- function(upsample_flag,train_data,label_col_name,seed_num){
  # upsample_flag <- 0
  # train_data <- curr_train_data
  # label_col_name <- outcome_colname
  # seed_num <- 1
  
  #Get label col index
  label_col_index <- which(colnames(train_data) == label_col_name)
  
  #Sampling
  if(upsample_flag==1){ #upsampling
    set.seed(seed_num)
    up_train <- upSample(x = train_data[, -label_col_index],
                         y = as.factor(train_data[,label_col_name]), yname = label_col_name)  
    sampled_train_data <- up_train
    
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_num)
    down_train <- downSample(x = train_data[, -label_col_index],
                             y = as.factor(train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
    
  }else{
    original_train <- train_data
    sampled_train_data <- original_train
  }
  
  #also return real feature name since sampling removed feature names
  colnames(sampled_train_data) <- colnames(train_data)
  
  return(sampled_train_data)
}

#Convert prediction probability to prediction class, threshold = 0.5
get_pred_class_func <- function(predicted_prob){
  pred_class <- predicted_prob
  pred_class[which(predicted_prob >=0.5)] <- 1
  pred_class[which(predicted_prob < 0.5)] <- 0
  pred_class <- as.factor(pred_class)
  return(pred_class)
}

#XGboost functions
#return traingined model and importnce matrix
train_xgboost<-function(data_part,train_label,xgb_params,num_rounds,upsample_flag){
  # data_part <- train_data_part
  # train_label <- sampled_train_data[,outcome_index]
    
  #recode train label back to 0 and 1
  train_label <- as.character(train_label)
  train_label[which(train_label == "Y")] <- 1
  train_label[which(train_label == "N")] <- 0
  train_label <- as.numeric(train_label)
  
  #factorize the label column only if using "reg:squarederror"
  curr_objective <- xgb_params$objective
  if (curr_objective == "reg:squarederror"){
    train_label <- as.factor(train_label)
  }
  
  train_matrix <- xgb.DMatrix(data = as.matrix(data_part), label = train_label)
  trained_model <- xgb.train(params = xgb_params,data = train_matrix,nrounds = num_rounds)
  importance_matrix = xgb.importance(model = trained_model)
  
  return(list(trained_model,importance_matrix[,1:2]))
  
}


predict_xgboost <- function(curr_model,test_data_part,test_label,pred_type){
  dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
  predicted_prob <- predict(curr_model, dtest, type = pred_type)
  return(predicted_prob)
}

cv_func <- function(analysis_df,Idxes_fold_df,outcome_colname,model_name,validation_df,upsample_flag,N_sampling){
  # analysis_df <- feature_df
  # outcome_colname <- "died_inp"
  # model_name <- "SVM"
  # validation_df <- UTSW_feature_df
  # upsample_flag <- 0
  # N_sampling <- 10

  #code factor as Y for 1, and N for 0, for caret package, we cannot use 1 or 0 for outcome
  #For UK df
  analysis_df <- code_Label_YN_func(analysis_df,outcome_colname)
  outcome_index <- which(colnames(analysis_df) == outcome_colname)
  #For utsw validation df
  validation_df <- code_Label_YN_func(validation_df,outcome_colname)
  outcome_index_utsw <- which(colnames(validation_df) == outcome_colname)
  
  
  
  #Training and prediction
  All_sampling_results_perFold <- list(NA)
  All_sampling_results_perFold_validation <- list(NA)
  All_sampling_importance_matrix_perFold <- list(NA)
  for (i in 1:10){ #10 Fold
    curr_test_data <- analysis_df[which(Idxes_fold_df[,"Fold"] == i),]
    curr_train_data <-  analysis_df[which(Idxes_fold_df[,"Fold"] != i),]
    
    #sampling for traning data
    sampling_pred_table_list <- list(NA)
    sampling_pred_table_Validation_list <- list(NA)
    sampling_importance_matrix_list <- list(NA)
    for (s in 1:N_sampling){
      print(paste0("Fold",i,": Sampling",s))
      seed_num <- s*i
      sampled_train_data <- Model_sampling_func(upsample_flag,curr_train_data,outcome_colname,seed_num)
      
     
      if (ncol(sampled_train_data) == 2){  #For data has one feature column
        train_data_part <- as.data.frame(sampled_train_data[,-outcome_index])
        colnames(train_data_part) <- colnames(sampled_train_data)[1]
      }else{
        train_data_part <- sampled_train_data[,-outcome_index]
      }
      
      if (ncol(curr_test_data) == 2){
        test_data_part <- as.data.frame(curr_test_data[,-outcome_index])
        colnames(test_data_part) <- colnames(curr_test_data)[1]
      }else{
        test_data_part <- curr_test_data[,-outcome_index]
      }
      
      if (ncol(validation_df) == 2){
        val_data_part <- as.data.frame(validation_df[,-outcome_index_utsw])
        colnames(val_data_part) <- colnames(validation_df)[1]
      }else{
        val_data_part <- validation_df[,-outcome_index_utsw]
      }
      
      #Model
      if (model_name == "SVM"){
        model_svm  <- train(train_data_part, sampled_train_data[,outcome_index],method='svmRadial' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
      }else if (model_name == "SVM_TOP"){
        model_svm  <- train(train_data_part, sampled_train_data[,outcome_index],method='svmPoly' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        importance_matrix_SVM <- import_res$importance
        importance_matrix_SVM$Sample_Index <- paste0("Sample",s)
        importance_matrix_SVM$Feature <- rownames(importance_matrix_SVM)
        colnames(importance_matrix_SVM)[1] <- "Importance_Scaled0_100"
        rownames(importance_matrix_SVM) <- NULL
        importance_matrix_SVM <- importance_matrix_SVM[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
        #model with top 25 features 
        if (nrow(importance_matrix_SVM) >= 25){
          top_features <- as.character(unlist(importance_matrix_SVM[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_SVM[1:nrow(importance_matrix_SVM),"Feature"]))
        }
        
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_svm  <- train(new_train_part, sampled_train_data[,outcome_index],method='svmPoly' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
        
      }else if (model_name == "RF"){
        model_rf <- train(train_data_part, sampled_train_data[,outcome_index], method='rf',
                          trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
        curr_model <- model_rf
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
      }else if (model_name == "RF_TOP"){
        model_rf <- train(train_data_part, sampled_train_data[,outcome_index], method='rf',
                          trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
        curr_model <- model_rf
        import_res <- varImp(curr_model, scale = TRUE)
        importance_matrix_RF <- import_res$importance
        importance_matrix_RF$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        importance_matrix_RF$Feature <- rownames(importance_matrix_RF)
        colnames(importance_matrix_RF)[1] <- "Importance_Scaled0_100"
        rownames(importance_matrix_RF) <- NULL
        importance_matrix_RF <- importance_matrix_RF[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
        #model with top 25 features 
        if (nrow(importance_matrix_RF) >= 25){
          top_features <- as.character(unlist(importance_matrix_RF[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_RF[1:nrow(importance_matrix_RF),"Feature"]))
        }
        
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_rf <- train(new_train_part, sampled_train_data[,outcome_index], method='rf',
                          trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
        curr_model <- model_rf
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
      }else if (model_name == "LogReg"){
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = sampled_train_data, family = binomial)
        curr_model <- model_logreg
        curr_coef <- as.data.frame(curr_model$coefficients)
        zero_coef_feautre <- rownames(curr_coef)[which(is.na(curr_coef$`curr_model$coefficients`) == T)]
        library(reghelper)
        beta_coef_res <- beta(model_logreg, skip = zero_coef_feautre)
        beta_coef <- as.data.frame(beta_coef_res$coefficients)
        m_idxes <- match(gsub(".z","",rownames(beta_coef)), rownames(curr_coef))
        
        curr_importance_matrix <- cbind(rownames(curr_coef),curr_coef)
        curr_importance_matrix$Beta_Coef <- NA
        curr_importance_matrix$Beta_Coef[m_idxes] <- beta_coef$Estimate
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(curr_importance_matrix) == "(Intercept)")
        curr_importance_matrix <- curr_importance_matrix[-inter_index,]
        colnames(curr_importance_matrix) <- c("Feature","Coeff","Beta_Coef","Sample_Index")
        rownames(curr_importance_matrix) <- NULL
        
      }else if (model_name == "LogReg_TOP"){
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = sampled_train_data, family = binomial)
        curr_model <- model_logreg
        curr_coef <- as.data.frame(curr_model$coefficients)
        importance_matrix_LR <- cbind(rownames(curr_coef),curr_coef)
        importance_matrix_LR$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(importance_matrix_LR) == "(Intercept)")
        importance_matrix_LR <- importance_matrix_LR[-inter_index,]
        colnames(importance_matrix_LR) <- c("Feature","Coeff")
        
        #model with top 25 features 
        if (nrow(importance_matrix_LR) >= 25){
          top_features <- as.character(unlist(importance_matrix_LR[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_LR[1:nrow(importance_matrix_LR),"Feature"]))
        }
        new_sampled_train_data<- as.data.frame(sampled_train_data[,c(top_features,outcome_colname)])
        colnames(new_sampled_train_data) <- c(top_features,outcome_colname)
        
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = new_sampled_train_data, family = binomial)
        curr_model <- model_logreg
        curr_coef <- as.data.frame(curr_model$coefficients)
        curr_importance_matrix <- cbind(rownames(curr_coef),curr_coef)
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(curr_importance_matrix) == "(Intercept)")
        curr_importance_matrix <- curr_importance_matrix[-inter_index,]
        colnames(curr_importance_matrix) <- c("Feature","Coeff")
        rownames(curr_importance_matrix) <- NULL
        
      }else if (model_name == "XGB"){
        #first find feature importanance 
        num_rounds<- 10
        xgb_params <- list(booster = "gbtree","objective" = "reg:logistic")
        xgb_res <- train_xgboost(train_data_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_xgb <- xgb_res[[1]] #model with all features
        importance_matrix_xgb <- xgb_res[[2]]
        curr_model <- model_xgb
        curr_importance_matrix <- importance_matrix_xgb
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        
      }else if (model_name == "XGB_TOP"){
        #first find feature importanance 
        num_rounds<- 10
        xgb_params <- list(booster = "gbtree","objective" = "reg:logistic")
        xgb_res <- train_xgboost(train_data_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_xgb <- xgb_res[[1]] #model with all features
        importance_matrix_xgb <- xgb_res[[2]]
        
        #model with top 25 features 
        if (nrow(importance_matrix_xgb) >= 25){
          top_features <- as.character(unlist(importance_matrix_xgb[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_xgb[1:nrow(importance_matrix_xgb),"Feature"]))
        }
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_top_xgb_res <- train_xgboost(new_train_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_top_xgb <- model_top_xgb_res[[1]] #model with top features
        importance_matrix_xgb_top <- model_top_xgb_res[[2]]
        curr_model <- model_top_xgb
        curr_importance_matrix <- importance_matrix_xgb_top
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
      }
      
      sampling_importance_matrix_list[[s]] <- curr_importance_matrix
      
      #prediction probabiliy of test data and validation data
      if (model_name =="SVM" | model_name =="RF"){
        pred_res <- predict(curr_model, newdata = test_data_part,type = "prob")  
        pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
        
        pred_res <- predict(curr_model, newdata = val_data_part,type = "prob")  
        pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
        
      }else if ( model_name =="SVM_TOP" | model_name =="RF_TOP"){
        new_test_data_part <- as.data.frame(test_data_part[,top_features])
        colnames(new_test_data_part) <- top_features
        pred_res <- predict(curr_model, newdata = new_test_data_part,type = "prob")  
        pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
        
        new_val_data_part <- as.data.frame(val_data_part[,top_features])
        colnames(new_val_data_part) <- top_features
        pred_res <- predict(curr_model, newdata = new_val_data_part,type = "prob")  
        pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
        
      }else if (model_name == "XGB"){
        #convert to xgboost format 
        pred_prob <- predict_xgboost(curr_model,test_data_part,curr_test_data[,outcome_colname],"prob")
        pred_prob_validation <- predict_xgboost(curr_model,val_data_part,validation_df[,outcome_colname],"prob")
        
        
      }else if (model_name == "XGB_TOP"){
        #convert to xgboost format 
        new_test_data_part <- as.data.frame(test_data_part[,top_features])
        colnames(new_test_data_part) <- top_features
        pred_prob <- predict_xgboost(curr_model,new_test_data_part,curr_test_data[,outcome_colname],"prob")
        new_val_data_part <- as.data.frame(val_data_part[,top_features])
        colnames(new_val_data_part) <- colnames(val_data_part[,top_features])
        pred_prob_validation <- predict_xgboost(curr_model,new_val_data_part,validation_df[,outcome_colname],"prob")
        
      }else if (model_name == "LogReg_TOP"){
        new_test_data <- as.data.frame(curr_test_data[,c(top_features,outcome_colname)])
        colnames(new_test_data) <- c(top_features,outcome_colname)
        pred_res <- predict(curr_model, new_test_data, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob <- as.numeric(pred_res)
        
        new_validation_df <- as.data.frame(validation_df[,c(top_features,outcome_colname)])
        colnames(new_validation_df) <- c(top_features,outcome_colname)
        pred_res <- predict(curr_model, new_validation_df, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob_validation <- as.numeric(pred_res)
      }else {
        pred_res <- predict(curr_model, curr_test_data, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob <- as.numeric(pred_res)
        
        pred_res <- predict(curr_model, validation_df, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob_validation <- as.numeric(pred_res)
      }
      
      
      #Prediction class
      pred_class <- get_pred_class_func(pred_prob) #testing 
      pred_class_validation <- get_pred_class_func(pred_prob_validation) #external validation
      
      
      #Code outcome of test back to 1 and 0
      curr_test_data <- code_Label_10_func(curr_test_data,outcome_colname)
      validation_df <- code_Label_10_func(validation_df,outcome_colname)
      
      #combine pred prob, pred class, acutal label, and fold index
      #Testing results
      pred_table <- cbind.data.frame(rownames(curr_test_data),pred_prob,pred_class,as.numeric(curr_test_data[,outcome_colname]),paste0("Fold",i),paste0("S",s))
      colnames(pred_table) <- c("ID","pred_prob","pred_class","Label","TestFold","TrainingSample_Index")
      sampling_pred_table_list[[s]] <- pred_table
      
      #External validation results
      pred_table_validation <- cbind.data.frame(rownames(validation_df),pred_prob_validation,pred_class_validation,as.numeric(validation_df[,outcome_colname]),paste0("Fold",i),paste0("S",s))
      colnames(pred_table_validation) <- c("ID","pred_prob","pred_class","Label","TestFold","TrainingSample_Index")
      sampling_pred_table_Validation_list[[s]] <- pred_table_validation
    } # N_sampling times sampling
    
    All_sampling_results_perFold[[i]] <- do.call(rbind,sampling_pred_table_list)
    All_sampling_results_perFold_validation[[i]] <- do.call(rbind,sampling_pred_table_Validation_list)
    All_sampling_importance_matrix_perFold[[i]] <- do.call(rbind,sampling_importance_matrix_list)
  }
  
  final_pred <- do.call(rbind,All_sampling_results_perFold)
  final_pred_validation <- do.call(rbind,All_sampling_results_perFold_validation)
  final_mportance_matrix<- do.call(rbind,All_sampling_importance_matrix_perFold)
  
  return(list(final_pred,final_pred_validation,final_mportance_matrix))
}




##Performance functions
#compute calibration slope and Intercept
compute_calibration_func <-function(perf_table){
  #perf_table <- curr_table
  
  #compute calibration Intercept and slope and plot
  pred_p <-   perf_table[,"pred_prob"]
  acutal <- as.numeric(as.vector(perf_table[,"Label"]))
  res = val.prob(pred_p,acutal, pl=FALSE)
  calib_res <- res[c("Intercept","Slope")]
  
  #Note: This is what val.prb actually doing
  #check <- glm(acutal ~ log(pred_p/(1-pred_p)),family="binomial")
  #check$coefficients
  
  #another way mentioned in Taylor's email:
  #Slope: Mean Model Output for All With Positive Outcome / Mean Model Output for All With Negative Outcome>
  #Intercept: Mean model output for all patients with a negative result for the outcome.
  all_pos_output <- perf_table[which(perf_table[,"Label"]==1),"pred_prob"]
  all_neg_output <- perf_table[which(perf_table[,"Label"]==0),"pred_prob"]
  possible_slope <- mean(all_pos_output)/mean(all_neg_output)
  possible_Intercept <- mean(all_neg_output)
  
  return(list(calib_res,possible_Intercept,possible_slope))
}

#Compute AUC, ACC, Precision, Sensitivity and...
compute_performance_func <- function(prediction_table){
  #prediction_table <- curr_v_tab
  prediction_table[,"pred_class"] <- as.factor(prediction_table[,"pred_class"])
  prediction_table[,"Label"] <- as.factor(prediction_table[,"Label"])
  
  pred_prob <- prediction_table[,"pred_prob"]
  pred_label <- prediction_table[,"pred_class"]
  actual_label <- prediction_table[,"Label"]
  
  #ROC AUC
  AUC_res <- roc(actual_label, pred_prob,direction = "<",ci =T, auc= T, quiet=TRUE) # control:0, case:1
  AUC <- as.numeric(AUC_res$auc)
  
  #By default, this function uses 2000 bootstraps to calculate a 95% confidence interval.
  AUC_95CI <- c(as.numeric(ci.auc(actual_label,pred_prob,direction = "<", quiet=TRUE))[1],as.numeric(ci.auc(actual_label,pred_prob,direction = "<"))[3])
  
  cm <- confusionMatrix(pred_label, actual_label, positive = "1", dnn = c("Prediction", "Reference"), mode="everything")
  ACC <- cm$overall[1]
  #NOte: Use Exact binomial testto compute CI for accuracy, first value is #number of sucess, #number of failure
  ##Note: Following uses binom.test(c(4883,1283),conf.level = 0.95) 
  ACC_95CI <- cm$overall[c("AccuracyLower","AccuracyUpper")] #
  Precision <- cm$byClass["Precision"]
  Sensitivity <- cm$byClass["Sensitivity"]
  Specificity  <- cm$byClass["Specificity"]
  PPV <- cm$byClass["Pos Pred Value"]
  NPV <- cm$byClass["Neg Pred Value"]
  F1_Score <- cm$byClass["F1"]
  perf_vec <- c(AUC,AUC_95CI,ACC,ACC_95CI,Precision,Sensitivity,Specificity,PPV,NPV,F1_Score)
  return(perf_vec)
}

#Compute report mean and CI for all folds
perf_Mean_CI_Folds_func <-function(Fold_perf_table){
  #Fold_perf_table <- EachFold_perf_table
  
  mean_CI_perf <- as.data.frame(matrix(NA,nrow = ncol(Fold_perf_table),ncol = 1))
  colnames(mean_CI_perf) <- "Mean_(95CI)"
  rownames(mean_CI_perf) <-  colnames(Fold_perf_table)
  for (j in 1:ncol(Fold_perf_table)){
    curr_CI <- CI(Fold_perf_table[,j], ci=0.95)
    curr_CI <- as.numeric(round(curr_CI,2))
    mean_CI_perf[j,1] <- paste0(curr_CI[2], "(",curr_CI[3],"-",curr_CI[1],")")
  }
  return(mean_CI_perf)
}

Test_AUC_diff_func <-function(perf_dir,baseline_model_file,comprison_model_file1){
  baseline_df <- read.csv(paste0(perf_dir,"/",baseline_model_file),stringsAsFactors = F)
  comp_df <- read.csv(paste0(perf_dir,"/",comprison_model_file1),stringsAsFactors = F)
  
  #Combine comparison models
  model_comp_df <- cbind.data.frame(baseline_df[,"Label"],
                                    baseline_df[,"pred_prob"],
                                    comp_df[,"pred_prob"])
  colnames(model_comp_df) <- c("Label","pred_prob_bl","pred_prob1")
  #AUC difference
  roc_bl <- roc(model_comp_df$Label, model_comp_df$pred_prob_bl)
  roc_1 <- roc(model_comp_df$Label, model_comp_df$pred_prob1)
  test_res <- roc.test(roc_bl,roc_1,method = "delong")
  pval <- test_res$p.value
  return(pval)
}

compare_AUC_func <- function(cohort_name,curr_perf,baseline_name,tocompare_modelname,method_name,perf_dir){
  # cohort_name <- "UKY"
  # baseline_name <- "max_kdigo"
  # tocompare_modelname <- curr_feature_name
  # method_name <- current_method
  
  #1.Compute exact diff  
  auc_idx <- which(grepl("AUC",rownames(curr_perf)) == T)  
  
  cohort_indxes <-  which(grepl(cohort_name,colnames(curr_perf)) == T)  
  cohort_perf<- curr_perf[,cohort_indxes]
  baseline_idx <- which(grepl(baseline_name,colnames(cohort_perf)) == T)  
  comparemodel_idx <- which(grepl(tocompare_modelname,colnames(cohort_perf)) == T)  
  baseline_AUC <- as.numeric(unlist(strsplit(cohort_perf[auc_idx,baseline_idx],"\\("))[1])
  compModel_AUC <- as.numeric(unlist(strsplit(cohort_perf[auc_idx,comparemodel_idx],"\\("))[1])
  exact_diffs <- compModel_AUC-baseline_AUC
  
  #2. diff test
  all_perf_files <- list.files(perf_dir)
  cohort_files <- all_perf_files[which(grepl(cohort_name,all_perf_files)==T)]
  methodcohort_files <- cohort_files[which(grepl(method_name,cohort_files)==T)]
  
  baseline_model_file  <- methodcohort_files[which(grepl(baseline_name,methodcohort_files)==T)]
  comprison_model_file <- methodcohort_files[which(grepl(tocompare_modelname,methodcohort_files)==T)]
  
  pvalue <- Test_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file)
  
  return(list(exact_diffs,pvalue))
}
