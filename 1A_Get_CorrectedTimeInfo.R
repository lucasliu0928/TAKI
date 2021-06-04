library(lubridate)
source("TAKI_Ultility.R")

get_one_ICU_admission_func <- function(one_hosp_df){
  # 1. If less than 12 hours between ICU admissions, combine them 
  # 2. otherwise, use the 1st ICU admission
  
  #reorder by ICU admit data
  reordered_one_hosp_df <- one_hosp_df[order(one_hosp_df[,"ICU_ADMIT_DATE"],decreasing = FALSE),]
  #compute time between current ICU and the previous ICU
  reordered_one_hosp_df$time_between_current_to_prev <- NA
  for (i in 2:nrow(reordered_one_hosp_df)){
    curr_ICU_ad_time <- reordered_one_hosp_df[i,"ICU_ADMIT_DATE"]
    prev_ICU_dc_time <-  reordered_one_hosp_df[i-1,"ICU_DISCHARGE_DATE"]
    reordered_one_hosp_df[i,"time_between_current_to_prev"] <- as.numeric(difftime(curr_ICU_ad_time,prev_ICU_dc_time,units = "hours"))
  }
  
  #Check all ICU between times starting from the 2nd ICU, if between time < 12,
  #then update the ICU end as the later time
  #If no one < 12, then just get the 1st ICU
  First_ICU_Start <- reordered_one_hosp_df[1,"ICU_ADMIT_DATE"]
  First_ICU_End   <- reordered_one_hosp_df[1,"ICU_DISCHARGE_DATE"]
  for (i in 2:nrow(reordered_one_hosp_df)){
    curr_between_time <- reordered_one_hosp_df[i,"time_between_current_to_prev"]
    if (curr_between_time < 12){
      First_ICU_End <- reordered_one_hosp_df[i,"ICU_DISCHARGE_DATE"]
    }
  }
  
  return(list(First_ICU_Start,First_ICU_End))
}


#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
taylor_UK_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features_candidates/uky/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/rawData_processed/uky/"


##########################################################################################
#Load Raw Data
##########################################################################################
#1.Load ADMISSION_INDX
raw_ADMISSION_INDX_df <- read.csv(paste0(raw_dir,"ADMISSION_INDX.csv"),stringsAsFactors = F)
#2.Refomart time
reformat_cols  <- c("HOSP_ADMIT_DATE","HOSP_DISCHARGE_DATE","ICU_ADMIT_DATE","ICU_DISCHARGE_DATE")
for (j in 1:length(reformat_cols)){
  curr_col_name <- reformat_cols[j]
  raw_ADMISSION_INDX_df[,curr_col_name] <- mdy_hm(raw_ADMISSION_INDX_df[,curr_col_name])
}

#3.remove duplicated on the four time column
raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[!duplicated(raw_ADMISSION_INDX_df[,reformat_cols]),]

#4.remove hosp start = hosp end
idx_hosp_equal_time <- which(raw_ADMISSION_INDX_df[,"HOSP_ADMIT_DATE"] == raw_ADMISSION_INDX_df[,"HOSP_DISCHARGE_DATE"])
if (length(idx_hosp_equal_time) > 0 ){
  raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[-idx_hosp_equal_time,]
}

#5. remove hosp start > hosp end
idx_hosp_endBeforeStart <- which(raw_ADMISSION_INDX_df[,"HOSP_ADMIT_DATE"] > raw_ADMISSION_INDX_df[,"HOSP_DISCHARGE_DATE"])
if (length(idx_hosp_endBeforeStart) > 0 ){
  raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[-idx_hosp_endBeforeStart,]
}


#6.remove ICU start = ICU end
idx_ICU_equal_time <- which(raw_ADMISSION_INDX_df[,"ICU_ADMIT_DATE"] == raw_ADMISSION_INDX_df[,"ICU_DISCHARGE_DATE"])
if (length(idx_ICU_equal_time) > 0 ){
  raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[-idx_ICU_equal_time,]
}

#7. remove ICU start > hosp end
idx_icu_endBeforeStart <- which(raw_ADMISSION_INDX_df[,"ICU_ADMIT_DATE"] > raw_ADMISSION_INDX_df[,"ICU_DISCHARGE_DATE"])
if (length(idx_icu_endBeforeStart) > 0 ){
  raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[-idx_icu_endBeforeStart,]
}


#8. remove NA HOSP Discharge
raw_ADMISSION_INDX_df <- raw_ADMISSION_INDX_df[-which(is.na(raw_ADMISSION_INDX_df[,"HOSP_DISCHARGE_DATE"])== T),]



##########################################################################################
#2.Load RENAL_REPLACE_THERAPY.csv
##########################################################################################
raw_RENAL_REPLACE_THERAPY_df <- read.csv(paste0(raw_dir,"RENAL_REPLACE_THERAPY.csv"),stringsAsFactors = F)
#2.Reformat time
reformat_cols  <- c("CRRT_START_DATE","CRRT_STOP_DATE","HD_START_DATE","HD_STOP_DATE")
for (j in 1:length(reformat_cols)){
  curr_col_name <- reformat_cols[j]
  raw_RENAL_REPLACE_THERAPY_df[,curr_col_name] <- ymd_hms(raw_RENAL_REPLACE_THERAPY_df[,curr_col_name])
}

#3.remove duplicated on the four time column
raw_RENAL_REPLACE_THERAPY_df <- raw_RENAL_REPLACE_THERAPY_df[!duplicated(raw_RENAL_REPLACE_THERAPY_df[,reformat_cols]),]

#4.get CRRT/HD start = CRRT/HD end IDs, exclude them from analysis ID, 
#because won't be able tocompute RRT time and can not make sure if on RRT during ICU
idx_CRRT_equal_time <- which(raw_RENAL_REPLACE_THERAPY_df[,"CRRT_START_DATE"] == raw_RENAL_REPLACE_THERAPY_df[,"CRRT_STOP_DATE"])
idx_CRRT_startBeforeEnd <- which(raw_RENAL_REPLACE_THERAPY_df[,"CRRT_START_DATE"] > raw_RENAL_REPLACE_THERAPY_df[,"CRRT_STOP_DATE"])
idx_HD_equal_time <- which(raw_RENAL_REPLACE_THERAPY_df[,"HD_START_DATE"] == raw_RENAL_REPLACE_THERAPY_df[,"HD_STOP_DATE"])
idx_HD_startBeforeEnd <- which(raw_RENAL_REPLACE_THERAPY_df[,"HD_START_DATE"] > raw_RENAL_REPLACE_THERAPY_df[,"HD_START_DATE"])

exclude_ID1 <- raw_RENAL_REPLACE_THERAPY_df[idx_CRRT_equal_time,"STUDY_PATIENT_ID"]
exclude_ID2 <- raw_RENAL_REPLACE_THERAPY_df[idx_CRRT_startBeforeEnd,"STUDY_PATIENT_ID"]
exclude_ID3 <- raw_RENAL_REPLACE_THERAPY_df[idx_HD_equal_time,"STUDY_PATIENT_ID"]
exclude_ID4 <- raw_RENAL_REPLACE_THERAPY_df[idx_HD_startBeforeEnd,"STUDY_PATIENT_ID"]

final_exclude <- unique(c(exclude_ID1,exclude_ID2,exclude_ID3,exclude_ID4))

##########################################################################################
#anlaysis Id for pts has hosp admition time
##########################################################################################
IDs_inADMISSION_df <- unique(raw_ADMISSION_INDX_df[,"STUDY_PATIENT_ID"])
analysis_ID <- IDs_inADMISSION_df[-which(IDs_inADMISSION_df %in% final_exclude)]

##########################################################################################
#                    Get time info  and DISCHARGE_DISPOSITION
#Features : HOSP dates, ICU dates , CRRT date, HD dates, HD treatments
#Note : 
#I. For multiple ICUs in one HOSP admission, 
#     1. If less than 12 hours between 2 ICU admissions, combine the two (use min ICU time as ICU start, max ICU imte as ICU stop)
#     2. otherwise, use the 1st ICU admission
#II. 
##########################################################################################
TimeInfo_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 11))
colnames(TimeInfo_df) <- c("STUDY_PATIENT_ID",
                           "HOSP_ADMIT_DATE",
                           "HOSP_DISCHARGE_DATE",
                           "First_ICU_ADMIT_DATE",
                           "First_ICU_DISCHARGE_DATE",
                           "CRRT_START_DATE",
                           "CRRT_STOP_DATE",
                           "HD_START_DATE",
                           "HD_STOP_DATE",
                           "HD_TREATMENTS",
                           "DISCHARGE_DISPOSITION")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  TimeInfo_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #curr admission indx df
  curr_df <- raw_ADMISSION_INDX_df[which(raw_ADMISSION_INDX_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Get HOSP time and ICU time
  #check if there is only one hosp admit
  n_hosp_admit <- length(unique(curr_df[,"HOSP_ADMIT_DATE"]))
  if (n_hosp_admit == 1){ #if only one hosp admit
    TimeInfo_df[i,"HOSP_ADMIT_DATE"]       <- as.character(unique(curr_df[,"HOSP_ADMIT_DATE"]))
    TimeInfo_df[i,"HOSP_DISCHARGE_DATE"]   <- as.character(unique(curr_df[,"HOSP_DISCHARGE_DATE"]))
    TimeInfo_df[i,"DISCHARGE_DISPOSITION"] <- unique(curr_df[,"DISCHARGE_DISPOSITION"])
     
    #check the number of ICU admit 
    n_icu_admits <- length(unique(curr_df[,"ICU_ADMIT_DATE"]))
    if (n_icu_admits > 1){ #if more than one ICU admits
      curr_ICU_res <- get_one_ICU_admission_func(curr_df)
      curr_1st_ICU_start <- curr_ICU_res[[1]]
      curr_1st_ICU_end <- curr_ICU_res[[2]]
    }else{
      curr_1st_ICU_start <- unique(curr_df[,"ICU_ADMIT_DATE"])
      curr_1st_ICU_end <- unique(curr_df[,"ICU_DISCHARGE_DATE"])
    }
    
    TimeInfo_df[i,"First_ICU_ADMIT_DATE"] <- as.character(curr_1st_ICU_start)
    TimeInfo_df[i,"First_ICU_DISCHARGE_DATE"] <- as.character(curr_1st_ICU_end)
  }
  
  #Get RRT time
  curr_rrt_df <- raw_RENAL_REPLACE_THERAPY_df[which(raw_RENAL_REPLACE_THERAPY_df[,"STUDY_PATIENT_ID"] == curr_id),]
  if (nrow(curr_rrt_df) > 0){
    TimeInfo_df[i,"CRRT_START_DATE"] <- as.character(curr_rrt_df[,"CRRT_START_DATE"])
    TimeInfo_df[i,"CRRT_STOP_DATE"]  <- as.character(curr_rrt_df[,"CRRT_STOP_DATE"])
    TimeInfo_df[i,"HD_START_DATE"]   <- as.character(curr_rrt_df[,"HD_START_DATE"])
    TimeInfo_df[i,"HD_STOP_DATE"]    <- as.character(curr_rrt_df[,"HD_STOP_DATE"])
    TimeInfo_df[i,"HD_TREATMENTS"]   <- curr_rrt_df[,"HD_TREATMENTS"]
  }
  
}

#Reforamt add 00:00:00
reformat_cols <- c( "HOSP_ADMIT_DATE","HOSP_DISCHARGE_DATE","First_ICU_ADMIT_DATE","First_ICU_DISCHARGE_DATE",
                    "CRRT_START_DATE","CRRT_STOP_DATE","HD_START_DATE","HD_STOP_DATE")
for (i in 1:length(reformat_cols)){
  curr_col <- TimeInfo_df[,reformat_cols[i]]
  curr_idxes_tochange <- which(nchar(curr_col) == 10)
  udpated_col <-  paste(curr_col[curr_idxes_tochange],"00:00:00")
  TimeInfo_df[curr_idxes_tochange,reformat_cols[i]] <- udpated_col
}

##########################################################################################
#Correction
##########################################################################################
#Compute ICU admit - hosp admit (supposed to be >= 0, ICU admit after hosp admit)
time1 <- ymd_hms(TimeInfo_df[,"First_ICU_ADMIT_DATE"])
time2 <- ymd_hms(TimeInfo_df[,"HOSP_ADMIT_DATE"])
TimeInfo_df$ICUAD_minus_HOSPAD_inDays <- difftime(time1,time2,units = "days") #ICU - HOSP

#Compute ICU discharge - hosp discharge (supposed to be <= 0, ICU discharge before hosp discharge)
time1 <- TimeInfo_df[,"First_ICU_DISCHARGE_DATE"]
time2 <- TimeInfo_df[,"HOSP_DISCHARGE_DATE"]
TimeInfo_df$ICUDC_minus_HOSPDC_inDays <- difftime(time1,time2,units = "days") #ICU - HOSP

##Correct admission time
#Initilization
TimeInfo_df$Corrected_HOSP_ADMIT_DATE <- TimeInfo_df[,"HOSP_ADMIT_DATE"]

#1.if ICU admit is more than 1 day before HOSP (ICU_admit - HOSP_admit < -1 days), exclude
idxes_ICUbeforeHOSP_morethan1 <- which(TimeInfo_df[,"ICUAD_minus_HOSPAD_inDays"] < -1) #8
TimeInfo_df <- TimeInfo_df[-idxes_ICUbeforeHOSP_morethan1,]

#2. if if ICU admit is less than 1 day before HOSP admit, 
#make the hosp and ICU  the same by setting hosp admit time = ICU admit time
idxes_ICUbeforeHOSP_lessthan1 <- which(TimeInfo_df[,"ICUAD_minus_HOSPAD_inDays"] >= -1 &
                                       TimeInfo_df[,"ICUAD_minus_HOSPAD_inDays"] < 0) #696
TimeInfo_df[idxes_ICUbeforeHOSP_lessthan1,"Corrected_HOSP_ADMIT_DATE"] <- TimeInfo_df[idxes_ICUbeforeHOSP_lessthan1,"First_ICU_ADMIT_DATE"]

##Correct discharge time
#Initilization
TimeInfo_df$Corrected_HOSP_DISCHARGE_DATE <- TimeInfo_df[,"HOSP_DISCHARGE_DATE"]

#1.if ICU discahrge is more than 1 day after HOSP discahrge (ICU_dc- HOSP_dc > 1 days), exclude
idxes_ICUafterHOSP_morethan1 <- which(TimeInfo_df[,"ICUDC_minus_HOSPDC_inDays"] > 1) #0
if (length(idxes_ICUafterHOSP_morethan1) > 0){
TimeInfo_df <- TimeInfo_df[-idxes_ICUafterHOSP_morethan1,]
}

#2.if if ICU discahrge is less than 1 day after HOSP discahrge, 
#make the hosp and ICU  the same by setting hosp discahrge time = ICU discahrge time
idxes_ICUafterHOSP_lessthan1 <- which(TimeInfo_df[,"ICUDC_minus_HOSPDC_inDays"] < 1 &
                                      TimeInfo_df[,"ICUDC_minus_HOSPDC_inDays"] > 0)  #0
if (length(idxes_ICUafterHOSP_lessthan1) > 0){
TimeInfo_df[idxes_ICUafterHOSP_lessthan1,"Corrected_HOSP_DISCHARGE_DATE"] <- TimeInfo_df[idxes_ICUafterHOSP_lessthan1,"First_ICU_DISCHARGE_DATE"]
}

#check
which(TimeInfo_df$Corrected_HOSP_ADMIT_DATE > TimeInfo_df$First_ICU_ADMIT_DATE)
which(TimeInfo_df$Corrected_HOSP_DISCHARGE_DATE < TimeInfo_df$First_ICU_DISCHARGE_DATE)


write.csv(TimeInfo_df,paste0(outdir,"All_Timeinfo_df.csv"),row.names = F)