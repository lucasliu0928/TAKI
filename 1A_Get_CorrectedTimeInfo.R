library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"


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

#7. remove ICU start > ICU end
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
#because won't be able to compute RRT time and can not make sure if on RRT during ICU
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
#Check if patients has ICU, HOSP,RRT dates before correction
#1: Has the corresponidng dates
#0: No date
##########################################################################################
has_date_df <- check_has_dates(TimeInfo_df)
table(has_date_df$HAS_HOSP)
table(has_date_df$HAS_ICU)
table(has_date_df$HAS_CRRT)
table(has_date_df$HAS_HD)

##########################################################################################
#Check if patients has Valid ICU, HOSP,RRT dates before correction 
# if CRRT,HD has overlap
# If CRRT, HD and ICU in HOSP with 0 hours to extend (exact in)
# if CRRT in ICU  with 0 hours to extend (exact in)
##########################################################################################
# valid_dates_df <- check_valid_dates(TimeInfo_df,0)
# table(valid_dates_df$CRRT_inHOSP)

##########################################################################################
#'Time correction for overlapped CRRT and HD dates for patients has both dates
##########################################################################################
has_bothRRT_idxes <- which(has_date_df$HAS_CRRT == 1 & has_date_df$HAS_HD == 1)
has_bothRRT_IDs <- has_date_df[has_bothRRT_idxes, "STUDY_PATIENT_ID"]

corrected_CRRTHD_df <- as.data.frame(matrix(NA, nrow = length(has_bothRRT_IDs), ncol = 6))
colnames(corrected_CRRTHD_df) <- c("STUDY_PATIENT_ID", "Updated_CRRT_Start","Updated_CRRT_End",
                                   "Updated_HD_Start","Updated_HD_End","Overlap_Flag")
for (i in 1:length(has_bothRRT_IDs)){
  if (i %% 100 == 0){print(i)}
  curr_id <- has_bothRRT_IDs[i]
  
  #Time df
  curr_time_df  <- TimeInfo_df[which(TimeInfo_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_CRRT_Start <- curr_time_df[ ,"CRRT_START_DATE"]
  curr_CRRT_End <- curr_time_df[ ,"CRRT_STOP_DATE"]
  curr_HD_Start <- curr_time_df[ ,"HD_START_DATE"]
  curr_HD_End <- curr_time_df[ ,"HD_STOP_DATE"]
  
  
  res <- correct_overlap_CRRTHD(curr_CRRT_Start,curr_CRRT_End,curr_HD_Start,curr_HD_End)

  corrected_CRRTHD_df[i,"STUDY_PATIENT_ID"] <- curr_id
  corrected_CRRTHD_df[i,"Updated_CRRT_Start"] <- as.character(res[,1])
  corrected_CRRTHD_df[i,"Updated_CRRT_End"] <- as.character(res[,2])
  corrected_CRRTHD_df[i,"Updated_HD_Start"] <-as.character(res[,3])
  corrected_CRRTHD_df[i,"Updated_HD_End"] <- as.character(res[,4])
  corrected_CRRTHD_df[i,"Overlap_Flag"] <- as.character(res[,5])
  
  
  
}

table(corrected_CRRTHD_df$Overlap_Flag)

#Store corrected dates to time df
TimeInfo_df$Updated_CRRT_Start <- NA
TimeInfo_df$Updated_CRRT_End <- NA
TimeInfo_df$Updated_HD_Start <- NA
TimeInfo_df$Updated_HD_End <- NA
TimeInfo_df$Overlap_Flag <- NA

for (i in 1:nrow(TimeInfo_df)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- TimeInfo_df[i,"STUDY_PATIENT_ID"]
  
  curr_idxes_inCorrected <- which(corrected_CRRTHD_df[,"STUDY_PATIENT_ID"] == curr_id)
  if (length(curr_idxes_inCorrected) == 0 ){ #if has not been corrected
    TimeInfo_df[i,"Updated_CRRT_Start"] <- TimeInfo_df[i,"CRRT_START_DATE"]
    TimeInfo_df[i,"Updated_CRRT_End"] <- TimeInfo_df[i,"CRRT_STOP_DATE"]
    TimeInfo_df[i,"Updated_HD_Start"] <- TimeInfo_df[i,"HD_START_DATE"]
    TimeInfo_df[i,"Updated_HD_End"] <- TimeInfo_df[i,"HD_STOP_DATE"]
    TimeInfo_df[i,"Overlap_Flag"] <- NA
  }else {
    TimeInfo_df[i,"Updated_CRRT_Start"] <- corrected_CRRTHD_df[curr_idxes_inCorrected,"Updated_CRRT_Start"]
    TimeInfo_df[i,"Updated_CRRT_End"] <- corrected_CRRTHD_df[curr_idxes_inCorrected,"Updated_CRRT_End"]
    TimeInfo_df[i,"Updated_HD_Start"] <- corrected_CRRTHD_df[curr_idxes_inCorrected,"Updated_HD_Start"]
    TimeInfo_df[i,"Updated_HD_End"] <- corrected_CRRTHD_df[curr_idxes_inCorrected,"Updated_HD_End"]
    TimeInfo_df[i,"Overlap_Flag"] <- corrected_CRRTHD_df[curr_idxes_inCorrected,"Overlap_Flag"]
  } 

}


#Drop old columns
TimeInfo_df <- TimeInfo_df[,-which(colnames(TimeInfo_df) %in% c("CRRT_START_DATE","CRRT_STOP_DATE","HD_START_DATE", "HD_STOP_DATE"))]



##########################################################################################
#Correction of CRRT,HD, ICU, HOSP
#1. If CRRT start is before ICU start, but less than 24 hours, set ICU start = CRRT Start; otherwise, exclude pts
#   If CRRT End is after ICU end, but less than 24 hours, set ICU End = CRRT End; otherwise, exclude 

#2. If min(CRRT/HD/ICU start) is before HOSP start, but less than 24 hours, set HOSP start = min(CRRT/HD/ICU start) ; otherwise, exclude pts
#   If max(CRRT/HD/ICU end) is after HOSP end, but less than 24 hours, set HOSP End = max(CRRT/HD/ICU end) ; otherwise, exclude 

##########################################################################################
#1.Corretion of ICU to cover CRRT, Need to correct ICU first , then correct hosp
TimeInfo_df$Updated_ICU_ADMIT_DATE <- NA
TimeInfo_df$Updated_ICU_DISCHARGE_DATE <- NA
TimeInfo_df$Correction_Flag_Start <- NA
TimeInfo_df$Correction_Flag_End <- NA

for (p in 1:nrow(TimeInfo_df)){
  if(p %% 1000==0) {
    # Print on the screen some message
    cat(paste0("iteration: ", p, "\n"))
  }
  curr_id <- TimeInfo_df[p, "STUDY_PATIENT_ID"]
  curr_time_df  <- TimeInfo_df[p, ]
  
  CRRT_Start <- curr_time_df[ ,"Updated_CRRT_Start"]
  CRRT_End <- curr_time_df[ ,"Updated_CRRT_End"]
  HD_Start <- curr_time_df[ ,"Updated_HD_Start"]
  HD_End <- curr_time_df[ ,"Updated_HD_End"]
  ICU_Start <- curr_time_df[ ,"First_ICU_ADMIT_DATE"]
  ICU_End <- curr_time_df[ ,"First_ICU_DISCHARGE_DATE"]
  HOSP_Start <- curr_time_df[,"HOSP_ADMIT_DATE"]
  HOSP_End <- curr_time_df[,"HOSP_DISCHARGE_DATE"]
  
 
  if (is.na(CRRT_Start) == T){ #if not on CRRT, then no correction of ICU
    TimeInfo_df[p,"Updated_ICU_ADMIT_DATE"] <- ICU_Start
    TimeInfo_df[p,"Updated_ICU_DISCHARGE_DATE"] <- ICU_End
  }else if (is.na(CRRT_Start) == F & is.na(ICU_Start) == F){  #If on CRRT, check iF CRRT in ICU +- 24 hours
    CRRT_ICU_Start_Flag <- check_T1start_T2start_status(CRRT_Start,ICU_Start)
    if (CRRT_ICU_Start_Flag == "T1 starts 24h before T2 starts"){
      TimeInfo_df[p,"Correction_Flag_Start"] <- "Exclude"
    }else if (CRRT_ICU_Start_Flag == "T1 starts before T2 starts But Less Than/equals 24h"){
      TimeInfo_df[p,"Correction_Flag_Start"] <- "Correct"
      TimeInfo_df[p,"Updated_ICU_ADMIT_DATE"] <- CRRT_Start 
    }else{
      TimeInfo_df[p,"Correction_Flag_Start"] <- "Original"
      TimeInfo_df[p,"Updated_ICU_ADMIT_DATE"] <- ICU_Start #no correction
    }
    
    
    CRRT_ICU_End_Flag <- check_T1end_T2end_status(CRRT_End,ICU_End)
    if (CRRT_ICU_End_Flag == "T1 ends 24h after T2 ends"){
      TimeInfo_df[p,"Correction_Flag_End"] <- "Exclude"
      
    }else if(CRRT_ICU_End_Flag == "T1 ends after T2 ends But Less Than/equals 24h") {
      TimeInfo_df[p,"Correction_Flag_End"] <- "Correct"
      TimeInfo_df[p,"Updated_ICU_DISCHARGE_DATE"] <- CRRT_End
      
    }else{
      TimeInfo_df[p,"Correction_Flag_End"] <- "Original"
      TimeInfo_df[p,"Updated_ICU_DISCHARGE_DATE"] <- ICU_End  #no correction
    }
  }
  
}

#Exclude patients with any one of the start/end flag is exclude
toexclude_idxes1 <- which(TimeInfo_df$Correction_Flag_Start == "Exclude")
toexclude_idxes2 <- which(TimeInfo_df$Correction_Flag_End == "Exclude")
exclude_idxes_all <- unique(c(toexclude_idxes1,toexclude_idxes2))
TimeInfo_df <- TimeInfo_df[-exclude_idxes_all,]

#Drop old columns
TimeInfo_df <- TimeInfo_df[,-which(colnames(TimeInfo_df) %in% c("First_ICU_ADMIT_DATE","First_ICU_DISCHARGE_DATE","Overlap_Flag","Correction_Flag_Start","Correction_Flag_End"))]

##2. Corretion of HOSP to cover ICU/HD/CRRT
TimeInfo_df$Updated_HOSP_ADMIT_DATE <- NA
TimeInfo_df$Updated_HOSP_DISCHARGE_DATE <- NA
TimeInfo_df$Correction_Flag_Start <- NA
TimeInfo_df$Correction_Flag_End <- NA

for (p in 1:nrow(TimeInfo_df)){
  if(p %% 1000==0) {
    # Print on the screen some message
    cat(paste0("iteration: ", p, "\n"))
  }
  curr_id <- TimeInfo_df[p, "STUDY_PATIENT_ID"]
  curr_time_df  <- TimeInfo_df[p, ]
  
  CRRT_Start <- curr_time_df[ ,"Updated_CRRT_Start"]
  CRRT_End <- curr_time_df[ ,"Updated_CRRT_End"]
  HD_Start <- curr_time_df[ ,"Updated_HD_Start"]
  HD_End <- curr_time_df[ ,"Updated_HD_End"]
  ICU_Start <- curr_time_df[ ,"Updated_ICU_ADMIT_DATE"]
  ICU_End <- curr_time_df[ ,"Updated_ICU_DISCHARGE_DATE"]
  HOSP_Start <- curr_time_df[,"HOSP_ADMIT_DATE"]
  HOSP_End <- curr_time_df[,"HOSP_DISCHARGE_DATE"]
  
  #Minimun start time of CRRT ,HD , ICU
  min_start <- min(c(ymd_hms(CRRT_Start),ymd_hms(HD_Start),ymd_hms(ICU_Start)),na.rm = T)
  min_start <- reformat_10char_dates_func(min_start)
  #Maximum end time of CRRT ,HD , ICU
  max_End <-   max(c(ymd_hms(CRRT_End),ymd_hms(HD_End),ymd_hms(ICU_End)),na.rm = T)
  max_End <- reformat_10char_dates_func(max_End)

  Start_Flag <- check_T1start_T2start_status(min_start,HOSP_Start)
  
  if (Start_Flag == "T1 starts 24h before T2 starts"){
    TimeInfo_df[p,"Correction_Flag_Start"] <- "Exclude"
  }else if (Start_Flag == "T1 starts before T2 starts But Less Than/equals 24h"){
    TimeInfo_df[p,"Correction_Flag_Start"] <- "Correct"
    TimeInfo_df[p,"Updated_HOSP_ADMIT_DATE"] <- min_start 
  }else{
    TimeInfo_df[p,"Correction_Flag_Start"] <- "Original"
    TimeInfo_df[p,"Updated_HOSP_ADMIT_DATE"] <- HOSP_Start #no correction
  }
    
  End_Flag <- check_T1end_T2end_status(max_End,HOSP_End)
  
  if (End_Flag == "T1 ends 24h after T2 ends"){
    TimeInfo_df[p,"Correction_Flag_End"] <- "Exclude"
    
  }else if(End_Flag == "T1 ends after T2 ends But Less Than/equals 24h") {
    TimeInfo_df[p,"Correction_Flag_End"] <- "Correct"
    TimeInfo_df[p,"Updated_HOSP_DISCHARGE_DATE"] <- max_End
    
  }else{
    TimeInfo_df[p,"Correction_Flag_End"] <- "Original"
    TimeInfo_df[p,"Updated_HOSP_DISCHARGE_DATE"] <-  HOSP_End #no correction
  }

  
}

#Exclude patients with any one of the start/end flag is exclude
toexclude_idxes1 <- which(TimeInfo_df$Correction_Flag_Start == "Exclude")
toexclude_idxes2 <- which(TimeInfo_df$Correction_Flag_End == "Exclude")
exclude_idxes_all <- unique(c(toexclude_idxes1,toexclude_idxes2))
TimeInfo_df <- TimeInfo_df[-exclude_idxes_all,]

#Drop old columns
TimeInfo_df <- TimeInfo_df[,-which(colnames(TimeInfo_df) %in% c("HOSP_ADMIT_DATE","HOSP_DISCHARGE_DATE", "Correction_Flag_Start","Correction_Flag_End"))]

##########################################################################################
# Add decease date IN TimeInfo_df
##########################################################################################
#Mortality time df 
outcome_df <-read.csv(paste0(raw_dir,"OUTCOMES_COMBINED.csv"),stringsAsFactors = F)
death_df <- outcome_df[which(outcome_df$DECEASED_DATE!=""),] #so this df only contains pts who died
death_df <- death_df[!duplicated(death_df[,c("STUDY_PATIENT_ID")]),]

TimeInfo_df$DECEASED_DATE <- NA
for (i in 1: nrow(TimeInfo_df)){
  curr_id <- TimeInfo_df[i, "STUDY_PATIENT_ID"]
  curr_death_df <- death_df[which(death_df[,"STUDY_PATIENT_ID"] ==curr_id),]
  if (nrow(curr_death_df) == 0){
    TimeInfo_df[i,"DECEASED_DATE"] <- NA
  }else{
    TimeInfo_df[i,"DECEASED_DATE"] <- curr_death_df$DECEASED_DATE
  }
}

#exclude patients died before HOSP admit 
exclude_idxes <- which(mdy(TimeInfo_df$DECEASED_DATE) < ymd_hms(TimeInfo_df$Updated_HOSP_ADMIT_DATE))
updated_TimeInfo_df <- TimeInfo_df[-exclude_idxes,]



##########################################################################################
#Check valid date after correction
##########################################################################################
#1.Check if patients has if ICU/CRRT/HD in HOSP, and if CRRT in ICU after correction 
check_df <- updated_TimeInfo_df
colnames(check_df)[which(colnames(check_df) %in% c("Updated_CRRT_Start","Updated_CRRT_End",
                                                   "Updated_HD_Start","Updated_HD_End","Updated_ICU_ADMIT_DATE",
                                                   "Updated_ICU_DISCHARGE_DATE",
                                                   "Updated_HOSP_ADMIT_DATE","Updated_HOSP_DISCHARGE_DATE"))] <- c("CRRT_START_DATE","CRRT_STOP_DATE","HD_START_DATE","HD_STOP_DATE",
                                                                                                                   "First_ICU_ADMIT_DATE","First_ICU_DISCHARGE_DATE","HOSP_ADMIT_DATE","HOSP_DISCHARGE_DATE")
valid_dates_df <- check_valid_dates(check_df,0)
table(valid_dates_df$CRRT_inICU)
table(valid_dates_df$CRRT_inHOSP)
table(valid_dates_df$HD_inHOSP)
table(valid_dates_df$ICU_inHOSP)
table(valid_dates_df$CRRT_HD_Overlap)


#2.check if ICU/RRT/HOSP end > start
which(check_df$Updated_CRRT_Start >= check_df$Updated_CRRT_End)
which(check_df$Updated_HD_Start >= check_df$Updated_HD_End)
which(check_df$Updated_ICU_ADMIT_DATE >= check_df$Updated_ICU_DISCHARGE_DATE)
which(check_df$Updated_HOSP_ADMIT_DATE >= check_df$Updated_HOSP_DISCHARGE_DATE)

write.csv(updated_TimeInfo_df,paste0(outdir,"All_Corrected_Timeinfo_V1.csv"),row.names = F)


##########################################################################################
#### Add 1. D0-D3 start and end time, (This is used only for exclusion criterion death in D0-D3, where D3 can be not outside ICU)
####     2. D0-D3 Start actual hours for each day, and actual days in D0,D1,D2,D3 (This will be used for features and other criterion related to the features)

#### D0 Start  == ICU start,      D0 End == the same day of D0 start at 23:59:59
#### D1 Start  == D0 end + 1 sec, D1 End == the same day of D1 start at 23:59:59
#### D2 Start  == D1 end + 1 sec, D2 End == the same day of D2 start at 23:59:59
#### D3 Start  == D2 end + 1 sec, D3 End == the same day of D3 start at 23:59:59
#'@NOTE: D1 or D2 or D3 can be no in ICU, if the duration of ICU is less than certain hours
##########################################################################################
updated_TimeInfo_df$D0_Start <- NA
updated_TimeInfo_df$D0_End <-  NA
updated_TimeInfo_df$D1_Start <- NA
updated_TimeInfo_df$D1_End <- NA
updated_TimeInfo_df$D2_Start <- NA
updated_TimeInfo_df$D2_End <- NA
updated_TimeInfo_df$D3_Start <- NA
updated_TimeInfo_df$D3_End <-NA

updated_TimeInfo_df$Actual_D0_Start <- NA
updated_TimeInfo_df$Actual_D0_End <-  NA
updated_TimeInfo_df$Actual_D1_Start <- NA
updated_TimeInfo_df$Actual_D1_End <- NA
updated_TimeInfo_df$Actual_D2_Start <- NA
updated_TimeInfo_df$Actual_D2_End <- NA
updated_TimeInfo_df$Actual_D3_Start <- NA
updated_TimeInfo_df$Actual_D3_End <-NA
updated_TimeInfo_df$Actual_D0_ICUHours <- NA
updated_TimeInfo_df$Actual_D1_ICUHours <- NA
updated_TimeInfo_df$Actual_D2_ICUHours <- NA
updated_TimeInfo_df$Actual_D3_ICUHours <- NA
updated_TimeInfo_df$Actual_ICU_Stays <- NA
for (i in 1:nrow(updated_TimeInfo_df)){
  if (i %% 1000 ==0){print(i)}
  
  #Time info
  curr_time_df <- updated_TimeInfo_df[i,]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end   <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  
  #Get ICU D0 to D3 start and end time
  curr_D0D3_df <- get_D0toD3_dates_func(curr_icu_start,curr_icu_end)
  
  updated_TimeInfo_df$D0_Start[i] <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 0 ),"Day_start"]
  updated_TimeInfo_df$D0_End[i]   <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 0 ),"Day_End"]
  updated_TimeInfo_df$D1_Start[i] <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 1 ),"Day_start"]
  updated_TimeInfo_df$D1_End[i]   <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 1 ),"Day_End"]
  updated_TimeInfo_df$D2_Start[i] <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 2 ),"Day_start"]
  updated_TimeInfo_df$D2_End[i]   <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 2 ),"Day_End"]
  updated_TimeInfo_df$D3_Start[i] <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 3 ),"Day_start"]
  updated_TimeInfo_df$D3_End[i]   <- curr_D0D3_df[which(curr_D0D3_df[,"Day"] == 3 ),"Day_End"]
  
  #Get the actual hours for each day 
  curr_filtered_D0D3_df <- compute_actualhours_EachDay(curr_D0D3_df,curr_icu_end)
  updated_TimeInfo_df$Actual_D0_Start[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 0 ),"Day_start"]
  updated_TimeInfo_df$Actual_D0_End[i]   <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 0 ),"Day_End"]
  updated_TimeInfo_df$Actual_D1_Start[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 1 ),"Day_start"]
  updated_TimeInfo_df$Actual_D1_End[i]   <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 1 ),"Day_End"]
  updated_TimeInfo_df$Actual_D2_Start[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 2 ),"Day_start"]
  updated_TimeInfo_df$Actual_D2_End[i]   <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 2 ),"Day_End"]
  updated_TimeInfo_df$Actual_D3_Start[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 3 ),"Day_start"]
  updated_TimeInfo_df$Actual_D3_End[i]   <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 3 ),"Day_End"]
  
  updated_TimeInfo_df$Actual_D0_ICUHours[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 0 ),"Hours_InOneDay"]
  updated_TimeInfo_df$Actual_D1_ICUHours[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 1 ),"Hours_InOneDay"]
  updated_TimeInfo_df$Actual_D2_ICUHours[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 2 ),"Hours_InOneDay"]
  updated_TimeInfo_df$Actual_D3_ICUHours[i] <- curr_filtered_D0D3_df[which(curr_filtered_D0D3_df[,"Day"] == 3 ),"Hours_InOneDay"]
  
  #Get the actual stayed days D0,D1,D2,D3
  updated_TimeInfo_df$Actual_ICU_Stays[i] <- get_acutal_Days_inICU(curr_filtered_D0D3_df)
}


write.csv(updated_TimeInfo_df,paste0(outdir,"All_Corrected_Timeinfo.csv"),row.names = F)

##########################################################################################
#### Add if on RRT last 48 hours before discharge
##########################################################################################
updated_TimeInfo_df$onRRT_Flag <- 0
updated_TimeInfo_df$onRRT_Last48hBeforeDischarge <- 0

for (i in 1:nrow(updated_TimeInfo_df)){
  if (i %% 1000 == 0) {print(i)}
  curr_time_df <- updated_TimeInfo_df[i,]
  
  #on RRT last 48 hours before HOSP discharge
  curr_hosp_end <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  curr_hd_end  <- ymd_hms(curr_time_df[,"Updated_HD_End"])
  curr_crrt_end <- ymd_hms(curr_time_df[,"Updated_CRRT_End"])
  
  if (is.na(curr_hd_end) == F | is.na(curr_crrt_end) == F){ #if on CRRT or HD
    updated_TimeInfo_df[i,"onRRT_Flag"] <- 1
    curr_max_RRT_Endtime <- max(c(curr_hd_end,curr_crrt_end), na.rm = T)
    
    if (curr_max_RRT_Endtime  >= (curr_hosp_end - hours(48))){
      updated_TimeInfo_df[i,"onRRT_Last48hBeforeDischarge"] <- 1
    }
  }
}

table(updated_TimeInfo_df$onRRT_Flag) #34198  2291
table(updated_TimeInfo_df$onRRT_Last48hBeforeDischarge) #34812  1677

write.csv(updated_TimeInfo_df,paste0(outdir,"All_Corrected_Timeinfo.csv"),row.names = F)

##########################################################################################
#### Add Acutal ICU hours in D0-D3
##########################################################################################
updated_TimeInfo_df$Actual_ICUHours_D0toD3 <- NA
for (i in 1:nrow(updated_TimeInfo_df)){
  if (i %% 1000 == 0) {print(i)}
  curr_time_df <- updated_TimeInfo_df[i,]
  curr_total_hours_inICU <- sum(curr_time_df[,c("Actual_D0_ICUHours", "Actual_D1_ICUHours","Actual_D2_ICUHours", "Actual_D3_ICUHours")])
  updated_TimeInfo_df[i,"Actual_ICUHours_D0toD3"] <- curr_total_hours_inICU
}

write.csv(updated_TimeInfo_df,paste0(outdir,"All_Corrected_Timeinfo.csv"),row.names = F)
