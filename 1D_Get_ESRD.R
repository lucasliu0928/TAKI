#Dertmine if ESRD before, during and after HOSPITAL 
#1. Source 1: ESRD_USRDS.csv (Gold dataset, if not in USRD, use ESRD_STATUS.csv, then manual check
#2. Source 2: ESRD_STATUS.csv (Use DURING_INDEXED_INDICATOR for ESRD from ICU discharge to 120 days after,  Manual Check: During  == Y and after == N  (Supposed to be during = Y and after =Y))
library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#2. source 1: USRDS_ESRD
USRDS_ESRD_df <-read.csv(paste0(raw_dir,"USRDS_ESRD.csv"),stringsAsFactors = F)
USRDS_ESRD_df <- USRDS_ESRD_df[-which(USRDS_ESRD_df$ESRD_DATE==""),] #remove blanks
#reformat 
USRDS_ESRD_df$ESRD_DATE <- gsub("00:00:00","",USRDS_ESRD_df$ESRD_DATE)
USRDS_ESRD_df$ESRD_DATE <- gsub("0:00","",USRDS_ESRD_df$ESRD_DATE)
dash_idxes <- which(grepl("-",USRDS_ESRD_df$ESRD_DATE) == T)
USRDS_ESRD_df$ESRD_DATE[dash_idxes] <- as.character(ymd(USRDS_ESRD_df$ESRD_DATE[dash_idxes]))
slash_idxes <- which(grepl("/",USRDS_ESRD_df$ESRD_DATE) == T)
USRDS_ESRD_df$ESRD_DATE[slash_idxes] <- as.character(mdy(USRDS_ESRD_df$ESRD_DATE[slash_idxes]))

#3. source 2
ESRD_STATUS_df <-read.csv(paste0(raw_dir,"ESRD_STATUS.csv"),stringsAsFactors = F)
ESRD_STATUS_df[which(ESRD_STATUS_df == "",arr.ind = T)] <- NA
ESRD_STATUS_df[which(ESRD_STATUS_df == "Y",arr.ind = T)] <- 1



##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3. Process USRDS_ESRD.csv to get before, during and after HOSP
##########################################################################################
ESRD_Indicator_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 6))
colnames(ESRD_Indicator_df) <- c("STUDY_PATIENT_ID","ESRD_BEFORE","ESRD_AT","ESRD_DURING","ESRD_AFTER","SOURCE")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_Indicator_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd cuz esrd dates without hms
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  
  #USRD info
  curr_usrd_df <- USRDS_ESRD_df[which(USRDS_ESRD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_esrd_date <- ymd(curr_usrd_df$ESRD_DATE)
  if (nrow(curr_usrd_df) != 0){
    ESRD_Indicator_df[i,"SOURCE"]<- "inUSRD"
    if (curr_esrd_date > curr_hosp_start & curr_esrd_date <= curr_hosp_end){
      ESRD_Indicator_df[i,"ESRD_DURING"] <- 1
    }else if (curr_esrd_date > curr_hosp_end){
      ESRD_Indicator_df[i,"ESRD_AFTER"] <- 1
    }else if (curr_esrd_date < curr_hosp_start) {
      ESRD_Indicator_df[i,"ESRD_BEFORE"] <- 1
    }else if (curr_esrd_date == curr_hosp_start){
      ESRD_Indicator_df[i,"ESRD_AT"] <- 1
      
    }
  }else { #if not in USRD
    ESRD_Indicator_df[i,"ESRD_BEFORE"] <- NA
    ESRD_Indicator_df[i,"ESRD_AT"] <- NA
    ESRD_Indicator_df[i,"ESRD_DURING"] <- NA
    ESRD_Indicator_df[i,"ESRD_AFTER"] <- NA
    ESRD_Indicator_df[i,"SOURCE"]<- "notin_USRD"
  }
  

}

##########################################################################################
#4. Process ESRD_STATUS.csv to get esrd info for patients who are not in USRD_ESRD.csv
##########################################################################################
notinUSRD_indxes <- which(ESRD_Indicator_df$SOURCE != "inUSRD")

ESRD_Indicator_df$SOURCE2 <- NA
for (i in 1:length(notinUSRD_indxes)){
  if (i %% 1000 == 0) {print(i)}
  curr_idx <- notinUSRD_indxes[i]
  curr_id <-  ESRD_Indicator_df[curr_idx,"STUDY_PATIENT_ID"]
  
  #source 2:
  curr_ESRD_STATUS_df <- ESRD_STATUS_df[which(ESRD_STATUS_df[,"STUDY_PATIENT_ID"] == curr_id),]
  if (nrow(curr_ESRD_STATUS_df) != 0){
    ESRD_Indicator_df[curr_idx,"SOURCE2"]<- "in_STATUS_Table"
    ESRD_Indicator_df[curr_idx,"ESRD_BEFORE"] <- curr_ESRD_STATUS_df[,"BEFORE_INDEXED_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_AT"] <- curr_ESRD_STATUS_df[,"AT_ADMISSION_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_DURING"] <- curr_ESRD_STATUS_df[,"DURING_INDEXED_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_AFTER"] <- curr_ESRD_STATUS_df[,"AFTER_INDEXED_INDICATOR"]
  }else{
    ESRD_Indicator_df[curr_idx,"SOURCE2"]<- "notin_STATUS_Table"
  }
}

write.csv(ESRD_Indicator_df,paste0(outdir,"ESRD.csv"),row.names=FALSE)

check <- ESRD_Indicator_df[which(ESRD_Indicator_df$SOURCE == "inUSRD" & ESRD_Indicator_df$ESRD_AT == 1),]
