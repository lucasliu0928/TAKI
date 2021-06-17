#Dertmine if ESRD before, during and after HOSPITAL 
#1. Source 1: ESRD_USRDS.csv (Gold dataset, if not in USRD, use ESRD_STATUS.csv, then manual check
#2. Source 2: ESRD_STATUS.csv (Use DURING_INDEXED_INDICATOR for ESRD from ICU discharge to 120 days after,  Manual Check: During  == Y and after == N  (Supposed to be during = Y and after =Y))
library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"


##########################################################################################
#1. Analysis Id before exclusion of ESRD
##########################################################################################
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"])


##########################################################################################
#1. Corrected Time df 
##########################################################################################
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df$onRRT_Flag <- 0
All_time_df$onRRT_Last48hBeforeDischarge <- 0

for (i in 1:nrow(All_time_df)){
  if (i %% 1000 == 0) {print(i)}
  curr_time_df <- All_time_df[i,]

  #on RRT last 48 hours before HOSP discharge
  curr_hosp_end <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  curr_hd_end  <- ymd_hms(curr_time_df[,"Updated_HD_End"])
  curr_crrt_end <- ymd_hms(curr_time_df[,"Updated_CRRT_End"])
  
  if (is.na(curr_hd_end) == F | is.na(curr_crrt_end) == F){ #if one of them is not NA
    All_time_df[i,"onRRT_Flag"] <- 1
    curr_max_RRT_Endtime <- max(c(curr_hd_end,curr_crrt_end), na.rm = T)
    
    if (curr_max_RRT_Endtime  >= curr_hosp_end - hours(48)){
      All_time_df[i,"onRRT_Last48hBeforeDischarge"] <- 1
    }
  }
}

table(All_time_df$onRRT_Flag)
table(All_time_df$onRRT_Last48hBeforeDischarge)

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
ESRD_STATUS_df[which(ESRD_STATUS_df == "",arr.ind = T)] <- 0
ESRD_STATUS_df[which(ESRD_STATUS_df == "Y",arr.ind = T)] <- 1


############################################################################################################
#3. Process USRDS_ESRD.csv to get during and after HOSP, and within 120 following HOSP discharge
#Use "During"==1 |  "ESRD_ICU120"==1 for ESRD outcome at 120 days (This is equailtant to do curr_esrd_date > curr_hosp_start & curr_esrd_date <= curr_icu_end_plus120)
#USe "AT" | "Before" for exclusion 
############################################################################################################
ESRD_Indicator_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 8))
colnames(ESRD_Indicator_df) <- c("STUDY_PATIENT_ID","ESRD_BEFORE_HOSP","ESRD_AT_HOSPADMIT","ESRD_DURING_HOSP","ESRD_AFTER_HOSP","ESRD_ICU120","SOURCE","ESRD_Outcome_USRDS2ndWay")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_Indicator_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd cuz esrd dates has no hms
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  curr_icu_end   <- ymd(strsplit(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"],split = " ")[[1]][1])
  curr_icu_end_plus120 <- curr_icu_end + days(120)
  
  
  #USRD info
  curr_usrd_df <- USRDS_ESRD_df[which(USRDS_ESRD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_esrd_date <- ymd(curr_usrd_df$ESRD_DATE)
  if (nrow(curr_usrd_df) != 0){
    ESRD_Indicator_df[i,"SOURCE"]<- "inUSRD"
    if (curr_esrd_date > curr_hosp_start & curr_esrd_date <= curr_hosp_end){
      ESRD_Indicator_df[i,"ESRD_DURING_HOSP"] <- 1
    }else if (curr_esrd_date > curr_hosp_end){
      ESRD_Indicator_df[i,"ESRD_AFTER_HOSP"] <- 1
    }else if (curr_esrd_date < curr_hosp_start) {
      ESRD_Indicator_df[i,"ESRD_BEFORE_HOSP"] <- 1
    }else if (curr_esrd_date == curr_hosp_start){
      ESRD_Indicator_df[i,"ESRD_AT_HOSPADMIT"] <- 1
    }
    
    #another way for outcome
    if (curr_esrd_date > curr_hosp_start & curr_esrd_date <= curr_icu_end_plus120){
      ESRD_Indicator_df[i,"ESRD_Outcome_USRDS2ndWay"] <- 1
    }
    #within 120 days following ICU discharge
    if (curr_esrd_date > curr_icu_end & curr_esrd_date <= curr_icu_end_plus120){
      ESRD_Indicator_df[i,"ESRD_ICU120"] <- 1
    }
  }else { #if not in USRD
    ESRD_Indicator_df[i,"ESRD_BEFORE_HOSP"] <- NA
    ESRD_Indicator_df[i,"ESRD_AT_HOSPADMIT"] <- NA
    ESRD_Indicator_df[i,"ESRD_DURING_HOSP"] <- NA
    ESRD_Indicator_df[i,"ESRD_AFTER_HOSP"] <- NA
    ESRD_Indicator_df[i,"ESRD_ICU120"] <- NA
    ESRD_Indicator_df[i,"SOURCE"]<- "notin_USRD"
  }
  

}

##########################################################################################
#4. Process ESRD_STATUS.csv to get esrd info for patients who are not in USRD_ESRD.csv
#'@TODO: Use "During"==1 | Question: "After" == 1?
#USe "AT" | "Before" for exclusion 
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
    ESRD_Indicator_df[curr_idx,"ESRD_BEFORE_HOSP"] <- curr_ESRD_STATUS_df[,"BEFORE_INDEXED_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_AT_HOSPADMIT"] <- curr_ESRD_STATUS_df[,"AT_ADMISSION_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_DURING_HOSP"] <- curr_ESRD_STATUS_df[,"DURING_INDEXED_INDICATOR"]
    ESRD_Indicator_df[curr_idx,"ESRD_AFTER_HOSP"] <- curr_ESRD_STATUS_df[,"AFTER_INDEXED_INDICATOR"]
  }else{
    ESRD_Indicator_df[curr_idx,"SOURCE2"]<- "notin_STATUS_Table"
  }
}


##########################################################################################
#5. Add dates for check
##########################################################################################
ESRD_Indicator_df$Updated_HOSP_ADMIT_DATE <- NA
ESRD_Indicator_df$Updated_HOSP_DISCHARGE_DATE <- NA
ESRD_Indicator_df$Updated_ICU_ADMIT_DATE <- NA
ESRD_Indicator_df$Updated_ICU_DISCHARGE_DATE <- NA
ESRD_Indicator_df$Updated_CRRT_Start <- NA
ESRD_Indicator_df$Updated_CRRT_End <- NA
ESRD_Indicator_df$Updated_HD_Start <- NA
ESRD_Indicator_df$Updated_HD_End <- NA
ESRD_Indicator_df$ICU_DC_PLUS120Days <- NA
ESRD_Indicator_df$onRRT_Last48hBeforeDischarge <- NA


for (i in 1:nrow(ESRD_Indicator_df)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- ESRD_Indicator_df[i,"STUDY_PATIENT_ID"]
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  ESRD_Indicator_df[i,"Updated_HOSP_ADMIT_DATE"]  <- curr_time_df[,"Updated_HOSP_ADMIT_DATE"] 
  ESRD_Indicator_df[i,"Updated_HOSP_DISCHARGE_DATE"]    <- curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"]
  ESRD_Indicator_df[i,"Updated_ICU_ADMIT_DATE"]    <-  curr_time_df[,"Updated_ICU_ADMIT_DATE"]
  ESRD_Indicator_df[i,"Updated_ICU_DISCHARGE_DATE"]    <-  curr_time_df[,"Updated_ICU_DISCHARGE_DATE"]
  
  ESRD_Indicator_df[i,"Updated_CRRT_Start"]   <-  curr_time_df[,"Updated_CRRT_Start"]
  ESRD_Indicator_df[i,"Updated_CRRT_End"]   <-  curr_time_df[,"Updated_CRRT_End"]
  
  ESRD_Indicator_df[i,"Updated_HD_Start"]   <-  curr_time_df[,"Updated_HD_Start"]
  ESRD_Indicator_df[i,"Updated_HD_End"]   <-  curr_time_df[,"Updated_HD_End"]
  
  ESRD_Indicator_df[i,"ICU_DC_PLUS120Days"] <- as.character(ymd_hms(ESRD_Indicator_df[i,"Updated_ICU_DISCHARGE_DATE"]) + days(120))
  
  #on RRT last 48 hours before HOSP discharge
  curr_hosp_end <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  curr_hd_end  <- ymd_hms(curr_time_df[,"Updated_HD_End"])
  curr_crrt_end <- ymd_hms(curr_time_df[,"Updated_CRRT_End"])
  if (is.na(curr_hd_end) == F | is.na(curr_crrt_end) == F){ #if one of them is not NA
      curr_max_RRT_Endtime <- max(c(curr_hd_end,curr_crrt_end), na.rm = T)
      
      if (curr_max_RRT_Endtime  >= curr_hosp_end - hours(48)){
        ESRD_Indicator_df[i,"onRRT_Last48hBeforeDischarge"] <- 1
      }
  }
  
}

write.csv(ESRD_Indicator_df,paste0(outdir,"ESRD.csv"),row.names=FALSE)


##########################################################################################
# Check ESRD
##########################################################################################
#1. Apply exclusion
## 
ESRD_Indicator_df <-read.csv(paste0(outdir,"ESRD.csv"),stringsAsFactors = F)
#In USRDs has outcome= 1,  on RRT last 48 hours
length(which(ESRD_Indicator_df[,"SOURCE"] == "inUSRD" & 
             ESRD_Indicator_df[,"ESRD_Outcome_USRDS2ndWay"] == 1 & 
             ESRD_Indicator_df[,"onRRT_Last48hBeforeDischarge"]==1))
#In USRDs has outcome =0, on RRT last 48 hours 
length(which(ESRD_Indicator_df[,"SOURCE"] == "inUSRD" & 
             is.na(ESRD_Indicator_df[,"ESRD_Outcome_USRDS2ndWay"]) == T & 
             ESRD_Indicator_df[,"onRRT_Last48hBeforeDischarge"]==1))

###
DuringFlag1_IDs <- ESRD_STATUS_df$STUDY_PATIENT_ID[which(ESRD_STATUS_df[,"DURING_INDEXED_INDICATOR"] == 1)]

#in USRD and has outcome yes ID
inUSRDYES_IDs <- ESRD_Indicator_df$STUDY_PATIENT_ID[which(ESRD_Indicator_df[,"SOURCE"] == "inUSRD" & 
                         ESRD_Indicator_df[,"ESRD_Outcome_USRDS2ndWay"] == 1 & 
                         ESRD_Indicator_df[,"onRRT_Last48hBeforeDischarge"]==1)]
#in USRD and has outcome no ID
inUSRDNO_IDs <- ESRD_Indicator_df$STUDY_PATIENT_ID[which(ESRD_Indicator_df[,"SOURCE"] == "inUSRD" & 
                                                           is.na(ESRD_Indicator_df[,"ESRD_Outcome_USRDS2ndWay"]) == T & 
                                                           ESRD_Indicator_df[,"onRRT_Last48hBeforeDischarge"]==1)]
  

length(which(inUSRDYES_IDs %in% DuringFlag1_IDs))
length(which(inUSRDNO_IDs %in% DuringFlag1_IDs))
