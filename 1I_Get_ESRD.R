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
#3. Process USRDS_ESRD.csv to get during and within 120 following HOSP discharge
############################################################################################################
ESRD_Indicator_df1 <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(ESRD_Indicator_df1) <- c("STUDY_PATIENT_ID","ESRD_DURING_AND_AFTER_HOSP_Within120D","SOURCE")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_Indicator_df1[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd cuz esrd dates has no hms
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  curr_hosp_end_plus120 <- curr_hosp_end + days(120)
  
  #Source 1 USRD info
  curr_usrd_df <- USRDS_ESRD_df[which(USRDS_ESRD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_esrd_date <- ymd(curr_usrd_df$ESRD_DATE)
  
  if (nrow(curr_usrd_df) != 0){ #if in USRDs
    ESRD_Indicator_df1[i,"SOURCE"]<- "in_USRDS"
    #During and after within 120 days
    if (curr_esrd_date > curr_hosp_start & curr_esrd_date <= curr_hosp_end_plus120) {
      ESRD_Indicator_df1[i,"ESRD_DURING_AND_AFTER_HOSP_Within120D"] <- 1
    }else{
      ESRD_Indicator_df1[i,"ESRD_DURING_AND_AFTER_HOSP_Within120D"] <- 0
    }

  }else{
    ESRD_Indicator_df1[i,"SOURCE"]<- "notin_USRDS"
    ESRD_Indicator_df1[i,"ESRD_DURING_AND_AFTER_HOSP_Within120D"]<- 0
  }
  
}

table(ESRD_Indicator_df1$ESRD_DURING_AND_AFTER_HOSP_Within120D)

############################################################################################################
#4. Process ESRD_STATUS.csv to get during
############################################################################################################
ESRD_Indicator_df2 <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(ESRD_Indicator_df2) <- c("STUDY_PATIENT_ID","ESRD_DURING","SOURCE")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_Indicator_df2[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 2: status table
  curr_ESRD_STATUS_df <- ESRD_STATUS_df[which(ESRD_STATUS_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  if (nrow(curr_ESRD_STATUS_df) != 0){ #if in ESRD_STATUS.csv
    ESRD_Indicator_df2[i,"SOURCE"]<- "in_STATUS_TABLE"
    if (curr_ESRD_STATUS_df[,"DURING_INDEXED_INDICATOR"] == 1) {
      ESRD_Indicator_df2[i,"ESRD_DURING"] <- 1
    }else{
      ESRD_Indicator_df2[i,"ESRD_DURING"] <- 0
    }
    
  }else{
    ESRD_Indicator_df2[i,"SOURCE"]<- "notin_STATUS_TABLE"
    ESRD_Indicator_df2[i,"ESRD_DURING"]<- 0
  }
  
}

table(ESRD_Indicator_df2$ESRD_DURING)


############################################################################################################
#5.Combine Two data source to manually check if STATUS agres USRDS
############################################################################################################
ESRD_Comb <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 5))
colnames(ESRD_Comb) <- c("STUDY_PATIENT_ID","ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS",
                                                "ESRD_DURING_STATUS",
                                                "SOURCE_USRDS","SOURCE_STATUS")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_Comb[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 1: USRDs
  curr_USRDs <- ESRD_Indicator_df1[which(ESRD_Indicator_df1[,"STUDY_PATIENT_ID"] == curr_id),]
  ESRD_Comb[i,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"] <- curr_USRDs[,"ESRD_DURING_AND_AFTER_HOSP_Within120D"]
  ESRD_Comb[i,"SOURCE_USRDS"] <- curr_USRDs[,"SOURCE"]
  
  #source 2: status table
  curr_status_tb <- ESRD_Indicator_df2[which(ESRD_Indicator_df2[,"STUDY_PATIENT_ID"] == curr_id),]
  ESRD_Comb[i,"ESRD_DURING_STATUS"] <- curr_status_tb[,"ESRD_DURING"]
  ESRD_Comb[i,"SOURCE_STATUS"] <- curr_status_tb[,"SOURCE"]
  
}

USRD_DuringAfterWithin120D <- ESRD_Comb[,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"]
STATUS_During <- ESRD_Comb[,"ESRD_DURING_STATUS"]
table(USRD_DuringAfterWithin120D,STATUS_During)

############################################################################################################
##6. Add onRRT_last48hours flag to check
#onRRT last 48hours supposed to be have a during flag 
############################################################################################################
ESRD_Comb$onRRT_Last48h <- NA
for (i in 1:nrow(ESRD_Comb)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- ESRD_Comb[i,"STUDY_PATIENT_ID"]
  
  #on RRT last 48h flag
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_onRRT48_flag  <- curr_time_df[,"onRRT_Last48hBeforeDischarge"] 

  ESRD_Comb[i,"onRRT_Last48h"] <- curr_onRRT48_flag

}

#N of pts who has USRDS no, status no,and on RRT last 48h
usrds0_status0_onRRT48_df <- ESRD_Comb[which(ESRD_Comb[,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"] == 0 &
                                          ESRD_Comb[,"ESRD_DURING_STATUS"]== 0 &
                                          ESRD_Comb[,"onRRT_Last48h"]==1),]
nrow(usrds0_status0_onRRT48_df)
#N of pts who has USRDS no, status yes, and on RRT last 48h
usrds0_status1_onRRT48_df <- ESRD_Comb[which(ESRD_Comb[,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"] == 0 &
                                            ESRD_Comb[,"ESRD_DURING_STATUS"]== 1 &
                                            ESRD_Comb[,"onRRT_Last48h"]==1),]
nrow(usrds0_status1_onRRT48_df)

#N of pts who has USRDS yes, status no, and on RRT last 48h
usrds1_status0_onRRT48_df <- ESRD_Comb[which(ESRD_Comb[,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"] == 1 &
                                               ESRD_Comb[,"ESRD_DURING_STATUS"]== 0 &
                                               ESRD_Comb[,"onRRT_Last48h"]==1),]
nrow(usrds1_status0_onRRT48_df)


#N of pts who has USRDS yes, status yes, and on RRT last 48h
usrds1_status1_onRRT48_df <- ESRD_Comb[which(ESRD_Comb[,"ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS"] == 1 &
                                               ESRD_Comb[,"ESRD_DURING_STATUS"]== 1 &
                                               ESRD_Comb[,"onRRT_Last48h"]==1),]
nrow(usrds1_status1_onRRT48_df)

#write.csv(ESRD_Comb,paste0(outdir,"ESRD.csv"),row.names=FALSE)

