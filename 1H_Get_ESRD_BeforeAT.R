#Dertmine if ESRD before or AT HOSPITAL 
#1. Source 1: ESRD_USRDS.csv (Gold dataset, if not in USRD, use ESRD_STATUS.csv, then manual check
#2. Source 2: ESRD_STATUS.csv 
library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"


##########################################################################################
#1. Analysis Id before exclusion of ESRD
##########################################################################################
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID_BeforeExclusionOfESRD.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"])


##########################################################################################
#2. Load data
##########################################################################################
#1. Corrected Time df 
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
ESRD_STATUS_df[which(ESRD_STATUS_df == "",arr.ind = T)] <- 0
ESRD_STATUS_df[which(ESRD_STATUS_df == "Y",arr.ind = T)] <- 1



############################################################################################################
#3. Process USRDS_ESRD.csv to get before or at
############################################################################################################
ESRD_BEFORE_AT_Indicator_df1 <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(ESRD_BEFORE_AT_Indicator_df1) <- c("STUDY_PATIENT_ID","BEFORE_AT_ADMISSION_USRDS","SOURCE")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_BEFORE_AT_Indicator_df1[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd cuz esrd dates has no hms

  #Source 1 USRD info
  curr_usrd_df <- USRDS_ESRD_df[which(USRDS_ESRD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_esrd_date <- ymd(curr_usrd_df$ESRD_DATE)

  if (nrow(curr_usrd_df) != 0){ #if in USRDs
    ESRD_BEFORE_AT_Indicator_df1[i,"SOURCE"]<- "in_USRDS"
     if (curr_esrd_date <= curr_hosp_start) {
      ESRD_BEFORE_AT_Indicator_df1[i,"BEFORE_AT_ADMISSION_USRDS"] <- 1
     }else{
      ESRD_BEFORE_AT_Indicator_df1[i,"BEFORE_AT_ADMISSION_USRDS"] <- 0
    }
    
  }else{
    ESRD_BEFORE_AT_Indicator_df1[i,"SOURCE"]<- "notin_USRDS"
    ESRD_BEFORE_AT_Indicator_df1[i,"BEFORE_AT_ADMISSION_USRDS"]<- 0
  }
  
}

table(ESRD_BEFORE_AT_Indicator_df1$BEFORE_AT_ADMISSION_USRDS)

############################################################################################################
#4. Process ESRD_STATUS.csv to get before or at
############################################################################################################
ESRD_BEFORE_AT_Indicator_df2 <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(ESRD_BEFORE_AT_Indicator_df2) <- c("STUDY_PATIENT_ID","BEFORE_AT_ADMISSION_STATUS","SOURCE")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_BEFORE_AT_Indicator_df2[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 2: status table
  curr_ESRD_STATUS_df <- ESRD_STATUS_df[which(ESRD_STATUS_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  if (nrow(curr_ESRD_STATUS_df) != 0){ #if in ESRD_STATUS.csv
    ESRD_BEFORE_AT_Indicator_df2[i,"SOURCE"]<- "in_STATUS_TABLE"
    if (curr_ESRD_STATUS_df[,"AT_ADMISSION_INDICATOR"] == 1 | curr_ESRD_STATUS_df[,"BEFORE_INDEXED_INDICATOR"] == 1) {
      ESRD_BEFORE_AT_Indicator_df2[i,"BEFORE_AT_ADMISSION_STATUS"] <- 1
    }else{
      ESRD_BEFORE_AT_Indicator_df2[i,"BEFORE_AT_ADMISSION_STATUS"] <- 0
    }
    
  }else{
    ESRD_BEFORE_AT_Indicator_df2[i,"SOURCE"]<- "notin_STATUS_TABLE"
    ESRD_BEFORE_AT_Indicator_df2[i,"BEFORE_AT_ADMISSION_STATUS"]<- 0
  }
  
}

############################################################################################################
#5.Combine Two data source to manually check if STATUS agres USRDS
############################################################################################################
ESRD_BEFORE_AT_Indicator_df_Comb <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 5))
colnames(ESRD_BEFORE_AT_Indicator_df_Comb) <- c("STUDY_PATIENT_ID","ESRD_BEFORE_AT_USRDS","ESRD_BEFORE_AT_STATUS",
                                                "SOURCE_USRDS","SOURCE_STATUS")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  ESRD_BEFORE_AT_Indicator_df_Comb[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 1: USRDs
  curr_USRDs <- ESRD_BEFORE_AT_Indicator_df1[which(ESRD_BEFORE_AT_Indicator_df1[,"STUDY_PATIENT_ID"] == curr_id),]
  ESRD_BEFORE_AT_Indicator_df_Comb[i,"ESRD_BEFORE_AT_USRDS"] <- curr_USRDs[,"BEFORE_AT_ADMISSION_USRDS"]
  ESRD_BEFORE_AT_Indicator_df_Comb[i,"SOURCE_USRDS"] <- curr_USRDs[,"SOURCE"]
    
  #source 2: status table
  curr_status_tb <- ESRD_BEFORE_AT_Indicator_df2[which(ESRD_BEFORE_AT_Indicator_df2[,"STUDY_PATIENT_ID"] == curr_id),]
  ESRD_BEFORE_AT_Indicator_df_Comb[i,"ESRD_BEFORE_AT_STATUS"] <- curr_status_tb[,"BEFORE_AT_ADMISSION_STATUS"]
  ESRD_BEFORE_AT_Indicator_df_Comb[i,"SOURCE_STATUS"] <- curr_status_tb[,"SOURCE"]
  
}

USRD_BeforeAT <- ESRD_BEFORE_AT_Indicator_df_Comb[,"ESRD_BEFORE_AT_USRDS"]
STATUS_BeforeAT <- ESRD_BEFORE_AT_Indicator_df_Comb[,"ESRD_BEFORE_AT_STATUS"]
table(USRD_BeforeAT,STATUS_BeforeAT)

############################################################################################################
#6. Final before/AT status
#USE USRDs first, then if patient not in USRDS, use status table
############################################################################################################
Final_ESRD_BEFORE_AT_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 2))
colnames(Final_ESRD_BEFORE_AT_df) <- c("STUDY_PATIENT_ID","ESRD_BEFORE_AT")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  Final_ESRD_BEFORE_AT_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 1: USRDs
  curr_USRDs <- ESRD_BEFORE_AT_Indicator_df1[which(ESRD_BEFORE_AT_Indicator_df1[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_USRDs_flag <- curr_USRDs[,"BEFORE_AT_ADMISSION_USRDS"]
  curr_USRDs_source <- curr_USRDs[,"SOURCE"]
  
  #source 2: status table
  curr_status_tb <- ESRD_BEFORE_AT_Indicator_df2[which(ESRD_BEFORE_AT_Indicator_df2[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_status_flag <- curr_status_tb[,"BEFORE_AT_ADMISSION_STATUS"]
  curr_status_source<- curr_status_tb[,"SOURCE"]
  
  if (curr_USRDs_source == "in_USRDS"){ #as long as it is in USRDS
    final_flag <- curr_USRDs_flag
  }else if(curr_USRDs_source == "notin_USRDS" & curr_status_source == "in_STATUS_TABLE"){ #if not in USRDS, but in status
    final_flag <- curr_status_flag
  }else if (curr_USRDs_source == "notin_USRDS" & curr_status_source == "notin_STATUS_TABLE"){#if not in both, both has the same flag
    final_flag <- unique(c(curr_USRDs_flag,curr_status_flag))
  }else{
    final_flag <- NA
  }
  
  Final_ESRD_BEFORE_AT_df[i,"ESRD_BEFORE_AT"] <- final_flag
}

table(Final_ESRD_BEFORE_AT_df$ESRD_BEFORE_AT) #7354  447 


#write.csv(Final_ESRD_BEFORE_AT_df,paste0(outdir,"ESRD_Before_AT.csv"),row.names = F)


############################################################################################################
#check how many status_flag=1, but usrd_flag=0, and they are actually in USRDS: 51
#so the final =1 , should equal status_yes (155-51) + USRDS_yes(48+295) = 447
############################################################################################################
length(which(ESRD_BEFORE_AT_Indicator_df_Comb$ESRD_BEFORE_AT_STATUS== 1 & 
               ESRD_BEFORE_AT_Indicator_df_Comb$ESRD_BEFORE_AT_USRDS==0 &
               ESRD_BEFORE_AT_Indicator_df_Comb$SOURCE_USRDS == "in_USRDS" ))

