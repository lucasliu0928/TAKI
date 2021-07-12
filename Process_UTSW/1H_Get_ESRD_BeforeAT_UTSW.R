library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)
analysis_ID <- Inclusion_df$STUDY_PATIENT_ID

##########################################################################################
#2. Load data
##########################################################################################
#1. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#2. source 1: USRDS_ESRD
USRDS_ESRD_df <-read.csv(paste0(raw_dir,"tUSRDS_CORE_Patients.csv"),stringsAsFactors = F)
#change column names
cols_name_tochange <- which(colnames(USRDS_ESRD_df) %in% c("PATIENT_NUM","FIRST_SE"))
colnames(USRDS_ESRD_df)[cols_name_tochange] <- c("STUDY_PATIENT_ID","ESRD_DATE")
##remove blanks
USRDS_ESRD_df <- USRDS_ESRD_df[-which(USRDS_ESRD_df$ESRD_DATE==""),] 
#reformat 
USRDS_ESRD_df$ESRD_DATE <- as.character(mdy(USRDS_ESRD_df$ESRD_DATE))

#3. source 2
raw_ESRD_STATUS_df <-read.csv(paste0(raw_dir,"tESRDSummary.csv"),stringsAsFactors = F)
#change column names
cols_name_tochange <- which(colnames(raw_ESRD_STATUS_df) %in% c("PATIENT_NUM"))
colnames(raw_ESRD_STATUS_df)[cols_name_tochange] <- c("STUDY_PATIENT_ID")

#convert ESRD STATus to one patient per row table
IDs_inSTATUS <- unique(raw_ESRD_STATUS_df$STUDY_PATIENT_ID)
ESRD_STATUS_df <- as.data.frame(matrix(NA, nrow = length(IDs_inSTATUS) ,ncol = 4))
colnames(ESRD_STATUS_df) <- c("STUDY_PATIENT_ID","BEFORE","DURING","AFTER")
for (i in 1:length(IDs_inSTATUS)){
  curr_id <- IDs_inSTATUS[i]
  ESRD_STATUS_df[i,"STUDY_PATIENT_ID"] <- curr_id
  #curr raw table
  curr_ESRD_STATUS_df <- raw_ESRD_STATUS_df[which(raw_ESRD_STATUS_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  curr_hosp_start_times <- unique(curr_ESRD_STATUS_df$HOSP_ADMSN_TIME)
  if (length(curr_hosp_start_times) == 1){
    curr_before <- unique(curr_ESRD_STATUS_df$BEFORE_INDEXED_ADT)
    curr_during <- unique(curr_ESRD_STATUS_df$DURING_INDEXED_ADT)
    curr_after  <- unique(curr_ESRD_STATUS_df$AFTER_INDEXED_ADT)
    
    before_indxes <- which(curr_before == "BEFORE_INDEXED_ADT")
    if (length(before_indxes) > 0){
      ESRD_STATUS_df[i,"BEFORE"] <- 1
    }else{
      ESRD_STATUS_df[i,"BEFORE"] <- 0
    }
    
    during_indxes <- which(curr_during == "DURING_INDEXED_ADT")
    if (length(during_indxes) > 0){
      ESRD_STATUS_df[i,"DURING"] <- 1
    }else{
      ESRD_STATUS_df[i,"DURING"] <- 0
    }
    
    after_indxes <- which(curr_after == "AFTER_INDEXED_ADT")
    if (length(after_indxes) > 0){
      ESRD_STATUS_df[i,"AFTER"] <- 1
    }else{
      ESRD_STATUS_df[i,"AFTER"] <- 0
    }
    
  }else{
    ESRD_STATUS_df[i,"BEFORE"] <- "More than 1 HOSP admission"
    ESRD_STATUS_df[i,"DURING"] <- "More than 1 HOSP admission"
    ESRD_STATUS_df[i,"AFTER"] <- "More than 1 HOSP admission"
    
  }
  
  
}

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
  curr_esrd_date <- ymd(curr_usrd_df[,"ESRD_DATE"])
  
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

table(ESRD_BEFORE_AT_Indicator_df1$BEFORE_AT_ADMISSION_USRDS) #10189   314  

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
    if (curr_ESRD_STATUS_df[,"BEFORE"] == 1) {
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

table(Final_ESRD_BEFORE_AT_df$ESRD_BEFORE_AT) #10068   435 


write.csv(Final_ESRD_BEFORE_AT_df,paste0(outdir,"ESRD_Before_AT.csv"),row.names = F)


############################################################################################################
#check how many status_flag=1, but usrd_flag=0, and they are has dates in USRDS: 30
#so the final =1 , should equal status_yes (151-30) + USRDS_yes(44+270) = 435
############################################################################################################
length(which(ESRD_BEFORE_AT_Indicator_df_Comb$ESRD_BEFORE_AT_STATUS== 1 & 
               ESRD_BEFORE_AT_Indicator_df_Comb$ESRD_BEFORE_AT_USRDS==0 &
               ESRD_BEFORE_AT_Indicator_df_Comb$SOURCE_USRDS == "in_USRDS" ))

