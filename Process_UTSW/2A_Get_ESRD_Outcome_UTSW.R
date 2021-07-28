library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"
##########################################################################################
#1.Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #2233

#2. Corrected Time df and get
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
#3. Process tUSRDS_CORE_Patients.csv to get during and within 120 following HOSP discharge
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

table(ESRD_Indicator_df1$ESRD_DURING_AND_AFTER_HOSP_Within120D) #2206  37

############################################################################################################
#4. Process tESRDSummary.csv to get during
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
    if (curr_ESRD_STATUS_df[,"DURING"] == 1) {
      ESRD_Indicator_df2[i,"ESRD_DURING"] <- 1
    }else{
      ESRD_Indicator_df2[i,"ESRD_DURING"] <- 0
    }
    
  }else{
    ESRD_Indicator_df2[i,"SOURCE"]<- "notin_STATUS_TABLE"
    ESRD_Indicator_df2[i,"ESRD_DURING"]<- 0
  }
  
}

table(ESRD_Indicator_df2$ESRD_DURING) #2136   97


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

############################################################################################################
#Check agreement
############################################################################################################
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


############################################################################################################
#6.ESRD_120 YES is defined as: 
#a.	For the patient has record in USRDS_ESRD, if Hospital Start < ESRD_Dates <= Hospital End + 120 days 
#b.	For the patient has no record in USRDS_ESRD but the patient has record in ESRD_STATUS, if ESRD_During = 1 
############################################################################################################
Final_ESRD_120_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 2))
colnames(Final_ESRD_120_df) <- c("STUDY_PATIENT_ID","ESRD_120")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  Final_ESRD_120_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #source 1: USRDs
  curr_USRDs <- ESRD_Indicator_df1[which(ESRD_Indicator_df1[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_USRDs_flag <- curr_USRDs[,"ESRD_DURING_AND_AFTER_HOSP_Within120D"]
  curr_USRDs_source <- curr_USRDs[,"SOURCE"]
  
  #source 2: status table
  curr_status_tb <- ESRD_Indicator_df2[which(ESRD_Indicator_df2[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_status_flag <- curr_status_tb[,"ESRD_DURING"]
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
  
  Final_ESRD_120_df[i,"ESRD_120"] <- final_flag
}

table(Final_ESRD_120_df$ESRD_120) #2128  105 


write.csv(Final_ESRD_120_df,paste0(outdir,"ESRD_120.csv"),row.names = F)


############################################################################################################
#check how many status_flag=1, but usrd_flag=0, and they are actually in USRDS: 3
#so the final=1 should equal status_yes (81-3) + USRDS_yes(11+16) = 105
############################################################################################################
length(which(ESRD_Comb$ESRD_DURING_STATUS== 1 & 
               ESRD_Comb$ESRD_DURING_AND_AFTER_HOSP_Within120D_USRDS==0 &
               ESRD_Comb$SOURCE_USRDS == "in_USRDS"))

table(USRD_DuringAfterWithin120D,STATUS_During)



