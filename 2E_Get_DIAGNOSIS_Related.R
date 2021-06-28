library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#3. Load raw DIAGNOSIS data
raw_DIAGNOSIS_df <- read.csv(paste0(raw_dir,"DIAGNOSIS.csv"),stringsAsFactors = F)

#4. Get sepsis df
sepsis_df <- raw_DIAGNOSIS_df[which(grepl("sepsis",raw_DIAGNOSIS_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]
sepsis_BeforeAtAdmin_df <- sepsis_df[which(sepsis_df$DIAGNOSIS_TYPE %in% c("06.Working Dx","01.Visit Reason","Admit Dx","..Working Dx")),]
sepsis_beforeat_IDs <- unique(sepsis_BeforeAtAdmin_df$STUDY_PATIENT_ID)

##########################################################################################
#Features to extract : 1. Septic before/at admission

#Steps: 1. Get raw available values
##########################################################################################
#1. Get raw available values
Final_sepsis_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2 ))
colnames(Final_sepsis_df) <- c("STUDY_PATIENT_ID","Sepsis_Before_or_At_Admission")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  Final_sepsis_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  if (curr_id %in% sepsis_beforeat_IDs){
    Final_sepsis_df[i,"Sepsis_Before_or_At_Admission"] <- 1
  }else{
    Final_sepsis_df[i,"Sepsis_Before_or_At_Admission"] <- 0
  }
}
table(Final_sepsis_df$Sepsis_Before_or_At_Admission) #6901  453
write.csv(Final_sepsis_df,paste0(outdir,"All_sepsis_Before_Or_At_Admission.csv"),row.names = F)