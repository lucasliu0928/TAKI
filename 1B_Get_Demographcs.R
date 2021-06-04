library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/rawData_processed/uky/"

##########################################################################################
#Load Raw Data
##########################################################################################
#1.Load raw demo data
raw_demo_df <- read.csv(paste0(raw_dir,"DEMOGRAPHICS_INDX.csv"),stringsAsFactors = F)
raw_demo_df <- raw_demo_df[!duplicated(raw_demo_df[,c("STUDY_PATIENT_ID","GENDER","RACE")]),] ##remove duplicate rows

#2. Load raw DOB data
raw_DOB_df <- read.csv(paste0(raw_dir,"DOB.csv"),stringsAsFactors = F)
raw_DOB_df <- raw_DOB_df[!duplicated(raw_DOB_df[,c("STUDY_PATIENT_ID","DOB")]),] ##remove duplicate rows

#3. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Timeinfo_df.csv"),stringsAsFactors = F)

##########################################################################################
#anlaysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])


##########################################################################################
#Features to extract :  1. Gender
#                       2. Race
#Load UK raw DOB
#Features to extract :
#                       1.DOB
#                       2.Compute Age at admision
##########################################################################################
#compute Age
raw_DOB_df$AGE <- NA
for (i in 1:nrow(raw_DOB_df)){
  if (i %% 1000 ==0){print(i)}
  curr_id <- raw_DOB_df[i,"STUDY_PATIENT_ID"]
  curr_DOB <- raw_DOB_df[i,"DOB"]
  
  curr_idxes <- which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id)
  if (length(curr_idxes) > 0){
    curr_hosp_start <- All_time_df[curr_idxes,"HOSP_ADMIT_DATE"]
    raw_DOB_df[i,"AGE"] <-  as.numeric(difftime(ymd_hms(curr_hosp_start) , ymd_hms(curr_DOB),units = "days"))/365
  }
  
}

#Get Features
GENDER <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"GENDER","GENDER")
RACE <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"RACE","RACE")
AGE <- get_raw_var_values_1option_func(raw_DOB_df,analysis_ID,"AGE","AGE")

All_RACE_GENDER_df <- cbind(GENDER,RACE,AGE)
All_RACE_GENDER_df <- All_RACE_GENDER_df[,-c(3,5)] #remove duplicated columns
#remove blank as NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"GENDER"] ==""),"GENDER"] <- NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"GENDER"] == "U"),"GENDER"] <- NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"RACE"] ==""),"RACE"] <- NA

write.csv(All_RACE_GENDER_df,paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),row.names = F)
