library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"


##########################################################################################
#Load Raw Data
#'@NOTE: SEX_ID == 1 (MALE), SEX_ID == 0 (non_MALE)
##########################################################################################
#1.Load raw demo data
raw_demo_df <- read.csv(paste0(raw_dir,"tPatients.csv"),stringsAsFactors = F)

#2.Change column name
col_to_change_indxes <- which(colnames(raw_demo_df) %in% c("PATIENT_NUM"))
colnames(raw_demo_df)[col_to_change_indxes] <- c("STUDY_PATIENT_ID")

#3.remove duplicates
raw_demo_df <- raw_demo_df[!duplicated(raw_demo_df[,c("STUDY_PATIENT_ID","SEX_ID","RACE_BLACK","DOB")]),] ##remove duplicate rows

#4.Code NA race_black as 0
raw_demo_df[which(is.na(raw_demo_df$RACE_BLACK)==T),"RACE_BLACK"] <- 0

#4. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

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
raw_demo_df$AGE <- NA
for (i in 1:nrow(raw_demo_df)){
  if (i %% 1000 ==0){print(i)}
  curr_id <- raw_demo_df[i,"STUDY_PATIENT_ID"]
  curr_DOB <- raw_demo_df[i,"DOB"]
  
  curr_idxes <- which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id)
  if (length(curr_idxes) > 0){
    curr_hosp_start <- All_time_df[curr_idxes,"Updated_HOSP_ADMIT_DATE"]
    raw_demo_df[i,"AGE"] <-  as.numeric(difftime(ymd_hms(curr_hosp_start) , ymd_hms(curr_DOB),units = "days"))/365
  }
  
}

#Get Features
Gender_Male <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"SEX_ID","SEX_ID")
Race_Black <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"RACE_BLACK","RACE_BLACK")
Race <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"SEX","SEX")

AGE <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"AGE","AGE")

All_RACE_GENDER_df <- cbind(Gender_Male,Race_Black,AGE)
All_RACE_GENDER_df <- All_RACE_GENDER_df[,-c(3,5)] #remove duplicated columns
colnames(All_RACE_GENDER_df)[c(2,3)] <- c("Gender_Male","Race_Black")

#exclude pts has no gender or no race or age (0)
no_demo_indexes <- which(is.na(All_RACE_GENDER_df$Gender_Male)== T |
                           is.na(All_RACE_GENDER_df$Race_Black)== T | 
                           is.na(All_RACE_GENDER_df$AGE)==T)
if(length(no_demo_indexes) >0){
All_RACE_GENDER_df <- All_RACE_GENDER_df[-no_demo_indexes,]
}

#add non-coded gender and race for EPI function use later
All_RACE_GENDER_df$GENDER <- NA
male_indxes <- which(All_RACE_GENDER_df$Gender_Male == 1)
All_RACE_GENDER_df$GENDER[male_indxes] <- "M"
All_RACE_GENDER_df$GENDER[-male_indxes] <- "F"

All_RACE_GENDER_df$RACE <- NA
black_indxes <- which(All_RACE_GENDER_df$Race_Black == 1)
All_RACE_GENDER_df$RACE[black_indxes] <- "BLACK/AFR AMERI"
All_RACE_GENDER_df$RACE[-black_indxes] <- "Others"

write.csv(All_RACE_GENDER_df,paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),row.names = F) #36017
