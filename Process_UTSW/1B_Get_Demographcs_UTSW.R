library(lubridate)
library(openxlsx)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"
##########################################################################################
#Load Raw Data
##########################################################################################
#1. Load xilong file
xilong_exlcusion_df <- read.xlsx(paste0("/Users/lucasliu/Downloads/Patient list of deleting 07062021.xlsx"),sheet = 1)

#1.Load raw demo data
raw_demo_df <- read.csv(paste0(raw_dir,"tPatients.csv"),stringsAsFactors = F)

#3. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(raw_dir,"tIndexedIcuAdmission.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[!duplicated(All_time_df[,c("PATIENT_NUM","ICU_DISCH_TIME","HSP_DISCH_TIME")]),] ##remove duplicate rows

check_df <- All_time_df[duplicated(All_time_df$PATIENT_NUM),]
check_idxes <- which(ymd_hms(All_time_df$ICU_DISCH_TIME) > ymd_hms(All_time_df$HSP_DISCH_TIME) +hours(24))
check_df <- All_time_df[check_idxes,c("PATIENT_NUM","ICU_DISCH_TIME","HSP_DISCH_TIME")]

#4.RRT time
RRT_df <-read.csv(paste0(raw_dir,"tDialysis.csv"),stringsAsFactors = F)

#5.Scr
All_Scr_df <-read.csv(paste0(raw_dir,"all_scr_data.csv"),stringsAsFactors = F)


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
    curr_hosp_start <- All_time_df[curr_idxes,"Updated_HOSP_ADMIT_DATE"]
    raw_DOB_df[i,"AGE"] <-  as.numeric(difftime(ymd_hms(curr_hosp_start) , ymd_hms(curr_DOB),units = "days"))/365
  }
  
}

#Get Features
GENDER <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"GENDER","GENDER")
RACE <- get_raw_var_values_1option_func(raw_demo_df,analysis_ID,"RACE","RACE")
AGE <- get_raw_var_values_1option_func(raw_DOB_df,analysis_ID,"AGE","AGE")

All_RACE_GENDER_df <- cbind(GENDER,RACE,AGE)
All_RACE_GENDER_df <- All_RACE_GENDER_df[,-c(3,5)] #remove duplicated columns
#code blank as NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"GENDER"] ==""),"GENDER"] <- NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"GENDER"] == "U"),"GENDER"] <- NA
All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"RACE"] ==""),"RACE"] <- NA

#exclude pts has no gender or no race or age
no_demo_indexes <- which(is.na(All_RACE_GENDER_df$GENDER)== T |
                           is.na(All_RACE_GENDER_df$RACE)== T | 
                           is.na(All_RACE_GENDER_df$AGE)==T)
All_RACE_GENDER_df <- All_RACE_GENDER_df[-no_demo_indexes,]
write.csv(All_RACE_GENDER_df,paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),row.names = F) #36017
