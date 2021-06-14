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

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3.Load CLINICAL_VITALS 
##########################################################################################
raw_Vitals_df <- read.csv(paste0(raw_dir,"CLINICAL_VITALS.csv"),stringsAsFactors = F)


##########################################################################################
#4. Extract Features
#'@NOTE: Only has D1 value
#Features to extract :  1. Temperature D1 High/Low
#                       2. MAP D1 High/Low
#                       3. Heart Rate D1 High/Low
##########################################################################################
#Get features
MAP_d1_Low <- get_raw_var_values_2options_func(raw_Vitals_df,analysis_ID,"MAP_D1_LOW","ART_MEAN_D1_LOW_VALUE","CUFF_MEAN_D1_LOW_VALUE")
MAP_d1_High <- get_raw_var_values_2options_func(raw_Vitals_df,analysis_ID,"MAP_D1_HIGH","ART_MEAN_D1_HIGH_VALUE","CUFF_MEAN_D1_HIGH_VALUE")
Temp_d1_Low <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"Temperature_D1_LOW","TEMPERATURE_D1_LOW_VALUE")
Temp_d1_High <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"Temperature_D1_HIGH","TEMPERATURE_D1_HIGH_VALUE")
#Convert to celsius
Temp_d1_Low[,"Temperature_D1_LOW"] <- (Temp_d1_Low[,"Temperature_D1_LOW"] -32)*(5/9)
Temp_d1_High[,"Temperature_D1_HIGH"] <- (Temp_d1_High[,"Temperature_D1_HIGH"] -32)*(5/9)

HR_d1_Low <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"HR_D1_LOW","HEART_RATE_D1_LOW_VALUE")
HR_d1_High <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"HR_D1_HIGH","HEART_RATE_D1_HIGH_VALUE")

All_Vital_df <- cbind(MAP_d1_Low,MAP_d1_High,Temp_d1_Low,Temp_d1_High,HR_d1_Low,HR_d1_High)
All_Vital_df <- All_Vital_df[,-c(3,5,7,9,11)] #remove duplicated ID columns
write.csv(All_Vital_df,paste0(outdir,"All_MAP_TEMP_HR_df.csv"))
