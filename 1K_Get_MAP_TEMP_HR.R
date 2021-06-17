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

#2.ICU d0toD3 info , used for filter feature values not in ICU
ICU_D0toD3_df <-read.csv(paste0(outdir,"ICU_D0toD3_Days_df.csv"),stringsAsFactors = F)


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

#'@TOCHECK
##########################################################################################
###If patient does not have D1 in ICU_D0toD3_df, remove the corresponding feature
##########################################################################################
remove_featureValue <- function(analysis_df,ICU_D0toD3_df){
  for (i in 1:nrow(analysis_df)){
    if (i %% 1000 == 0){print(i)}
    curr_id <- analysis_df[i,"STUDY_PATIENT_ID"]
    curr_ICU_days <- ICU_D0toD3_df[which(ICU_D0toD3_df[,"STUDY_PATIENT_ID"] == curr_id),"ICU_Stays_inDays"]
    curr_ICU_days <- unlist(strsplit(curr_ICU_days,split = "$$",fixed = T))
    search_string <- paste0(c(curr_ICU_days,"STUDY_PATIENT_ID"),collapse = "|") #columns of ID and corresponding Day feature
    colIndxes_toexclude <- which(grepl(search_string,colnames(analysis_df)) == F)
    if (length(colIndxes_toexclude) > 0 ){
      analysis_df[i,colIndxes_toexclude] <- NA
    }
  }
  return(analysis_df)
}

updated_Vital_df <- remove_featureValue(All_Vital_df,ICU_D0toD3_df)

write.csv(updated_Vital_df,paste0(outdir,"All_MAP_TEMP_HR_df_V2.csv"))

#'@NOTE: Add length ICU of stay to check
#if ICU duration > 1 day, it should not has NA, unless no value in corresponding feature df
#if ICU < 1 day, it still possible it has value, e.g, duration = 4 hours, if day 0 start from 10pm, Day0 = 2 hours, Day1 = 2 hours
All_time_df$ICU_LOS <-difftime(ymd_hms(All_time_df$Updated_ICU_DISCHARGE_DATE), ymd_hms(All_time_df$Updated_ICU_ADMIT_DATE),units = "days")
updated_Vital_df$ICU_LOS <- All_time_df$ICU_LOS 
updated_Vital_df$ICU_Days <- ICU_D0toD3_df$ICU_Stays_inDays

check <- updated_Vital_df[which(is.na(updated_Vital_df$MAP_D1_LOW)==T),]
table(check$ICU_Days)

#Check if patietn who has d1, but no feature value from the old All_MAP_TEMP_HR_df.csv
#some of the NAs in All_MAP_TEMP_HR_df.csv has both D0 only and no feature df
All_MAP_TEMP_HR_df <-read.csv(paste0(outdir,"All_MAP_TEMP_HR_df.csv"),stringsAsFactors = F)
IDs <- updated_Vital_df$STUDY_PATIENT_ID[which(is.na(All_MAP_TEMP_HR_df$MAP_D1_LOW)==T)]
which(!IDs %in% check$STUDY_PATIENT_ID)
