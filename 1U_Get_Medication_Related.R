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

#3.MEDICATIONS_INDX
raw_MEDICATIONS_df <- read.csv(paste0(raw_dir,"MEDICATIONS_INDX.csv"),stringsAsFactors = F)

#remove the date has nchar > 10 (e.g 4/9/201- times a day), cannot tell which date it is
remove_idxes <- which(nchar(raw_MEDICATIONS_df[,"ORDER_ENTERED_DATE"])>10)
raw_MEDICATIONS_df <- raw_MEDICATIONS_df[-remove_idxes,]

#Nephrotoxin_df
Nephrotoxin_indxes <- which(raw_MEDICATIONS_df[,"MEDICATION_TYPE"] %in% c("ACE INHIBITORS","ANGIOTENSIN RECEPTOR BLOCKERS","Aminoglycosides","NSAIDs"))
Nephrotoxin_df  <- raw_MEDICATIONS_df[Nephrotoxin_indxes,]


#Vasopressor_df 
medication_names <- c("dopamine","dobutamine","milrinone","epinephrine","norepinephrine","phenylephrine","vasopressin")
search_string <- paste0(medication_names,collapse = "|")
Vasopressor_indxes <- which(grepl(search_string,raw_MEDICATIONS_df[,"MEDICATION_NAME"],ignore.case = T)==T)
Vasopressor_df  <- raw_MEDICATIONS_df[Vasopressor_indxes,]

##########################################################################################
#Features to extract : 1. Nephrotoxin exposure in ICU D0-D3
#                      2. Vasopressor exposure in ICU D0-D3

##########################################################################################
#1. Get raw available values
Exposure_df<- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 5))
colnames(Exposure_df) <- c("STUDY_PATIENT_ID","Nephrotoxin_ICUD0toD3","Nephrotoxin_Meds_ICUD0toD3","Vasopressor_ICUD0toD3","Vasopressor_Meds_ICUD0toD3")

for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  Exposure_df[i,"STUDY_PATIENT_ID"] <- curr_id

  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  
  #Get actual days/times in ICU D0-D3
  #it could be ICU end time (e.g, if ICU stays < 3 days) or the end of ICU D3
  curr_actual_ICU_time_idxes <- which(colnames(curr_time_df) %in% c("Actual_D0_End","Actual_D1_End","Actual_D2_End","Actual_D3_End"))
  curr_actual_ICU_time <- curr_time_df[,curr_actual_ICU_time_idxes]
  curr_last_ICU_time <- max(ymd_hms(curr_actual_ICU_time),na.rm = T)
  
  res1 <- get_exposure_toMedication_inTimeWindow(curr_icu_start,curr_last_ICU_time,Nephrotoxin_df,curr_id,"MEDICATION_TYPE")
  Exposure_df[i,"Nephrotoxin_ICUD0toD3"] <- res1[[1]]
  Exposure_df[i,"Nephrotoxin_Meds_ICUD0toD3"] <- res1[[2]]
  
  res2 <- get_exposure_toMedication_inTimeWindow(curr_icu_start,curr_last_ICU_time,Vasopressor_df,curr_id,"MEDICATION_NAME")
  Exposure_df[i,"Vasopressor_ICUD0toD3"] <-res2[[1]]
  Exposure_df[i,"Vasopressor_Meds_ICUD0toD3"] <-res2[[2]]
  
}

table(Exposure_df$Nephrotoxin_ICUD0toD3)
table(Exposure_df$Vasopressor_ICUD0toD3)

#Compute missing
feature_columns <- c("Nephrotoxin_ICUD0toD3","Vasopressor_ICUD0toD3")
missing_table <- get_missing_rate_table(Exposure_df,feature_columns)
missing_table

write.csv(Exposure_df,paste0(outdir,"All_Nephrotoxin_Vasopressor.csv"),row.names = F)
