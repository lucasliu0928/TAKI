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


#3.ORGANSUPP_ECMO
raw_ORGANSUPP_ECMO_df <- read.csv(paste0(raw_dir,"ORGANSUPP_ECMO.csv"),stringsAsFactors = F)

#4.ORGANSUPP_IABP
raw_ORGANSUPP_IABP_df <- read.csv(paste0(raw_dir,"ORGANSUPP_IABP.csv"),stringsAsFactors = F)

#5.ORGANSUPP_VENT
raw_ORGANSUPP_VENT_df <- read.csv(paste0(raw_dir,"ORGANSUPP_VENT.csv"),stringsAsFactors = F)

#6.ORGANSUPP_VAD
raw_ORGANSUPP_VAD_df <- read.csv(paste0(raw_dir,"ORGANSUPP_VAD.csv"),stringsAsFactors = F)


##########################################################################################
#Features to extract :  1. ECMO in ICU D0-D3
#                       2. IABP in ICU D0-D3
#                       3. MV in ICU D0-D3
#                       4. VAD in ICU D0-D3
#                       5. Mechanical Hemodynamic Support: Use of VAD, IABP, or ECMO in ICU D0-D3
##########################################################################################
ECMO_IABP_MV_VAD_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 6))
colnames(ECMO_IABP_MV_VAD_df) <- c("STUDY_PATIENT_ID", "ECMO_ICUD0toD3","IABP_ICUD0toD3",
                                   "MV_ICUD0toD3","VAD_ICUD0toD3","MechanicalHemodynamicSupp_ICUD0toD3")
for (i in 1: length(analysis_ID)){
  if (i %% 1000== 0){print(i)}
  curr_id <- analysis_ID[i]
  ECMO_IABP_MV_VAD_df[i,"STUDY_PATIENT_ID"] <- curr_id
  ECMO_IABP_MV_VAD_df[i,"ECMO_ICUD0toD3"] <- get_onMachine_flag_ICUD0_D3(raw_ORGANSUPP_ECMO_df,All_time_df,curr_id,"ECMO_START_DATE","ECMO_STOP_DATE")
  ECMO_IABP_MV_VAD_df[i,"IABP_ICUD0toD3"] <- get_onMachine_flag_ICUD0_D3(raw_ORGANSUPP_IABP_df,All_time_df,curr_id,"IABP_START_DATE","IABP_STOP_DATE")
  ECMO_IABP_MV_VAD_df[i,"MV_ICUD0toD3"] <- get_onMachine_flag_ICUD0_D3(raw_ORGANSUPP_VENT_df,All_time_df,curr_id,"VENT_START_DATE","VENT_STOP_DATE")
  ECMO_IABP_MV_VAD_df[i,"VAD_ICUD0toD3"] <- get_onMachine_flag_ICUD0_D3(raw_ORGANSUPP_VAD_df,All_time_df,curr_id,"VAD_START_DATE","VAD_STOP_DATE")
  
  if ( ECMO_IABP_MV_VAD_df[i,"VAD_ICUD0toD3"] == 1 | 
       ECMO_IABP_MV_VAD_df[i,"IABP_ICUD0toD3"] == 1|
       ECMO_IABP_MV_VAD_df[i,"ECMO_ICUD0toD3"] == 1){
    ECMO_IABP_MV_VAD_df[i,"MechanicalHemodynamicSupp_ICUD0toD3"] <- 1
  }else{
    ECMO_IABP_MV_VAD_df[i,"MechanicalHemodynamicSupp_ICUD0toD3"] <- 0
  }
}


#4. Compute missing
feature_columns <-  c("ECMO_ICUD0toD3", "IABP_ICUD0toD3","MV_ICUD0toD3", "VAD_ICUD0toD3",
                      "MechanicalHemodynamicSupp_ICUD0toD3")
missing_table <- get_missing_rate_table(ECMO_IABP_MV_VAD_df,feature_columns)
missing_table

write.csv(ECMO_IABP_MV_VAD_df,paste0(outdir,"All_ECMO_IABP_MV_VAD_ICUD0toD3.csv"),row.names = F)