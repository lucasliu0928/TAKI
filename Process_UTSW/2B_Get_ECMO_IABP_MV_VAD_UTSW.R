library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

preprocess_machine_data_func <- function(input_df, start_time_col,stop_time_col){
  machine_df <- input_df[,c("STUDY_PATIENT_ID",start_time_col,stop_time_col)]
  
  #remove NA dates rows
  machine_df <- machine_df[complete.cases(machine_df[,c(start_time_col,start_time_col)]),]
  
  #reformat dates to "2014-12-06 00:00:00"
  machine_df[,start_time_col] <- paste(ymd(machine_df[,start_time_col]),"00:00:00")
  machine_df[,stop_time_col]  <- paste(ymd(machine_df[,stop_time_col]),"00:00:00")
  
  return(machine_df)
}



#data dir
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only


#3.RAW data
raw_ECMO_IABP_MV_VAD_df <- read.csv(paste0(outdir,"xilong_extracted/Final dataset for Lucas part 3 08122021.csv"),stringsAsFactors = F)
#recode empty to NA
raw_ECMO_IABP_MV_VAD_df[which(raw_ECMO_IABP_MV_VAD_df=="",arr.ind = T)] <- NA
colnames(raw_ECMO_IABP_MV_VAD_df)[2] <- "STUDY_PATIENT_ID"

#3.ORGANSUPP_ECMO
raw_ORGANSUPP_ECMO_df <- preprocess_machine_data_func(raw_ECMO_IABP_MV_VAD_df,"ECMO_START_DATE","ECMO_END_DATE")
  
#4.ORGANSUPP_IABP
raw_ORGANSUPP_IABP_df <- preprocess_machine_data_func(raw_ECMO_IABP_MV_VAD_df,"IABP_START_DATE","IABP_END_DATE")

#5.ORGANSUPP_VENT
raw_ORGANSUPP_VENT_df <-  preprocess_machine_data_func(raw_ECMO_IABP_MV_VAD_df,"MV_START_DATE","MV_END_DATE")

#6.ORGANSUPP_VAD
raw_ORGANSUPP_VAD_df <- preprocess_machine_data_func(raw_ECMO_IABP_MV_VAD_df,"VAD_START_DATE","VAD_END_DATE")




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

#write.csv(ECMO_IABP_MV_VAD_df,paste0(outdir,"All_ECMO_IABP_MV_VAD_ICUD0toD3.csv"),row.names = F)


##########################################################################################
#Features to extract :  1. days ECMO in ICU D0-D3
#                       2. days IABP in ICU D0-D3
#                       3. days MV in ICU D0-D3
#                       4. days VAD in ICU D0-D3
#'@NOTE If on Machine in ICU D0toD3, but START date = STOP date
#'then 1. update stop date == stop/start date at 23:59:59, 2. update on machine res
##########################################################################################
Days_ECMO_IABP_MV_VAD_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 5))
colnames(Days_ECMO_IABP_MV_VAD_df) <- c("STUDY_PATIENT_ID", "Days_ECMO_ICUD0toD3","Days_IABP_ICUD0toD3",
                                        "Days_MV_ICUD0toD3","Days_VAD_ICUD0toD3")
for (i in 1: length(analysis_ID)){
  if (i %% 1000== 0){print(i)}
  curr_id <- analysis_ID[i]
  Days_ECMO_IABP_MV_VAD_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  ecmo_res <- get_onMachine_flag_ICUD0_D3_v2(raw_ORGANSUPP_ECMO_df,All_time_df,curr_id,"ECMO_START_DATE","ECMO_STOP_DATE")
  
  IABP_res <- get_onMachine_flag_ICUD0_D3_v2(raw_ORGANSUPP_IABP_df,All_time_df,curr_id,"IABP_START_DATE","IABP_STOP_DATE")
  
  MV_res <- get_onMachine_flag_ICUD0_D3_v2(raw_ORGANSUPP_VENT_df,All_time_df,curr_id,"VENT_START_DATE","VENT_STOP_DATE")
  
  VAD_res <- get_onMachine_flag_ICUD0_D3_v2(raw_ORGANSUPP_VAD_df,All_time_df,curr_id,"VAD_START_DATE","VAD_STOP_DATE")
  
  
  #'@NOTE: If on Machine in ICU D0toD3, but START date = STOP date
  #'then1. update stop date == stop/start date at 23:59:59, 2. update on machine res
  updated_raw_ORGANSUPP_ECMO_df <- correct_STARTEqualEND(curr_id,ecmo_res,raw_ORGANSUPP_ECMO_df,"ECMO_START_DATE","ECMO_STOP_DATE")
  ecmo_res2 <- get_onMachine_flag_ICUD0_D3_v2(updated_raw_ORGANSUPP_ECMO_df,All_time_df,curr_id,"ECMO_START_DATE","ECMO_STOP_DATE")
  
  updated_raw_ORGANSUPP_IABP_df <- correct_STARTEqualEND(curr_id,IABP_res,raw_ORGANSUPP_IABP_df,"IABP_START_DATE","IABP_STOP_DATE")
  IABP_res2 <- get_onMachine_flag_ICUD0_D3_v2(updated_raw_ORGANSUPP_IABP_df,All_time_df,curr_id,"IABP_START_DATE","IABP_STOP_DATE")
  
  updated_raw_ORGANSUPP_VENT_df <- correct_STARTEqualEND(curr_id,MV_res,raw_ORGANSUPP_VENT_df,"VENT_START_DATE","VENT_STOP_DATE")
  MV_res2 <- get_onMachine_flag_ICUD0_D3_v2(updated_raw_ORGANSUPP_VENT_df,All_time_df,curr_id,"VENT_START_DATE","VENT_STOP_DATE")
  
  updated_raw_ORGANSUPP_VAD_df <- correct_STARTEqualEND(curr_id,VAD_res,raw_ORGANSUPP_VAD_df,"VAD_START_DATE","VAD_STOP_DATE")
  VAD_res2 <- get_onMachine_flag_ICUD0_D3_v2(updated_raw_ORGANSUPP_VAD_df,All_time_df,curr_id,"VAD_START_DATE","VAD_STOP_DATE")
  
  
  
  
  Days_ECMO_IABP_MV_VAD_df[i,"Days_ECMO_ICUD0toD3"] <- ecmo_res2[[2]]
  Days_ECMO_IABP_MV_VAD_df[i,"Days_IABP_ICUD0toD3"] <- IABP_res2[[2]]
  Days_ECMO_IABP_MV_VAD_df[i,"Days_MV_ICUD0toD3"]   <- MV_res2[[2]]
  Days_ECMO_IABP_MV_VAD_df[i,"Days_VAD_ICUD0toD3"]  <- VAD_res2[[2]]
}

#write.csv(Days_ECMO_IABP_MV_VAD_df,paste0(outdir,"All_ECMO_IABP_MV_VAD_Days_in_ICUD0toD3.csv"),row.names = F)
