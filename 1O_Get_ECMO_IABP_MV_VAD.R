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


intersect(raw_ORGANSUPP_ECMO_df$STUDY_PATIENT_ID,All_time_df$STUDY_PATIENT_ID)

##########################################################################################
#Features to extract :  1. ECMO in ICU D0-D3
#                       2. IABP in ICU D0-D3
#                       3. MV in ICU D0-D3
#                       4. VAD in ICU D0-D3
#                       5. Mechanical Hemodynamic Support: Use of VAD, IABP, or ECMO in ICU D0-D3
##########################################################################################
get_onMachine_flag_ICUD0_D3 <- function(machine_df,time_df,pt_id,start_t_col,stop_t_col){
  # machine_df <- raw_ORGANSUPP_ECMO_df
  # pt_id <- curr_id
  # start_t_col <- "ECMO_START_DATE"
  # stop_t_col <- "ECMO_STOP_DATE"
  # time_df <- All_time_df 
  #ICU start
  curr_time_df <-  time_df[which(time_df[,"STUDY_PATIENT_ID"] == pt_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  
  #Get actual days/times in ICU D0-D3
  #it could be ICU end time (e.g, if ICU stays < 3 days) or the end of ICU D3
  curr_actual_ICU_time_idxes <- which(colnames(curr_time_df) %in% c("Actual_D0_End","Actual_D1_End","Actual_D2_End","Actual_D3_End"))
  curr_actual_ICU_time <- curr_time_df[,curr_actual_ICU_time_idxes]
  curr_last_ICU_time <- max(ymd_hms(curr_actual_ICU_time),na.rm = T)
  
  ICU_interval <-interval(curr_icu_start,curr_last_ICU_time)
  
  
  curr_df <- machine_df[which(machine_df[,"STUDY_PATIENT_ID"] == pt_id),]
  if (nrow(curr_df) !=0 ){ #if pt is ever on machine 
    #get on machine start and end time 
    curr_machine_start <- ymd_hms(curr_df[,start_t_col])
    curr_machine_end <-  ymd_hms(curr_df[,stop_t_col])
    
    machine_interval <-interval(curr_machine_start,curr_machine_end)
    
    if(int_overlaps(ICU_interval, machine_interval)==T){
      on_flag <- 1 
      
    }else{
      on_flag <- 0 
    }
    
  }else{
    on_flag <- 0 
  }
  
  return(on_flag)
}

#362 ID on
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
