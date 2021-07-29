library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#data dir
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #2233

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#on RRT alll time df
onHD_df <- All_time_df[which(is.na(All_time_df$Updated_HD_Start)==F),]
onCRRT_df <- All_time_df[which(is.na(All_time_df$Updated_CRRT_Start)==F),]

##########################################################################################
#Features to extract :  1. no RRT                                  (0)
#                       1. HD only in ICU D0-D3                    (1)
#                       2. CRRT with/with out HD in ICU D0-D3      (2)
##########################################################################################
onRRT_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 2))
colnames(onRRT_df) <- c("STUDY_PATIENT_ID", "onRRT_ICUD0toD3")
for (i in 1: length(analysis_ID)){
  if (i %% 1000== 0){print(i)}
  curr_id <- analysis_ID[i]
  onRRT_df[i,"STUDY_PATIENT_ID"] <- curr_id
  on_HD_ICUD0toD3 <- get_onMachine_flag_ICUD0_D3(onHD_df,All_time_df,curr_id,"Updated_HD_Start","Updated_HD_End")
  on_CRRT_ICUD0toD3 <- get_onMachine_flag_ICUD0_D3(onCRRT_df,All_time_df,curr_id,"Updated_CRRT_Start","Updated_CRRT_End")
 
  if (on_HD_ICUD0toD3 == 1 &  on_CRRT_ICUD0toD3 == 0){
    onRRT_df[i,"onRRT_ICUD0toD3"] <- 1  #HD only
  }else if( (on_HD_ICUD0toD3 == 0 | on_HD_ICUD0toD3 == 1) &  on_CRRT_ICUD0toD3 == 1) {
    onRRT_df[i,"onRRT_ICUD0toD3"] <- 2 #CRRT with or without HD
  }else if (on_HD_ICUD0toD3 == 0 &  on_CRRT_ICUD0toD3 == 0){
    onRRT_df[i,"onRRT_ICUD0toD3"] <- 0 #none
  }
}

table(onRRT_df$onRRT_ICUD0toD3)

#4. Compute missing
feature_columns <-  c("onRRT_ICUD0toD3")
missing_table <- get_missing_rate_table(onRRT_df,feature_columns)
missing_table

write.csv(onRRT_df,paste0(outdir,"All_onRRT_ICUD0toD3.csv"),row.names = F)

##########################################################################################
###Check if on RRT in ICU_D0_D3 get KDIGO = 4
#NOTE: All patient on RRT in ICU_D0_D3 have KDIGO = 4
#      one patient has KDIGO = 4 , but not acutally on RRT in ICU D0_D3, becuase the 48 hours extension effect for KDIGO
#      e.g, HD end 2009-09-19,  2009-09-19 is not in ICU D0_D3, but 2009-09-19+  48 hours is in ICU_D0_D3, so KDIGO score is 4
##########################################################################################
#'Check all on RRT inICU_D0_D3 must has KDIGO=4, not on RRT IDs must not have KDIGO=4
KDIGO_df <- read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)
KDIGO_df <- KDIGO_df[which(KDIGO_df$STUDY_PATIENT_ID %in% onRRT_df$STUDY_PATIENT_ID),]

onRRT_D0D3_IDs <- onRRT_df$STUDY_PATIENT_ID[which(onRRT_df$onRRT_ICUD0toD3 %in% c(1,2))]
KDIGO4_IDs <- KDIGO_df[which(KDIGO_df$MAX_KDIGO_ICU_D0toD3==4),"STUDY_PATIENT_ID"]

#check on RRT but not KDIGO= 4 (none)
which(!onRRT_D0D3_IDs %in% KDIGO4_IDs)
#check  KDIGO= 4 but not on RRT, because the 48 hours extension for KDIGO score
length(which(!KDIGO4_IDs %in% onRRT_D0D3_IDs)) #0
