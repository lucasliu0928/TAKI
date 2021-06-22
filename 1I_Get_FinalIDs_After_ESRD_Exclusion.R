library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Analysis Id before exclusion of ESRD
##########################################################################################
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID_BeforeExclusionOfESRD.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#2. ESRD before at df
##########################################################################################
Final_ESRD_BEFORE_AT_df <- read.csv(paste0(outdir,"ESRD_Before_AT.csv"),stringsAsFactors = F)


##########################################################################################
#Exclusion
##########################################################################################
exclude_pts_func <-function(analysis_IDs,exclusion_IDs){
  remove_idx <- which(analysis_IDs %in% exclusion_IDs) 
  if (length(remove_idx) > 0 ){ #if analysis_IDs has any ID qualifies exclusion
    acutal_exclusion_ID <- analysis_IDs[remove_idx] #The IDs in anlaysis Id qualifies exlucsion criteria
    updated_analysis_IDs <- analysis_IDs[-remove_idx]
  }else{ #remove nothing
    acutal_exclusion_ID <-NULL
    updated_analysis_IDs <- analysis_IDs
  }
  return(list(acutal_exclusion_ID,updated_analysis_IDs))
}

#Exclude 8- ESRD before/AT hosp
ExclusionID8 <- Final_ESRD_BEFORE_AT_df[which(Final_ESRD_BEFORE_AT_df[,"ESRD_BEFORE_AT"] == 1),"STUDY_PATIENT_ID"]
res <- exclude_pts_func(analysis_ID,ExclusionID8)
actual_exclusion_IDs8 <- res[[1]] 
updated_inclusion_IDs8 <- res[[2]]
length(actual_exclusion_IDs8) #447
length(updated_inclusion_IDs8) #7354

#Final analysis ID 
Final_Anlaysis_ID <-as.data.frame(updated_inclusion_IDs8)
colnames(Final_Anlaysis_ID) <- "STUDY_PATIENT_ID"
nrow(Final_Anlaysis_ID) #7354
write.csv(Final_Anlaysis_ID,paste0(outdir,"Final_Analysis_ID.csv"),row.names = F)


