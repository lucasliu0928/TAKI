library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/Taylors_Data/UKY/raw_csv_files/"
intermediate_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/"


##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#Charlson
raw_Charlson_df <- read.csv(paste0(raw_dir,"CHARLSON_SCORE.csv"),stringsAsFactors = F)

#Use elix to determine Diabetes and Hypertension
raw_ELIXHAUSER_df <- read.csv(paste0(raw_dir,"ELIXHAUSER_SCORE.csv"),stringsAsFactors = F)

#Use imputed eGFR for CKD (imputed means set to 75 )
baselineEGFR_df <- read.csv(paste0(intermediate_dir,"Baseline_EGFR.csv"),stringsAsFactors = F)

#Use Diagnosis terms for CKD 
CKD_df <- read.csv(paste0(intermediate_dir,"All_CKD_UseDiagTerms.csv"),stringsAsFactors = F)
##Use ICD codes for CKD
CKD_df2 <- read.csv(paste0(intermediate_dir,"All_CKD_UseDiagCodess.csv"),stringsAsFactors = F)


Final_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 8))
colnames(Final_df) <- c("STUDY_PATIENT_ID","CHARLSON_SCORE","TOTAL_ELIX",
                        "Diabetes","Hypertension","CKD","CKD_UseDiagTerms",
                        "CKD_UseICDCodes")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  Final_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  curr_charlson_df     <-  raw_Charlson_df[which(raw_Charlson_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_elix_df         <-  raw_ELIXHAUSER_df[which(raw_ELIXHAUSER_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_baselineEGFR_df <-  baselineEGFR_df[which(baselineEGFR_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_ckd_diag_df     <-  CKD_df[which(CKD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_ckd_diag_df2     <-  CKD_df2[which(CKD_df2[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Charlson score
  Final_df[i,"CHARLSON_SCORE"] <- curr_charlson_df[,"CHARLSON_INDEX"]
  #TOTAL Elix
  Final_df[i,"TOTAL_ELIX"] <- curr_elix_df[,"ELIXHAUSER_INDEX"]
  
  #Diabetes:     Elixhauser 11 and 12
  if (curr_elix_df[,"ELX_GRP_11"] == 1 | curr_elix_df[,"ELX_GRP_12"] == 1) {
    Final_df[i,"Diabetes"] <- 1
  }else{
    Final_df[i,"Diabetes"] <- 0
  }
  
  #Hypertension: Elixhauser 6 and 7 
  if (curr_elix_df[,"ELX_GRP_6"] == 1 | curr_elix_df[,"ELX_GRP_7"] == 1) {
    Final_df[i,"Hypertension"] <- 1
  }else{
    Final_df[i,"Hypertension"] <- 0
  }
  
  #CKD
  if(curr_baselineEGFR_df[,"Baseline_eGFR"] < 60){
    Final_df[i,"CKD"] <- 1
  }else{
    Final_df[i,"CKD"] <- 0
  }
  
  #CKD version 2: use diagnosis term
  if(curr_ckd_diag_df[,"CKD_UseDiagTerms"] == 1){
    Final_df[i,"CKD_UseDiagTerms"] <- 1
  }else{
    Final_df[i,"CKD_UseDiagTerms"] <- 0
  }
  
  #CKD version 3: use ICD codes
  if(curr_ckd_diag_df2[,"CKD_UseDiagCodes"] == 1){
    Final_df[i,"CKD_UseICDCodes"] <- 1
  }else{
    Final_df[i,"CKD_UseICDCodes"] <- 0
  }
  
}

write.csv(Final_df,paste0(outdir,"All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv"),row.names = F)
