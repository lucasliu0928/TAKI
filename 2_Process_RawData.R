library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"


##########################################################################################
#Load Raw Data
##########################################################################################


#8.Load raw data
raw_BLOOD_GAS_df <- read.csv(paste0(raw_dir,"BLOOD_GAS.csv"),stringsAsFactors = F)

#9.Load raw data
raw_CLINICAL_OTHERS_df <- read.csv(paste0(raw_dir,"CLINICAL_OTHERS.csv"),stringsAsFactors = F)

#10. Load raw Charlson data
raw_charlson_df <- read.csv(paste0(raw_dir,"CHARLSON_SCORE.csv"),stringsAsFactors = F)
#11. Load raw ELIXHAUSER_SCORE data
raw_ELIXHAUSER_df <- read.csv(paste0(raw_dir,"ELIXHAUSER_SCORE.csv"),stringsAsFactors = F)

#12. Load raw DIAGNOSIS data
raw_DIAGNOSIS_df <- read.csv(paste0(raw_dir,"DIAGNOSIS.csv"),stringsAsFactors = F)
sepsis_check_df <- raw_DIAGNOSIS_df[which(grepl("sepsis",raw_DIAGNOSIS_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]
unique_sepsis_df <- as.data.frame(unique(sepsis_check_df$DIAGNOSIS_DESC))
write.csv(unique_sepsis_df,paste0(outdir,"sepsis_TAKI_UKY.csv"))

#septic
#check <- raw_DIAGNOSIS_df[which(grepl("sepsis",raw_DIAGNOSIS_df$DIAGNOSIS_DESC,ignore.case = T)),]


#13.ORGANSUPP_VENT
raw_ORGANSUPP_VENT_df <- read.csv(paste0(raw_dir,"ORGANSUPP_VENT.csv"),stringsAsFactors = F)

#14.ORGANSUPP_ECMO
raw_ORGANSUPP_ECMO_df <- read.csv(paste0(raw_dir,"ORGANSUPP_ECMO.csv"),stringsAsFactors = F)

#15.ORGANSUPP_IABP
raw_ORGANSUPP_IABP_df <- read.csv(paste0(raw_dir,"ORGANSUPP_IABP.csv"),stringsAsFactors = F)

#16.ORGANSUPP_VAD
raw_ORGANSUPP_VAD_df <- read.csv(paste0(raw_dir,"ORGANSUPP_VAD.csv"),stringsAsFactors = F)

#17.MEDICATIONS_INDX
raw_MEDICATIONS_df <- read.csv(paste0(raw_dir,"MEDICATIONS_INDX.csv"),stringsAsFactors = F)


#18.IO
raw_IO_df <- read.csv(paste0(raw_dir,"IO_TOTALS.csv"),stringsAsFactors = F)

#19. URINE_OUTPUT
raw_URINE_OUTPUT_df <- read.csv(paste0(raw_dir,"URINE_OUTPUT.csv"),stringsAsFactors = F)

#20. SURGERY_INDX
raw_SURGERY_INDX_df <- read.csv(paste0(raw_dir,"SURGERY_INDX.csv"),stringsAsFactors = F)


#21. ESRD_STATUS
raw_ESRD_STATUS_Manually_df <- read.csv(paste0(raw_dir,"ESRD_MANUAL_REVISION.csv"),stringsAsFactors = F)
colnames(raw_ESRD_STATUS_Manually_df)[2:4] <- paste0("Manual_",colnames(raw_ESRD_STATUS_Manually_df)[2:4])

raw_USRDS_ESRD_DATE_df <- read.csv(paste0(raw_dir,"USRDS_ESRD.csv"),stringsAsFactors = F)
colnames(raw_USRDS_ESRD_DATE_df)[2] <- paste0("USRDS_",colnames(raw_USRDS_ESRD_DATE_df)[2])

raw_ESRD_STATUS_df <- read.csv(paste0(raw_dir,"ESRD_STATUS.csv"),stringsAsFactors = F)
colnames(raw_ESRD_STATUS_df)[2:5] <- paste0("SCM_",colnames(raw_ESRD_STATUS_df)[2:5])
colnames(raw_ESRD_STATUS_df)[2:5]

#22. OUTCOMES_COMBINED (DOD)
raw_OUTCOMES_COMBINED <-read.csv(paste0(raw_dir,"OUTCOMES_COMBINED.csv"),stringsAsFactors = F)

#23. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Timeinfo_df.csv"),stringsAsFactors = F)

##########################################################################################
#anlaysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])



##########################################################################################
#Load UK raw BLOOD_GAS
#Features to extract :  1. pO2 D1 High/Low
#                       2. pCO2 D1 High/Low
#                       3. pH D1 High/Low
##########################################################################################
#get features
po2_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PO2_D1_LOW","PO2_D1_LOW_VALUE")
po2_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PO2_D1_HIGH","PO2_D1_HIGH_VALUE")

pco2_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PCO2_D1_LOW","PCO2_D1_LOW_VALUE")
pco2_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PCO2_D1_HIGH","PCO2_D1_HIGH_VALUE")

ph_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PH_D1_LOW","PH_D1_LOW_VALUE")
ph_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PH_D1_HIGH","PH_D1_HIGH_VALUE")


All_BloodGAS_df <- cbind(po2_d1_Low,po2_d1_High,pco2_d1_Low,pco2_d1_High,ph_d1_Low,ph_d1_High)
All_BloodGAS_df <- All_BloodGAS_df[,-seq(3,11,2)] #remove duplicated columns
write.csv(All_BloodGAS_df,paste0(outdir,"All_BloodGAS_df.csv"))


##########################################################################################
#Load UK raw CLINICAL_OTHERS
#Features to extract :  1. Height
#                       2. Weight
#                       3. Respiration D1 Low/High
#                       4. GCS D1 LOW
#                       5. FIO2 D1 Low/High
##########################################################################################
#Get Features
height <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"HEIGHT_CM","HEIGHT_CM_VALUE")
weight <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"INITIAL_WEIGHT_KG","INITIAL_WEIGHT_KG")

resp_d1_Low <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"RESP_RATE_D1_LOW","RESP_RATE_D1_LOW_VALUE")
resp_d1_High <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"RESP_RATE_D1_HIGH","RESP_RATE_D1_HIGH_VALUE")

gcs_d1_low <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"GCS_D1_LOW","GLASGOW_SCORE_D1_LOW_VALUE")

fio2_d1_Low <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"FI02_D1_LOW","FI02_D1_LOW_VALUE")
fio2_d1_High <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"FI02_D1_HIGH","FI02_D1_HIGH_VALUE")

All_ClinicalOthers_df <- cbind(height,weight,resp_d1_Low,resp_d1_High,gcs_d1_low,fio2_d1_Low,fio2_d1_High)
All_ClinicalOthers_df <- All_ClinicalOthers_df[,-seq(3,13,2)] #remove duplicated columns
write.csv(All_ClinicalOthers_df,paste0(outdir,"All_ClinicalOthers_df.csv"))



##########################################################################################
#Load UK raw CHARLSON_SCORE
#Features to extract :  1. Charlson
##########################################################################################
#Get Features
CHARLSON_INDEX_df <- get_raw_var_values_1option_func(raw_charlson_df,analysis_ID,"CHARLSON_INDEX","CHARLSON_INDEX")
write.csv(CHARLSON_INDEX_df,paste0(outdir,"All_CHARLSON_INDEX_df.csv"))


##########################################################################################
#Load UK raw ELIXHAUSER_SCORE
#Features to extract :  1. ELIXHAUSER_SCORE
##########################################################################################
colnames(raw_ELIXHAUSER_df)[2:32] <- gsub("ELX","Elixhauser",colnames(raw_ELIXHAUSER_df)[2:32])
ELIXHAUSER_df <- raw_ELIXHAUSER_df[which(raw_ELIXHAUSER_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
write.csv(ELIXHAUSER_df,paste0(outdir,"All_ELIXHAUSER_df.csv"))

#'@TODO
##########################################################################################
#Load UK raw DIAGNOSIS
#Features to extract :  1. DIAGNOSIS
##########################################################################################
DIAGNOSIS_df  <- raw_DIAGNOSIS_df[which(raw_DIAGNOSIS_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
write.csv(DIAGNOSIS_df,paste0(outdir,"All_DIAGNOSIS_df.csv"))



##########################################################################################
#Load UK raw ORGANSUPP_VENT
#Load UK raw ORGANSUPP_ECMO
#Load UK raw ORGANSUPP_IABP
#Load UK raw ORGANSUPP_VAD
##########################################################################################
MV_df  <- raw_ORGANSUPP_VENT_df[which(raw_ORGANSUPP_VENT_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
write.csv(MV_df,paste0(outdir,"All_MV_df.csv"))


ECMO_df  <- raw_ORGANSUPP_ECMO_df[which(raw_ORGANSUPP_ECMO_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
write.csv(ECMO_df,paste0(outdir,"All_ECMO_df.csv"))


IABP_df <- get_vars_for_analysisId_func(raw_ORGANSUPP_IABP_df,analysis_ID)
write.csv(IABP_df,paste0(outdir,"All_IABP_df.csv"))


VAD_df <- get_vars_for_analysisId_func(raw_ORGANSUPP_VAD_df,analysis_ID)
write.csv(VAD_df,paste0(outdir,"All_VAD_df.csv"))

##########################################################################################
#Load UK raw MEDICATIONS_INDX
##########################################################################################
MEDICATIONS_df <- get_vars_for_analysisId_func(raw_MEDICATIONS_df,analysis_ID)
write.csv(MEDICATIONS_df,paste0(outdir,"All_MEDICATIONS_df.csv"))

##########################################################################################
#Load UK raw IO_TOTALS
##########################################################################################
raw_IO_df <- get_vars_for_analysisId_func(raw_IO_df,analysis_ID)

raw_IO_df$FluidOverload <- NA
in_col_names <- colnames(raw_IO_df)[which(grepl("IN",colnames(raw_IO_df))==T)]
out_col_names <- colnames(raw_IO_df)[which(grepl("OUT",colnames(raw_IO_df))==T)]

for (i in 1:nrow(raw_IO_df)){
  curr_id <- raw_IO_df[i,"STUDY_PATIENT_ID"]
  
  curr_weight <- weight[which(weight[,"STUDY_PATIENT_ID"] == curr_id),"INITIAL_WEIGHT_KG"]
  curr_TOTAL_in <- sum(raw_IO_df[i,in_col_names],na.rm = T)
  curr_TOTAL_out <- sum(raw_IO_df[i,out_col_names],na.rm = T)
  
  if (is.na(curr_weight) == F & is.na(curr_TOTAL_in) == F & is.na(curr_TOTAL_out) == F){
    curr_val <- (curr_TOTAL_in - curr_TOTAL_out)/(curr_weight*100)
  }else{
    curr_val <- NA
  }
  raw_IO_df[i,"FluidOverload"] <- curr_val
}

IO_df <- raw_IO_df[,c("STUDY_PATIENT_ID", "FluidOverload")]
write.csv(IO_df,paste0(outdir,"All_IO_df.csv"))

##########################################################################################
#'@TODO
#Load UK raw URINE_OUTPUT
##########################################################################################
raw_URINE_OUTPUT_df <- get_vars_for_analysisId_func(raw_URINE_OUTPUT_df,analysis_ID)



##########################################################################################
#Combine the three ESRD sources
##########################################################################################
combined_ESRD_df <- raw_ESRD_STATUS_Manually_df
combined_ESRD_df$USRDS_ESRD_DATE <- NA
combined_ESRD_df$SCM_BEFORE_INDEXED_INDICATOR <- NA
combined_ESRD_df$SCM_AT_ADMISSION_INDICATOR <- NA
combined_ESRD_df$SCM_DURING_INDEXED_INDICATOR <- NA
combined_ESRD_df$SCM_AFTER_INDEXED_INDICATOR <- NA
for (i in 1:nrow(combined_ESRD_df)){
  curr_id <- combined_ESRD_df[i,"STUDY_PATIENT_ID"]
  
  #Get date
  curr_idxes_inUSRDdate <- which(raw_USRDS_ESRD_DATE_df[,"STUDY_PATIENT_ID"] == curr_id)
  if (length(curr_idxes_inUSRDdate) > 0){
    combined_ESRD_df[i,"USRDS_ESRD_DATE"] <- raw_USRDS_ESRD_DATE_df[curr_idxes_inUSRDdate,"USRDS_ESRD_DATE"]
  }
  
  curr_idxes_inStatus <- which(raw_ESRD_STATUS_df[,"STUDY_PATIENT_ID"] == curr_id)
  if (length(curr_idxes_inStatus) > 0){
    combined_ESRD_df[i,"SCM_BEFORE_INDEXED_INDICATOR"] <- raw_ESRD_STATUS_df[curr_idxes_inStatus,"SCM_BEFORE_INDEXED_INDICATOR"]
    combined_ESRD_df[i,"SCM_AT_ADMISSION_INDICATOR"] <- raw_ESRD_STATUS_df[curr_idxes_inStatus,"SCM_AT_ADMISSION_INDICATOR"]
    combined_ESRD_df[i,"SCM_DURING_INDEXED_INDICATOR"] <- raw_ESRD_STATUS_df[curr_idxes_inStatus,"SCM_DURING_INDEXED_INDICATOR"]
    combined_ESRD_df[i,"SCM_AFTER_INDEXED_INDICATOR"] <- raw_ESRD_STATUS_df[curr_idxes_inStatus,"SCM_AFTER_INDEXED_INDICATOR"]
  }
}



# # ##########################################################################################
# # #Load anlaysis Id after exclusion
# # ##########################################################################################
# #Load UK exclusion
# UK_exclusion_df <- read.csv(paste0(taylor_UK_data_dir,"exclusion_criteria_taylor.csv"),stringsAsFactors = F)
# UK_ID_left_df <- UK_exclusion_df[which(rowSums(UK_exclusion_df[,2:ncol(UK_exclusion_df)])==0),] #keep pt without "1" in any of the column
# analysis_ID <- unique(UK_ID_left_df$STUDY_PATIENT_ID)

