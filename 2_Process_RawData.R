library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"


##########################################################################################
#Load Raw Data
##########################################################################################

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



#17.MEDICATIONS_INDX
raw_MEDICATIONS_df <- read.csv(paste0(raw_dir,"MEDICATIONS_INDX.csv"),stringsAsFactors = F)


#18.IO
raw_IO_df <- read.csv(paste0(raw_dir,"IO_TOTALS.csv"),stringsAsFactors = F)

#19. URINE_OUTPUT
raw_URINE_OUTPUT_df <- read.csv(paste0(raw_dir,"URINE_OUTPUT.csv"),stringsAsFactors = F)

#20. SURGERY_INDX
raw_SURGERY_INDX_df <- read.csv(paste0(raw_dir,"SURGERY_INDX.csv"),stringsAsFactors = F)

##########################################################################################
#anlaysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

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


