library(lubridate)

#Raw data dir
intermediate_dir1 <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/xilong_extracted/"
intermediate_dir2 <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"


##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #2233

#Charlson and Elix
raw_CharlsonAndElix_df <- read.csv(paste0(intermediate_dir1,"All variables for each patients 07212021.csv"),stringsAsFactors = F)

#Change Elixhaser name to the orignal name(1-31) instead of 0-30
Elix_indxes <- which(colnames(raw_CharlsonAndElix_df) %in% paste0("Elixhauser_",seq(0,30),"_F"))
colnames(raw_CharlsonAndElix_df)[Elix_indxes] <- paste0("ELX_GRP_",seq(1,31))

#Code Elix NA as 0
elix_col_names <- paste0("ELX_GRP_",seq(1,31))
raw_CharlsonAndElix_df[which(is.na(raw_CharlsonAndElix_df[,elix_col_names]==T),arr.ind = T),elix_col_names] <- 0


#Use imputed eGFR for CKD (imputed means set to 75 )
baselineEGFR_df <- read.csv(paste0(intermediate_dir2,"Baseline_EGFR.csv"),stringsAsFactors = F)


Final_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 6))
colnames(Final_df) <- c("STUDY_PATIENT_ID","CHARLSON_SCORE","TOTAL_ELIX","Diabetes","Hypertension","CKD")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  Final_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  curr_charlsonAndElix_df     <-  raw_CharlsonAndElix_df[which(raw_CharlsonAndElix_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_baselineEGFR_df <-  baselineEGFR_df[which(baselineEGFR_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Charlson score
  Final_df[i,"CHARLSON_SCORE"] <- curr_charlsonAndElix_df[,"charlson_F"]
  
  #TOTAL Elix
  Final_df[i,"TOTAL_ELIX"] <- sum(curr_charlsonAndElix_df[,paste0("ELX_GRP_",seq(1,31))])
  
  #Diabetes:     Elixhauser 11 and 12
  if (curr_charlsonAndElix_df[,"ELX_GRP_11"] == 1 | curr_charlsonAndElix_df[,"ELX_GRP_12"] == 1) {
    Final_df[i,"Diabetes"] <- 1
  }else{
    Final_df[i,"Diabetes"] <- 0
  }
  
  #Hypertension: Elixhauser 6 and 7 
  if (curr_charlsonAndElix_df[,"ELX_GRP_6"] == 1 | curr_charlsonAndElix_df[,"ELX_GRP_7"] == 1) {
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
}

write.csv(Final_df,paste0(outdir,"All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv"),row.names = F)
