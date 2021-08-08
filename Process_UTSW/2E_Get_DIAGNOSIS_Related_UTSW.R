library(lubridate)
source("TAKI_Ultility.R")

get_sepsis_func <- function(sepsis_df, dia_col){
  # dia_col <- "DX_NAME"
  # sepsis_df <- raw_DIAGNOSIS_df1
  
  #inclusion term
  sepsis_df <- sepsis_df[which(grepl("sep",sepsis_df[,dia_col],ignore.case = T) ==T),]
  
  #exclusion term
  exc_idxes1 <- which(grepl("asep",sepsis_df[,dia_col],ignore.case = T) ==T)
  exc_idxes2 <- which(grepl("separation",sepsis_df[,dia_col],ignore.case = T) ==T)
  exc_idxes3 <- which(grepl("septal",sepsis_df[,dia_col],ignore.case = T) ==T)
  exc_idxes4 <- which(grepl("SEPTOPLASTY",sepsis_df[,dia_col],ignore.case = T) ==T)

  all_exc <- unique(c(exc_idxes1,exc_idxes2,exc_idxes3,exc_idxes4))

  if (length(all_exc) > 0){
    sepsis_df <-  sepsis_df[-all_exc,]
  }else{
    sepsis_df <- sepsis_df
  }
  return(sepsis_df)
}


#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #2233

#3. Load raw DIAGNOSIS data
raw_DIAGNOSIS_df1 <- read.csv(paste0(raw_dir,"tHospitalAdmissionDiagnoses.csv"),stringsAsFactors = F)
raw_DIAGNOSIS_df2 <- read.csv(paste0(raw_dir,"tHospitalFinalDiagnoses.csv"),stringsAsFactors = F)

#4. Get sepsis df
sepsis_df1 <- get_sepsis_func(raw_DIAGNOSIS_df1,"DX_NAME") 
sepsis_df2 <- get_sepsis_func(raw_DIAGNOSIS_df2,"DX_NAME")

##Before or at admit
#Every Id in sepsis df and part of sepsis_df 2 (All IDs in sepsis df1 are also in sepsis df 2)
sepsis_df2 <- sepsis_df2[which(sepsis_df2$DX_POA_YNU == "Y"),]

sepsis_beforeat_IDs <- unique(c(sepsis_df1[,"PATIENT_NUM"], sepsis_df2[,"PATIENT_NUM"]))

##########################################################################################
#Features to extract : 1. Septic before/at admission
##########################################################################################
#1. Get raw available values
Final_sepsis_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2 ))
colnames(Final_sepsis_df) <- c("STUDY_PATIENT_ID","Sepsis_Before_or_At_Admission")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  Final_sepsis_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  if (curr_id %in% sepsis_beforeat_IDs){
    Final_sepsis_df[i,"Sepsis_Before_or_At_Admission"] <- 1
  }else{
    Final_sepsis_df[i,"Sepsis_Before_or_At_Admission"] <- 0
  }
}

table(Final_sepsis_df$Sepsis_Before_or_At_Admission) #1777 456   

write.csv(Final_sepsis_df,paste0(outdir,"All_sepsis_Before_Or_At_Admission.csv"),row.names = F)
