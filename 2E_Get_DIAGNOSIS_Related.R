library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#3. Load raw DIAGNOSIS data
raw_DIAGNOSIS_df <- read.csv(paste0(raw_dir,"DIAGNOSIS.csv"),stringsAsFactors = F)

##########################################################################################
#Get sepsis df
#'@Updated 080621 
#'##########################################################################################
#All Sepsis
sepsis_df <- raw_DIAGNOSIS_df[which(grepl("sep",raw_DIAGNOSIS_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]

#Exclude "asep"
sepsis_df <- sepsis_df[-which(grepl("asep",sepsis_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]

sepsis_df <- sepsis_df[-which(grepl("separation",sepsis_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]

sepsis_df <- sepsis_df[-which(grepl("septal",sepsis_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]

sepsis_df <- sepsis_df[-which(grepl("SEPTOPLASTY",sepsis_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]

#All sepsis
#all_sepsis_IDs <- unique(sepsis_df$STUDY_PATIENT_ID)

##Before or at admit
sepsis_df <- sepsis_df[which(sepsis_df$PRES_AT_ADMIT == "Y"),]
sepsis_beforeat_IDs <- unique(sepsis_df$STUDY_PATIENT_ID)

#' ##########################################################################################
#' #'@Added010722             #CKD
#' #'##########################################################################################
#' #Report CKD unique items
#' #CKD_df <- raw_DIAGNOSIS_df[which(grepl("ckd|chronic kidney disease",raw_DIAGNOSIS_df[,"DIAGNOSIS_DESC"],ignore.case = T) ==T),]
#' #unique_itemname_CKD_df <- CKD_df[duplicated(CKD_df[,"DIAGNOSIS_DESC"]) == F,c("DIAGNOSIS_DESC","ICD_CODE","ICD_TYPECODE")]
#' #write.csv(unique_itemname_CKD_df,paste0(outdir,"CKD_Diagnosis_NAMEs_And_ICDCodes.csv"),row.names = F)
#' 
#' #Load CKD selected terms 
#' ##'@IMPPORTNOTE: 
#' #'@Do use selected terms to find CKD diagnoiss
#' #'@Donot use ICD codes that corresponding to the row selected, 
#' #'@because some diagnoisi correpoding to multiple codes, and not all the codes related to CKD
#' #'@Example: in USTW data: I50.9 in diagnosis df correponding to Congestive heart failure, unspecified	
#' #'          I50.9 also exsits as one of codes in unique item df for  
#' #'          Malignant hypertensive heart and kidney disease with heart failure and with chronic kidney disease stage I through stage IV, or unspecified
#' CKD_def_df  <- read.xlsx(paste0(outdir, "uky_ckd.xlsx"),sheet = 1)
#' CKD_def_df_selected <- CKD_def_df[which(CKD_def_df[,"Selected"] == 1),]
#' selected_terms <- unique(CKD_def_df_selected[,"DIAGNOSIS_DESC"])
#' 
#' #CKD df
#' CKD_df <- raw_DIAGNOSIS_df[which(raw_DIAGNOSIS_df[,"DIAGNOSIS_DESC"] %in% selected_terms),]
#' 
#' #Check if we get all selecetd terms in CKD df
#' setdiff(unique(CKD_df$DIAGNOSIS_DESC),selected_terms)
#' setdiff(selected_terms,unique(CKD_df$DIAGNOSIS_DESC))
#' 
#' #CKD IDs
#' CKD_IDs <- unique(CKD_df$STUDY_PATIENT_ID) 


##########################################################################################
#'@Added010722             #CKD Version 2 using ICD codes
#'ICD9:   585.X
#'ICD10 : N18.X
#'##########################################################################################
icd9_ckd <- paste0("585.",seq(1,9))
icd10_ckd <- paste0("N18.",seq(1,9))

CKD_df <- raw_DIAGNOSIS_df[which(raw_DIAGNOSIS_df[,"ICD_CODE"] %in% c(icd9_ckd,icd10_ckd)),]
CKD_IDs <- unique(CKD_df$STUDY_PATIENT_ID) 

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

table(Final_sepsis_df$Sepsis_Before_or_At_Admission) #5393 1942  

write.csv(Final_sepsis_df,paste0(outdir,"All_sepsis_Before_Or_At_Admission.csv"),row.names = F)


##########################################################################################
#Features to extract : 2.CKD by selected terms
##########################################################################################
#1. Get raw available values
Final_CKD_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2 ))
colnames(Final_CKD_df) <- c("STUDY_PATIENT_ID","CKD_UseDiagCodes")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  Final_CKD_df[i,"STUDY_PATIENT_ID"] <- curr_id

  if (curr_id %in% CKD_IDs){
    Final_CKD_df[i,"CKD_UseDiagCodes"] <- 1
  }else{
    Final_CKD_df[i,"CKD_UseDiagCodes"] <- 0
  }
}

table(Final_CKD_df$CKD_UseDiagCodes) #5154 2200 

write.csv(Final_CKD_df,paste0(outdir,"All_CKD_UseDiagCodess.csv"),row.names = F)
