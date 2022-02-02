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
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/"

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

#' ##########################################################################################
#' #'@Added010722             #CKD
#' #'##########################################################################################
#' #Report CKD unique items
#' # CKD_df1 <- raw_DIAGNOSIS_df1[which(grepl("ckd|chronic kidney disease",raw_DIAGNOSIS_df1[,"DX_NAME"],ignore.case = T) ==T),]
#' # CKD_df2 <- raw_DIAGNOSIS_df2[which(grepl("ckd|chronic kidney disease",raw_DIAGNOSIS_df2[,"DX_NAME"],ignore.case = T) ==T),]
#' # colnames(CKD_df2)[which(colnames(CKD_df2) == "CURRENT_ICD9_LIST")] <- "ICD_9"
#' # colnames(CKD_df2)[which(colnames(CKD_df2) == "CURRENT_ICD10_LIST")] <- "ICD_10"
#' # 
#' # CKD_df_comb <- rbind(CKD_df1[,c("PATIENT_NUM","DX_NAME","ICD_9","ICD_10")], 
#' #                      CKD_df2[,c("PATIENT_NUM","DX_NAME","ICD_9","ICD_10")])
#' # unique_itemname_CKD_df <- CKD_df_comb[duplicated(CKD_df_comb[,c("DX_NAME")])==F,c("DX_NAME","ICD_9","ICD_10")]
#' #write.csv(unique_itemname_CKD_df,paste0(outdir,"CKD_Diagnosis_NAMEs_And_ICDCodes.csv"),row.names = F)
#' 
#' #Load CKD selected terms 
#' ##'@IMPPORTNOTE: 
#' #'@Do use selected terms to find CKD diagnoiss
#' #'@Donot use ICD codes that corresponding to the row selected, 
#' #'@because some diagnoisi correpoding to multiple codes, and not all the codes related to CKD
#' #'@Example: I50.9 in diagnosis df correponding to Congestive heart failure, unspecified	
#' #'          I50.9 also exsits as one of codes in unique item df for  
#' #'          Malignant hypertensive heart and kidney disease with heart failure and with chronic kidney disease stage I through stage IV, or unspecified
#' CKD_def_df  <- read.xlsx(paste0(outdir, "utsw_ckd.xlsx"),sheet = 1)
#' CKD_def_df_selected <- CKD_def_df[which(CKD_def_df[,"Selected"] == 1),]
#' selected_terms <- unique(CKD_def_df_selected[,"DX_NAME"])
#' 
#' #CKD df
#' CKD_df1 <- raw_DIAGNOSIS_df1[which(raw_DIAGNOSIS_df1[,"DX_NAME"] %in% selected_terms),]
#' CKD_df2 <- raw_DIAGNOSIS_df2[which(raw_DIAGNOSIS_df2[,"DX_NAME"] %in% selected_terms),]
#' 
#' #Check if we get all selecetd terms in CKD df
#' setdiff(unique(c(CKD_df1$DX_NAME,CKD_df2$DX_NAME)),selected_terms)
#' setdiff(selected_terms,unique(c(CKD_df1$DX_NAME,CKD_df2$DX_NAME)))
#' 
#' #CKD pateints
#' CKD_IDs1 <- unique(CKD_df1$PATIENT_NUM)
#' CKD_IDs2 <- unique(CKD_df2$PATIENT_NUM)
#' CKD_IDs  <- unique(c(CKD_IDs1,CKD_IDs2))

##########################################################################################
#'@Added010722             #CKD Version 2 using ICD codes
#'ICD9:   585.X
#'ICD10 : N18.X
#'##########################################################################################
icd9_ckd <- paste0("585.",seq(1,9))
icd10_ckd <- paste0("N18.",seq(1,9))

CKD_df1 <- raw_DIAGNOSIS_df1[which(grepl(paste0(icd9_ckd,collapse = "|"),raw_DIAGNOSIS_df1[,"ICD_9"])==T),]
CKD_df2 <- raw_DIAGNOSIS_df1[which(grepl(paste0(icd10_ckd,collapse = "|"),raw_DIAGNOSIS_df1[,"ICD_10"])==T),]
CKD_df3 <- raw_DIAGNOSIS_df2[which(grepl(paste0(icd9_ckd,collapse = "|"),raw_DIAGNOSIS_df2[,"CURRENT_ICD9_LIST"])==T),]
CKD_df4 <- raw_DIAGNOSIS_df2[which(grepl(paste0(icd10_ckd,collapse = "|"),raw_DIAGNOSIS_df2[,"CURRENT_ICD10_LIST"])==T),]

CKD_IDs1 <- unique(CKD_df1$PATIENT_NUM) 
CKD_IDs2 <- unique(CKD_df2$PATIENT_NUM) 
CKD_IDs3 <- unique(CKD_df3$PATIENT_NUM) 
CKD_IDs4 <- unique(CKD_df4$PATIENT_NUM) 
CKD_IDs  <- unique(c(CKD_IDs1,CKD_IDs2,CKD_IDs3,CKD_IDs4))

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

table(Final_CKD_df$CKD_UseDiagCodes) #1356 877 

write.csv(Final_CKD_df,paste0(outdir,"All_CKD_UseDiagCodes.csv"),row.names = F)
