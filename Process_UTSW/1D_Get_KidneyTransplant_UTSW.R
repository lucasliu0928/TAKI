library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UTSW/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1.Load inclusion IDs
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#2. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#3. Kidney Transpant at admission
KIDT_AT_df <- read.csv(paste0(raw_dir,"tHospitalAdmissionDiagnoses.csv"),stringsAsFactors = F)
KIDT_AT_df <- KIDT_AT_df[which(grepl("kid",KIDT_AT_df[,"DX_NAME"],ignore.case = T) == T),]
KIDT_AT_df <- KIDT_AT_df[which(grepl("trans",KIDT_AT_df[,"DX_NAME"],ignore.case = T) == T),]
colnames(KIDT_AT_df)[which(colnames(KIDT_AT_df) == "PATIENT_NUM")] <- "STUDY_PATIENT_ID"

#3. Kidney Transpant during admission
KIDT_During_df <- read.csv(paste0(raw_dir,"tHospitalFinalDiagnoses.csv"),stringsAsFactors = F)
KIDT_During_df <- KIDT_During_df[which(grepl("kid",KIDT_During_df[,"DX_NAME"],ignore.case = T) == T),]
KIDT_During_df <- KIDT_During_df[which(grepl("trans",KIDT_During_df[,"DX_NAME"],ignore.case = T) == T),]
colnames(KIDT_During_df)[which(colnames(KIDT_During_df) == "PATIENT_NUM")] <- "STUDY_PATIENT_ID"

  
##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(Inclusion_df[,"STUDY_PATIENT_ID"]) #10503

##########################################################################################
#3. make sure the kidney transplant is in current hosp admission
##########################################################################################
#1.KidT at
KIDT_AT_df <- KIDT_AT_df[which(KIDT_AT_df$STUDY_PATIENT_ID %in% analysis_ID),]
KIDT_AT_df$inCurrentAdmission <- NA
for (i in 1:nrow(KIDT_AT_df)){
  curr_id <- KIDT_AT_df[i,"STUDY_PATIENT_ID"]
  
  curr_hosp_start_inKidT <- ymd(strsplit(KIDT_AT_df[i,"HOSP_ADMSN_TIME"],split = " ")[[1]][1]) #only get ymd 
  curr_hosp_end_inKidT   <- ymd(strsplit(KIDT_AT_df[i,"HOSP_DISCH_TIME"],split = " ")[[1]][1])
  
  
  curr_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID == curr_id),]
  
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd 
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  
  if (curr_hosp_start_inKidT == curr_hosp_start & curr_hosp_end==curr_hosp_end_inKidT){
    KIDT_AT_df[i,"inCurrentAdmission"] <- 1
  }else{
    KIDT_AT_df[i,"inCurrentAdmission"] <- 0
    
  }
}
table(KIDT_AT_df$inCurrentAdmission)

#2.KidT during
KIDT_During_df <- KIDT_During_df[which(KIDT_During_df$STUDY_PATIENT_ID %in% analysis_ID),]
KIDT_During_df$inCurrentAdmission <- NA
for (i in 1:nrow(KIDT_During_df)){
  curr_id <- KIDT_During_df[i,"STUDY_PATIENT_ID"]
  
  curr_hosp_start_inKidT <- ymd(strsplit(KIDT_During_df[i,"HOSP_ADMSN_TIME"],split = " ")[[1]][1]) #only get ymd 
  curr_hosp_end_inKidT   <- ymd(strsplit(KIDT_During_df[i,"HOSP_DISCH_TIME"],split = " ")[[1]][1])
  
  
  curr_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID == curr_id),]
  
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd 
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  
  if (curr_hosp_start_inKidT == curr_hosp_start & curr_hosp_end==curr_hosp_end_inKidT){
    KIDT_During_df[i,"inCurrentAdmission"] <- 1
  }else{
    KIDT_During_df[i,"inCurrentAdmission"] <- 0
    
  }
}
table(KIDT_During_df$inCurrentAdmission)



##########################################################################################
#3. Get kidney transplant before/AT and during (20)
##########################################################################################
KidT_Indicator_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 2))
colnames(KidT_Indicator_df) <- c("STUDY_PATIENT_ID","KidneyTrans_BEFOREorDURING")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  KidT_Indicator_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Source1
  curr_kt_df <- KIDT_AT_df[which(KIDT_AT_df[,"STUDY_PATIENT_ID"] == curr_id),]
  if (nrow(curr_kt_df) != 0){ #if it is in source 1, it is a yes
    curr_flag1 <- 1
  }else {
    curr_flag1 <- 0
  }
  
  #Source 2
  curr_kt_df2 <- KIDT_During_df[which(KIDT_During_df[,"STUDY_PATIENT_ID"] == curr_id),]

  if (nrow(curr_kt_df2) != 0 ){ #if it is in source 2, it is a yes
    curr_flag2 <- 1
  }else{
    curr_flag2 <- 0
  }
  
  #if any one source has a flag 1
  if (curr_flag1 == 1 | curr_flag2 == 1){
    KidT_Indicator_df[i,"KidneyTrans_BEFOREorDURING"] <- 1
  }else{
    KidT_Indicator_df[i,"KidneyTrans_BEFOREorDURING"] <- 0
  }
  
}

table(KidT_Indicator_df$KidneyTrans_BEFOREorDURING)
write.csv(KidT_Indicator_df,paste0(outdir,"KidneyTransplant.csv"),row.names=FALSE)
