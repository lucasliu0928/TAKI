library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#2. Kidney Transpant df
KIDT_df <- read.csv(paste0(raw_dir,"SURGERY_INDX.csv"),stringsAsFactors = F)
KIDT_df <- KIDT_df[which(grepl("kid",KIDT_df[,"SURGERY_DESCRIPTION"],ignore.case = T) == T),]
KIDT_df <- KIDT_df[which(grepl("trans",KIDT_df[,"SURGERY_DESCRIPTION"],ignore.case = T) == T),]
KIDT_df <- KIDT_df[!duplicated(KIDT_df$STUDY_PATIENT_ID),] #some pts get kt and then rejection two labels used for refering kidney transplant

#3. DIAGNOSIS df
raw_DIAGNOSIS_df <- read.csv(paste0(raw_dir,"DIAGNOSIS.csv"),stringsAsFactors = F)
search_codes<- c("Z94.0","V42.0" , "996.81", "Z48.22", "T86.12", "T86.11", "T86.10")
KIDT_df2 <- raw_DIAGNOSIS_df[which(raw_DIAGNOSIS_df[,"ICD_CODE"] %in% search_codes),]
unique(raw_DIAGNOSIS_df$DIAGNOSIS_TYPE)
table(KIDT_df2$ICD_CODE)

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3. Get kidney transplant before and during
##########################################################################################
KidT_Indicator_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 3))
colnames(KidT_Indicator_df) <- c("STUDY_PATIENT_ID","KidneyTrans_BEFORE","KidneyTrans_DURING")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0) {print(i)}
  curr_id <- analysis_ID[i]
  KidT_Indicator_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd(strsplit(curr_time_df[,"Updated_HOSP_ADMIT_DATE"],split = " ")[[1]][1]) #only get ymd cuz esrd dates without hms
  curr_hosp_end   <- ymd(strsplit(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"],split = " ")[[1]][1])
  
 
  #kid trans info
  curr_kt_df <- KIDT_df[which(KIDT_df[,"STUDY_PATIENT_ID"] == curr_id),]
  if (nrow(curr_kt_df) != 0){
    curr_kt_date <- mdy(curr_kt_df[,"SURGERY_PERFORMED_DATE"])
    if (curr_kt_date >= curr_hosp_start & curr_kt_date <= curr_hosp_end){ 
      KidT_Indicator_df[i,"KidneyTrans_DURING"] <- 1
    }else if (curr_kt_date < curr_hosp_start) {
      KidT_Indicator_df[i,"KidneyTrans_BEFORE"] <- 1
    }
  }else {
    KidT_Indicator_df[i,"KidneyTrans_BEFORE"] <- NA
    KidT_Indicator_df[i,"KidneyTrans_DURING"] <- NA
  }

}
write.csv(KidT_Indicator_df,paste0(outdir,"KidneyTranspant.csv"),row.names=FALSE)
