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

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3. Death or alive in the first 4 days of ICU D0,D1,D3,D4
##########################################################################################
Death_ICU_D0toD4_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Death_ICU_D0toD4_df) <- c("STUDY_PATIENT_ID","Death_ICU_D0toD4")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Death_ICU_D0toD4_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  curr_decease_date <- mdy(curr_time_df[,"DECEASED_DATE"])
  
  #Get ICU possible day 4 end time no matter if the patient has ICU > 4 days or not
  d0_endtime <- get_sameDay_lasttime_func(curr_icu_start)
  d4_endtime <- d0_endtime + days(4)
  
  if (is.na(curr_decease_date) == T){ #no death date
      Death_ICU_D0toD4_df[i,"Death_ICU_D0toD4"] <- 0
  }else {
      if (curr_decease_date >= curr_icu_start  & curr_decease_date <= d4_endtime ){
        Death_ICU_D0toD4_df[i,"Death_ICU_D0toD4"] <- 1
      }else{
        Death_ICU_D0toD4_df[i,"Death_ICU_D0toD4"] <- 0
      }
  }

}

table(Death_ICU_D0toD4_df$Death_ICU_D0toD4)
##########################################################################################
#3. Death or alive in Hospital
##########################################################################################
Death_inHOSP <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Death_inHOSP) <- c("STUDY_PATIENT_ID","Death_inHOSP")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Death_inHOSP[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd_hms(curr_time_df[,"Updated_HOSP_ADMIT_DATE"])
  curr_hosp_end   <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  curr_decease_date <- mdy(curr_time_df[,"DECEASED_DATE"])
  
  if (is.na(curr_decease_date) == T){ #no death date
    Death_inHOSP[i,"Death_inHOSP"] <- 0
  }else {
    if (curr_decease_date >= curr_hosp_start  & curr_decease_date <= curr_hosp_end ){
      Death_inHOSP[i,"Death_inHOSP"] <- 1
    }else{
      Death_inHOSP[i,"Death_inHOSP"] <- 0
    }
  }
  
}

table(Death_inHOSP$Death_inHOSP)

##########################################################################################
#4. Death or alive within 120 days post ICU discharge + Death after ICU admit 
##########################################################################################
Death_ICU120_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Death_ICU120_df) <- c("STUDY_PATIENT_ID","Death_ICUStartTo120")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Death_ICU120_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  curr_decease_date <- mdy(curr_time_df[,"DECEASED_DATE"])
  
  #Get ICU + 120 days
  ICU_120_time <- curr_icu_end + days(120)
  
  if (is.na(curr_decease_date) == T){ #no death date
    Death_ICU120_df[i,"Death_ICUStartTo120"] <- 0
  }else {
    if (curr_decease_date > curr_icu_start  & curr_decease_date <= ICU_120_time){
      Death_ICU120_df[i,"Death_ICUStartTo120"] <- 1
    }else{
      Death_ICU120_df[i,"Death_ICUStartTo120"] <- 0
    }
  }
  
}


##########################################################################################
###Combine above 3 dataframe
##########################################################################################
#make sure IDs are equal
identical(Death_ICU_D0toD4_df$STUDY_PATIENT_ID,Death_inHOSP$STUDY_PATIENT_ID)
identical(Death_ICU_D0toD4_df$STUDY_PATIENT_ID,Death_ICU120_df$STUDY_PATIENT_ID)

comb_death_df <- cbind(Death_ICU_D0toD4_df,Death_inHOSP,Death_ICU120_df)
comb_death_df <- comb_death_df[,-c(3,5)]
write.csv(comb_death_df,paste0(outdir,"All_Mortality.csv"),row.names=FALSE)
