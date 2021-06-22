library(lubridate)
source("TAKI_Ultility.R")



#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Load inclusion ID
Inclusion_df <-read.csv(paste0(outdir,"Inclusion_IDs.csv"),stringsAsFactors = F)

#2. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(Inclusion_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3. Death or alive in the first 3 days of ICU D0,D1,D3, even if patient died outside ICU (e.g, D3 not in ICU)
##########################################################################################
Death_ICU_D0toD3_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Death_ICU_D0toD3_df) <- c("STUDY_PATIENT_ID","Death_ICU_D0toD3")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Death_ICU_D0toD3_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  curr_decease_date <- mdy(curr_time_df[,"DECEASED_DATE"])
  
  #Get ICU day 3 end time no matter it is in ICU or not 
  curr_d3_end_time <- ymd_hms(curr_time_df[,"D3_End"])

  if (is.na(curr_decease_date) == T){ #no death date
      Death_ICU_D0toD3_df[i,"Death_ICU_D0toD3"] <- 0
  }else {
      if (curr_decease_date >= curr_icu_start  & curr_decease_date <= curr_d3_end_time ){
        Death_ICU_D0toD3_df[i,"Death_ICU_D0toD3"] <- 1
      }else{
        Death_ICU_D0toD3_df[i,"Death_ICU_D0toD3"] <- 0
      }
  }

}

table(Death_ICU_D0toD3_df$Death_ICU_D0toD3) #34973  1044 

##########################################################################################
#3. Death or alive in Hospital
#hosp_start  <=  decease_date <= hosp_end
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

table(Death_inHOSP$Death_inHOSP) #33685  2332

##########################################################################################
#4. Death or alive within 120 days post HOSP discharge + Death in HOSP
# HOSP_Start <= death date <= (HOSP DC + 120 days)
##########################################################################################
Death_HOSP120_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 2))
colnames(Death_HOSP120_df) <- c("STUDY_PATIENT_ID","Death_HOSPStartTo120")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  Death_HOSP120_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_hosp_start <- ymd_hms(curr_time_df[,"Updated_HOSP_ADMIT_DATE"])
  curr_hosp_end   <- ymd_hms(curr_time_df[,"Updated_HOSP_DISCHARGE_DATE"])
  curr_decease_date <- mdy(curr_time_df[,"DECEASED_DATE"])
  
  #Get HOSP DC + 120 days
  HOSP_120_time <- curr_hosp_end + days(120)
  
  if (is.na(curr_decease_date) == T){ #no death date
    Death_HOSP120_df[i,"Death_HOSPStartTo120"] <- 0
  }else {
    if (curr_decease_date >= curr_hosp_start  & curr_decease_date <= HOSP_120_time){
      Death_HOSP120_df[i,"Death_HOSPStartTo120"] <- 1
    }else{
      Death_HOSP120_df[i,"Death_HOSPStartTo120"] <- 0
    }
  }
  
}

table(Death_HOSP120_df$Death_HOSPStartTo120) #

##########################################################################################
###Combine above 3 dataframe
##########################################################################################
#make sure IDs are equal
identical(Death_ICU_D0toD3_df$STUDY_PATIENT_ID,Death_inHOSP$STUDY_PATIENT_ID)
identical(Death_ICU_D0toD3_df$STUDY_PATIENT_ID,Death_HOSP120_df$STUDY_PATIENT_ID)

comb_death_df <- cbind(Death_ICU_D0toD3_df,Death_inHOSP,Death_HOSP120_df)
comb_death_df <- comb_death_df[,-c(3,5)]
write.csv(comb_death_df,paste0(outdir,"All_Mortality.csv"),row.names=FALSE)

#Check
table(Death_ICU_D0toD3_df[,"Death_ICU_D0toD3"], Death_HOSP120_df[,"Death_HOSPStartTo120"]) #should be 1044 overlap
table(Death_ICU_D0toD3_df[,"Death_ICU_D0toD3"], Death_inHOSP[,"Death_inHOSP"]) #should be 1044 overlap
table(Death_HOSP120_df[,"Death_HOSPStartTo120"], Death_inHOSP[,"Death_inHOSP"]) #should be 2332 overlap