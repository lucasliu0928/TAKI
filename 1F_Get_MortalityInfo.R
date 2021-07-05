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


#'@TODO
##########################################################################################
#2. Check if mistach between DECEASED_DATE and disposition (Expired|Hospice) 
#   If expired or hospice, should have a  deceased date <= HOSP_DISCHARGE_DATE + 24h
##########################################################################################
expired_orhospice_indexes <- which(grepl("Expired|Hospice",All_time_df[,"DISCHARGE_DISPOSITION"],ignore.case = T)==T)
expired_orhospice_df <- All_time_df[expired_orhospice_indexes,c("STUDY_PATIENT_ID","DISCHARGE_DISPOSITION","Updated_HOSP_DISCHARGE_DATE","DECEASED_DATE")]

#Get pt whoes deceased date > HOSP_DISCHARGE_DATE + 24h
check_idxes <- which(mdy(expired_orhospice_df[,"DECEASED_DATE"]) >  ymd_hms(expired_orhospice_df[,"Updated_HOSP_DISCHARGE_DATE"]) + hours(24))
check_df <- expired_orhospice_df[check_idxes,]
colnames(check_df)[3] <- "HOSP_DISCHARGE_DATE"

write.csv(check_df,"/Users/lucasliu/Desktop/DISCHARGE_DISPOSITION_And_DECEASED_DATE_Check.csv",row.names = F)
##########################################################################################
#3. Analysis Id for pts has corrected HOSP ADMISSION time
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

#'@TODO:
##########################################################################################
#3. Death or alive in Hospital
#1.if has disease date, hosp_start  <=  decease_date <= hosp_end + 24 hours
#2.if NO disease date,  if disposition contains expried/hopice,treat it as died in hosp
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
  curr_disposition <- curr_time_df[,"DISCHARGE_DISPOSITION"]
  
  if (is.na(curr_decease_date) == T){ #no death date
    #check if disposition contains hospice or expired
    if (grepl("Expired|Hospice",curr_disposition,ignore.case = T)==T){
      Death_inHOSP[i,"Death_inHOSP"] <- 1
    }else{
      Death_inHOSP[i,"Death_inHOSP"] <- 0
    }
    
  }else {#if has death date
    if (curr_decease_date >= curr_hosp_start  & curr_decease_date <= curr_hosp_end + hours(24)){
      Death_inHOSP[i,"Death_inHOSP"] <- 1
    }else{
      Death_inHOSP[i,"Death_inHOSP"] <- 0
    }
  }
  
}

table(Death_inHOSP$Death_inHOSP) #33685  2332 #after udpate 31073 vs 4944

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

table(Death_HOSP120_df$Death_HOSPStartTo120) #32330  3687 

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
#1. died in ICU d0-d3, but not in hosp
#the D3 date can outise the hosp, these patient hosp dc date < D3  dates
which(comb_death_df[,"Death_ICU_D0toD3"] == 1 & comb_death_df[,"Death_inHOSP"]==0) #43
#2. died in ICU d0-d3, but not within hosp + 120 days 
which(comb_death_df[,"Death_ICU_D0toD3"] == 1 & comb_death_df[,"Death_HOSPStartTo120"]==0) #0
#3. died in hosp, but not within hosp + 120 days 
which(comb_death_df[,"Death_inHOSP"] == 1 & comb_death_df[,"Death_HOSPStartTo120"]==0) #0
