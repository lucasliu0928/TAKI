library(lubridate)
source("TAKI_Ultility.R")

get_feature_forPt <- function(pt_id, input_df,feature_name){
  feature_value <- input_df[which(input_df[,"STUDY_PATIENT_ID"] == pt_id),feature_name]
  return(feature_value)
}


#data dir
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#Mortality df
All_death_df <-read.csv(paste0(outdir,"All_Mortality.csv"),stringsAsFactors = F)

#ESRD 120
All_ESRD120_df <-read.csv(paste0(outdir,"ESRD_120.csv"),stringsAsFactors = F)

#eGFR drop
All_eGFR120_df <-read.csv(paste0(outdir,"EGFR_Drop_120_df.csv"),stringsAsFactors = F)
#Code NA as 0, these ones has no outpatient scr, we treat it as no eGFR drop
All_eGFR120_df[which(is.na(All_eGFR120_df[,"eGFR_Drop30"])==T),"eGFR_Drop30"] <- 0
All_eGFR120_df[which(is.na(All_eGFR120_df[,"eGFR_Drop50"])==T),"eGFR_Drop50"] <- 0

##########################################################################################
#Get outcome
#a.	Hospital mortality 
#b.	MAKE in the 120 days following HOSP discharge 
#i.	If the patient died within 120 days following HOSP discharge (or prior to discharge)
#ii.	If the patient survived but identified as recipient of any type of RRT within the last 48 hours of hospitalization
#iii.	If the patient survived but developed ESKD within 120 days followingHOSP discharge
#iv.	If the patient survived and was not identified in ii. and iii, and the eGFR (closet to 120 days post HOSP discharge) dropped >50% of the baseline eGFR 
#     (For the patient survived and was not identified as ESKD or as recipient of RRT within the last 48 hours of hospital stay)

##########################################################################################
outcome_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 9))
colnames(outcome_df) <- c("STUDY_PATIENT_ID",
                          "Death_inHOSP",
                          "Death_HOSPStartTo120",
                          "onRRT_Last48hBeforeDischarge",
                          "ESRD_120",
                          "eGFR_Drop30",
                          "eGFR_Drop50",
                          "MAKE_HOSP120_Drop30",
                          "MAKE_HOSP120_Drop50")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  outcome_df[i,"STUDY_PATIENT_ID"]    <- curr_id
  outcome_df[i,"Death_inHOSP"]         <-   get_feature_forPt(curr_id, All_death_df,"Death_inHOSP")
  
  outcome_df[i,"Death_HOSPStartTo120"]  <-   get_feature_forPt(curr_id, All_death_df,"Death_HOSPStartTo120")
  
  if (outcome_df[i,"Death_HOSPStartTo120"] ==0){ #for survivors:
      outcome_df[i,"onRRT_Last48hBeforeDischarge"]   <-   get_feature_forPt(curr_id, All_time_df,"onRRT_Last48hBeforeDischarge")
      outcome_df[i,"ESRD_120"]   <-   get_feature_forPt(curr_id, All_ESRD120_df,"ESRD_120")
      
      if (outcome_df[i,"onRRT_Last48hBeforeDischarge"] ==0 & outcome_df[i,"ESRD_120"] ==0){ #if not died, and not rrt 48h, and not ESRD
        outcome_df[i,"eGFR_Drop30"]    <-   get_feature_forPt(curr_id, All_eGFR120_df,"eGFR_Drop30")
        outcome_df[i,"eGFR_Drop50"]    <-   get_feature_forPt(curr_id, All_eGFR120_df,"eGFR_Drop50")
      }
  }
  
  
  
}

#Add make column
make_drop30_indexes <- which(outcome_df[,"Death_HOSPStartTo120"] == 1 | 
                             outcome_df[,"onRRT_Last48hBeforeDischarge"] == 1|  outcome_df[,"ESRD_120"] == 1 |
                             outcome_df[,"eGFR_Drop30"])
outcome_df[make_drop30_indexes,"MAKE_HOSP120_Drop30"] <- 1
outcome_df[-make_drop30_indexes,"MAKE_HOSP120_Drop30"] <- 0


make_drop50_indexes <- which(outcome_df[,"Death_HOSPStartTo120"] == 1 | 
                               outcome_df[,"onRRT_Last48hBeforeDischarge"] == 1|  outcome_df[,"ESRD_120"] == 1 |
                               outcome_df[,"eGFR_Drop50"])
outcome_df[make_drop50_indexes,"MAKE_HOSP120_Drop50"] <- 1
outcome_df[-make_drop50_indexes,"MAKE_HOSP120_Drop50"] <- 0

table(outcome_df$MAKE_HOSP120_Drop30) #5384 1970 
table(outcome_df$MAKE_HOSP120_Drop50) #5631 1723
table(outcome_df$Death_inHOSP) #6715  639
write.csv(outcome_df,paste0(outdir,"Model_Feature_Outcome/All_outcome.csv"),row.names=FALSE)


#############################################################################################################################
########################################## Check Taylors data ########################################## 
#############################################################################################################################
#'@Question: Other outomce sources?
#User input
#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features/"
UK_data_dir <- paste0(data_dir,"uky/")
UTSW_data_dir <- paste0(data_dir,"utsw/")

outcome_df2 <- read.csv(paste0(UK_data_dir,"outcomes.csv"),stringsAsFactors = F)
colnames(outcome_df2)[which(colnames(outcome_df2) == "id")] <- c("STUDY_PATIENT_ID")


#ADD Make outcome
outcome_df2$MAKE <- NA
MAKE1_idxes <- which(outcome_df2[,"died_in_window"] == 1 |
                       outcome_df2[,"gfr_drop_50"] == 1 |
                       outcome_df2[,"rrt_48hr"] == 1 |
                       outcome_df2[,"esrd_manual_revision"] == 1|
                       outcome_df2[,"esrd_scm"] == 1|
                       outcome_df2[,"esrd_usrds"] == 1)
outcome_df2$MAKE[MAKE1_idxes] <- 1
outcome_df2$MAKE[-MAKE1_idxes] <- 0


#Load UK exclusion
UK_exclusion_df <- read.csv("/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/exclusion_criteria._taylor.csv",stringsAsFactors = F)
UK_ID_left_df <- UK_exclusion_df[which(rowSums(UK_exclusion_df[,2:ncol(UK_exclusion_df)])==0),] #keep pt without "1" in any of the column

#Update outcome df with ID left
updated_outcome_df <- outcome_df2[which(outcome_df2[,"STUDY_PATIENT_ID"] %in% UK_ID_left_df[,"STUDY_PATIENT_ID"]),c("STUDY_PATIENT_ID","died_inp","MAKE")]
table(updated_outcome_df$MAKE)   #4257 2594
table(updated_outcome_df$died_inp) #5233 1618 

