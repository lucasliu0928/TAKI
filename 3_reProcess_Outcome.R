source("TAKI_Ultility.R")

#User input
#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features/"
UK_data_dir <- paste0(data_dir,"uky/")
UTSW_data_dir <- paste0(data_dir,"utsw/")

#out dir
out_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/Updated_features/"
UK_outdir<-  paste0(out_dir,"uky/")
UTSW_outdir<-  paste0(out_dir,"utsw/")


####################################################################################################
##### UK outcome
####################################################################################################
#Load Outcome data
outcome_df <- read.csv(paste0(UK_data_dir,"outcomes.csv"),stringsAsFactors = F)
colnames(outcome_df)[which(colnames(outcome_df) == "id")] <- c("STUDY_PATIENT_ID")


#ADD Make outcome
outcome_df$MAKE <- NA
MAKE1_idxes <- which(outcome_df[,"died_in_window"] == 1 |
                       outcome_df[,"gfr_drop_50"] == 1 |
                       outcome_df[,"rrt_48hr"] == 1 |
                       outcome_df[,"esrd_manual_revision"] == 1|
                       outcome_df[,"esrd_scm"] == 1|
                       outcome_df[,"esrd_usrds"] == 1)
outcome_df$MAKE[MAKE1_idxes] <- 1
outcome_df$MAKE[-MAKE1_idxes] <- 0


#Load UK exclusion
UK_exclusion_df <- read.csv("/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/exclusion_criteria._taylor.csv",stringsAsFactors = F)
UK_ID_left_df <- UK_exclusion_df[which(rowSums(UK_exclusion_df[,2:ncol(UK_exclusion_df)])==0),] #keep pt without "1" in any of the column

#Update outcome df with ID left
updated_outcome_df <- outcome_df[which(outcome_df[,"STUDY_PATIENT_ID"] %in% UK_ID_left_df[,"STUDY_PATIENT_ID"]),c("STUDY_PATIENT_ID","died_inp","MAKE")]
table(updated_outcome_df$MAKE)
table(updated_outcome_df$died_inp)
write.csv(updated_outcome_df,paste0(UK_outdir,"updated_outcome.csv"),row.names = F)


####################################################################################################
##### UTSW outcome
####################################################################################################
#Load Outcome data
UTSW_outcome_df <- read.csv(paste0(UTSW_data_dir,"outcomes.csv"),stringsAsFactors = F)
colnames(UTSW_outcome_df)[which(colnames(UTSW_outcome_df) == "Died")] <- c("died_inp")
write.csv(UTSW_outcome_df,paste0(UTSW_outdir,"updated_outcome.csv"),row.names = F)
