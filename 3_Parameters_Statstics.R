
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features_forsubmission/"

UTSW_df <- read.csv(paste0(data_dir,"utsw/base_model_ALL_FEATURES.csv"),stringsAsFactors = F)

UK_df$Albumin

#############################################################
# UK data
#############################################################
#Load UK base data
UK_df <- read.csv(paste0(data_dir,"uky/base_model_ALL_FEATURES.csv"),stringsAsFactors = F)

#Load UK exclusion
UK_exclusion_df <- read.csv(paste0(data_dir,"uky/exclusion_criteria._taylor.csv"),stringsAsFactors = F)
UK_ID_left_df <- UK_exclusion_df[which(rowSums(UK_exclusion_df[,2:ncol(UK_exclusion_df)])==0),] #keep pt without "1" in any of the column

#filter exclusion
updated_UK_df <- UK_df[which(UK_df[,"STUDY_PATIENT_ID"] %in% UK_ID_left_df[,"STUDY_PATIENT_ID"]),]

compute_missing_rate(updated_UK_df,"Albumin")
