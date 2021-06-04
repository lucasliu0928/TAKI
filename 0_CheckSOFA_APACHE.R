#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features/uky/"

#1. Load feature data
UK_feature_df <- read.csv(paste0(data_dir,"sofa_apache.csv"),stringsAsFactors = F)

#@.Load ID
library(openxlsx)
ID_df <- read.xlsx("/Volumes/LJL_ExtPro/Data/AKI_Data/Victors_data/Matching_big_dataset.xlsx")

