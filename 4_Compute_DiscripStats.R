source("TAKI_Ultility.R")

#Data dir
UK_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"
UTSW_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/Model_Feature_Outcome/"

#out dir
out_dir <- "//Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0708/"

####################################################################################
#### 1. Load data
####################################################################################
feature_file <- "All_Feature_NOTimputed.csv"
outcome_file <- "All_outcome.csv"
outcome_colname_list <- c("Death_inHOSP","MAKE_HOSP120_Drop50")

#For UK
UK_data <- Combine_featureAndoutcomes_func(UK_data_dir,feature_file,outcome_file,outcome_colname_list)
#Update Anemia before computation using imputed labs

#For UTSW
UTSW_data <- Combine_featureAndoutcomes_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname_list)


####################################################################################
###Missing table
####################################################################################


#For UK



UK_MissingTable <- get_missing_rate_table(UK_data,colnames(UK_data))

#UTSW
UTSW_MissingTable <- get_missing_rate_table(UTSW_data,colnames(UTSW_data))
