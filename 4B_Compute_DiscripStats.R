source("TAKI_Ultility.R")
library(lubridate)

#Data dir
UK_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"
UTSW_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/Discrip_Stats/"

####################################################################################
#### 1. Load data
####################################################################################
feature_file <- "Model_Feature_Outcome/All_Feature_NOTimputed.csv"
outcome_file <- "Model_Feature_Outcome/All_outcome.csv"
outcome_colname_list <- c("Death_inHOSP","MAKE_HOSP120_Drop50")

#For UK
UK_data <- Combine_featureAndoutcomes_func(UK_data_dir,feature_file,outcome_file,outcome_colname_list)

#For UTSW
UTSW_data <- Combine_featureAndoutcomes_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname_list)


####################################################################################
###3.Add some variables
####################################################################################
#For UK
UK_data <- add_listofvar_func(UK_data,UK_data_dir,"UK")

#For UTSW
UTSW_data <- add_listofvar_func(UTSW_data,UTSW_data_dir,"UTSW")

####################################################################################
##4. Recode last and max KDIGO 3 and 4 
####################################################################################
UK_data <-  recode_KDIGO_func(UK_data,"LAST_KDIGO_ICU_D0toD3")
UK_data <-  recode_KDIGO_func(UK_data,"MAX_KDIGO_ICU_D0toD3")


UTSW_data <-  recode_KDIGO_func(UTSW_data,"LAST_KDIGO_ICU_D0toD3")
UTSW_data <-  recode_KDIGO_func(UTSW_data,"MAX_KDIGO_ICU_D0toD3")

####################################################################################
###2. Missing table
####################################################################################
#For UK
UK_MissingTable <- get_missing_rate_table(UK_data,colnames(UK_data))
colnames(UK_MissingTable)[2] <- paste0("UK_",colnames(UK_MissingTable)[2])
write.csv(UK_MissingTable,paste0(out_dir,"Feature_MissingTable_UK.csv"),row.names = F)

#For UTSW
UTSW_MissingTable <- get_missing_rate_table(UTSW_data,colnames(UTSW_data))
colnames(UTSW_MissingTable)[2] <- paste0("UTSW_",colnames(UTSW_MissingTable)[2])
write.csv(UTSW_MissingTable,paste0(out_dir,"Feature_MissingTable_UTSW.csv"),row.names = F)



####################################################################################
#### supp table 2 Mortality
####################################################################################
selected_features_Mortality <- c("UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                                 "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                                 "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3","Hours_inICUD0toD3", "Temperature_D1_LOW", "Temperature_D1_HIGH")
#Outdir for mortality
#For UK
survived_df_UK <- UK_data[which(UK_data$Death_inHOSP==0),]
died_df_UK     <- UK_data[which(UK_data$Death_inHOSP==1),]
UK_tb2_sur <- compute_stats_func(survived_df_UK,"UK_survived",selected_features_Mortality)
UK_tb2_die <- compute_stats_func(died_df_UK,"UK_died",selected_features_Mortality)
UK_comb_tb2 <- cbind(UK_tb2_sur,UK_tb2_die)

#For UTSW
survived_df_UTSW <- UTSW_data[which(UTSW_data$Death_inHOSP==0),]
died_df_UTSW     <- UTSW_data[which(UTSW_data$Death_inHOSP==1),]
UTSW_tb2_sur <- compute_stats_func(survived_df_UTSW,"UTSW_survived",selected_features_Mortality)
UTSW_tb2_die <- compute_stats_func(died_df_UTSW,"UTSW_died",selected_features_Mortality)
UTSW_comb_tb2 <- cbind(UTSW_tb2_sur,UTSW_tb2_die)

final_supp_tb2 <- cbind(UK_comb_tb2,UTSW_comb_tb2)
final_supp_tb2 <- final_supp_tb2[,-c(3,5,7)]
write.csv(final_supp_tb2,paste0(out_dir,"Supp_table2.csv"),row.names = F)

####################################################################################
#### supp table 3 MAKE
####################################################################################
selected_features_MAKE <- c("LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                            "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                            "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                            "Admit_sCr","Sodium_D1_LOW")
  
  
#For UK
MAKE0_df_UK     <- UK_data[which(UK_data$MAKE_HOSP120_Drop50==0),]
MAKE1_df_UK     <- UK_data[which(UK_data$MAKE_HOSP120_Drop50==1),]
UK_tb3_0 <- compute_stats_func(MAKE0_df_UK,"UK_MAKE0",selected_features_MAKE)
UK_tb3_1 <- compute_stats_func(MAKE1_df_UK,"UK_MAKE1",selected_features_MAKE)
UK_comb_tb3 <- cbind(UK_tb3_0,UK_tb3_1)

#For UTSW
MAKE0_df_UTSW     <- UTSW_data[which(UTSW_data$MAKE_HOSP120_Drop50==0),]
MAKE1_df_UTSW     <- UTSW_data[which(UTSW_data$MAKE_HOSP120_Drop50==1),]
UTSW_tb3_0 <- compute_stats_func(MAKE0_df_UTSW,"UTSW_MAKE0",selected_features_MAKE)
UTSW_tb3_1 <- compute_stats_func(MAKE1_df_UTSW,"UTSW_MAKE1",selected_features_MAKE)
UTSW_comb_tb3 <- cbind(UTSW_tb3_0,UTSW_tb3_1)

final_supp_tb3 <- cbind(UK_comb_tb3,UTSW_comb_tb3)
final_supp_tb3 <- final_supp_tb3[,-c(3,5,7)]
write.csv(final_supp_tb3,paste0(out_dir,"Supp_table3.csv"),row.names = F)

####################################################################################
#### supp table4. Discriptive stats for All cohaort
####################################################################################
var_list <- c("AGE","GENDER","RACE","BMI","CHARLSON_SCORE","TOTAL_ELIX","Diabetes","Hypertension","Baseline_eGFR","CKD",
              "SOFA_TOTAL","APACHE_TOTAL","Days_inHOSP","Hours_inICUD0toD3","ECMO_ICUD0toD3","IABP_ICUD0toD3","VAD_ICUD0toD3","MV_ICUD0toD3","Days_MV_ICUD0toD3",
              "Sepsis_Before_or_At_Admission","UrineOutput_D0toD3","UrineFlow_D0toD3","FluidOverload_inPercentage",
              "Bicarbonate_D1_AVGof(LOWHIGH)","BUN_D0toD3_HIGH","Hematocrit_D1_AVGof(LOWHIGH)","Hemoglobin_D1_AVGof(LOWHIGH)",
              "Baseline_sCr","Admit_sCr","Peak_SCr_inICU_D0_D3","LastSCr_inICU_D0_D3","MAX_KDIGO_ICU_D0toD3","LAST_KDIGO_ICU_D0toD3",
              "onRRT_ICUD0toD3","RRTinfo_ICUD0toD3",
              "CRRT_Days_inICUD0toD3","HD_Days_inICUD0toD3")


#For UK
UK_tb4 <- compute_stats_func(UK_data,"UK",var_list)

#For UTSW
UTSW_tb4 <- compute_stats_func(UTSW_data,"UTSW",var_list)

comb_supptb4 <- cbind(UK_tb4,UTSW_tb4)
write.csv(comb_supptb4,paste0(out_dir,"Supp_table4.csv"),row.names = F)

