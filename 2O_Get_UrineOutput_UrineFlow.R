library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
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

#3. Weight
weight_df <- read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_NOTimputed.csv"),stringsAsFactors = F)

#4. URINE_OUTPUT
raw_URINE_OUTPUT_df <- read.csv(paste0(raw_dir,"URINE_OUTPUT.csv"),stringsAsFactors = F)

##########################################################################################
#Features to extract : 1. Compute Urine Output: Total urine output ICU D0-D3 / measurement time
#                      2. Compute Urine Flow:   Total urine output ICU D0-D3 / weight / measurement time

#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. remove outliers
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#1. Get raw available values
U0_INDWELLING_URETHRAL_CATHTER_D0_VALUE_df <- get_raw_var_values_1option_func(raw_URINE_OUTPUT_df,analysis_ID,"U0_INDWELLING_URETHRAL_CATHTER_D0_VALUE","U0_INDWELLING_URETHRAL_CATHTER_D0_VALUE")
U0_INDWELLING_URETHRAL_CATHTER_D1_VALUE_df <- get_raw_var_values_1option_func(raw_URINE_OUTPUT_df,analysis_ID,"U0_INDWELLING_URETHRAL_CATHTER_D1_VALUE","U0_INDWELLING_URETHRAL_CATHTER_D1_VALUE")
U0_INDWELLING_URETHRAL_CATHTER_D2_VALUE_df <- get_raw_var_values_1option_func(raw_URINE_OUTPUT_df,analysis_ID,"U0_INDWELLING_URETHRAL_CATHTER_D2_VALUE","U0_INDWELLING_URETHRAL_CATHTER_D2_VALUE")
U0_INDWELLING_URETHRAL_CATHTER_D3_VALUE_df <- get_raw_var_values_1option_func(raw_URINE_OUTPUT_df,analysis_ID,"U0_INDWELLING_URETHRAL_CATHTER_D3_VALUE","U0_INDWELLING_URETHRAL_CATHTER_D3_VALUE")

All_UrineOutput_df <- cbind(U0_INDWELLING_URETHRAL_CATHTER_D0_VALUE_df,U0_INDWELLING_URETHRAL_CATHTER_D1_VALUE_df,
                            U0_INDWELLING_URETHRAL_CATHTER_D2_VALUE_df,U0_INDWELLING_URETHRAL_CATHTER_D3_VALUE_df)

All_UrineOutput_df <- All_UrineOutput_df[,-which(colnames(All_UrineOutput_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one

#Recode 0 as NAs
for (j in 2:ncol(All_UrineOutput_df)){
  idxes_0s <- which(All_UrineOutput_df[,j] == 0)
  if (length(idxes_0s) > 0){
    All_UrineOutput_df[idxes_0s,j] <- NA
  }
}

#2.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_UrineOutput_df <- remove_featureValue(All_UrineOutput_df,All_time_df)
table(updated_UrineOutput_df$Excluded_Feature)


#4 Compute Total using values after outlier exclustion
col_names <-    c("U0_INDWELLING_URETHRAL_CATHTER_D0_VALUE",
                  "U0_INDWELLING_URETHRAL_CATHTER_D1_VALUE",
                  "U0_INDWELLING_URETHRAL_CATHTER_D2_VALUE",
                  "U0_INDWELLING_URETHRAL_CATHTER_D3_VALUE")
updated_UrineOutput_df$TOTAL <- NA
updated_UrineOutput_df$Weight_KG <- NA
updated_UrineOutput_df$MeasurementTime_hr <- NA
updated_UrineOutput_df$UrineOutput <- NA
updated_UrineOutput_df$UrineFlow <- NA

for (i in 1:nrow(updated_UrineOutput_df)){
  curr_id <- updated_UrineOutput_df[i,"STUDY_PATIENT_ID"]
  
  curr_weight <- weight_df[which(weight_df[,"STUDY_PATIENT_ID"] == curr_id),"INITIAL_WEIGHT_KG"]
  curr_time_df <-  All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_total_hours_inICU <- curr_time_df[,"Actual_ICUHours_D0toD3"]
  
  all_values <- updated_UrineOutput_df[i,col_names]

  if (all(is.na(all_values)==T) == T){ #if no values is available
    curr_total <- NA
    curr_urine_output <- NA
    curr_urine_flow <- NA
  }else{
    curr_total <- sum(all_values,na.rm = T)
    curr_urine_output <- curr_total/curr_total_hours_inICU
    curr_urine_flow <- curr_total/curr_weight/curr_total_hours_inICU
  }
  
  updated_UrineOutput_df[i,"TOTAL"] <- curr_total
  updated_UrineOutput_df[i,"Weight_KG"] <- curr_weight
  updated_UrineOutput_df[i,"MeasurementTime_hr"] <- curr_total_hours_inICU
  updated_UrineOutput_df[i,"UrineOutput"] <- curr_urine_output
  updated_UrineOutput_df[i,"UrineFlow"] <- curr_urine_flow
}


#3.Remove outlier
#UrineOutput 
#'@NOTE: only excluded urine output for outliers, so the total value of outliers are not NA
updated_UrineOutput_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_UrineOutput_df,"UrineOutput","TOP")

#4. Compute missing
feature_columns <-  c("UrineOutput","UrineFlow")
missing_table <- get_missing_rate_table(updated_UrineOutput_df_OutlierExcluded,feature_columns)
missing_table

write.csv(updated_UrineOutput_df_OutlierExcluded,paste0(outdir,"All_UrineOutput_NOTimputed.csv"),row.names = F)

#5.imputation median for feature column 
Final_UrineOutput_df <- median_imputation_func(updated_UrineOutput_df_OutlierExcluded,feature_columns)
missing_table2 <- get_missing_rate_table(Final_UrineOutput_df,feature_columns)
missing_table2

#remove features not needed for model
exlucde_indxes <- which(colnames(Final_UrineOutput_df) %in% c("Excluded_Feature"))
Final_UrineOutput_df <- Final_UrineOutput_df[,-exlucde_indxes]
write.csv(Final_UrineOutput_df,paste0(outdir,"All_UrineOutput_Imputed.csv"),row.names = F)

###########################################################################################
#Random select 20 of not imputated for checking
###########################################################################################
set.seed(123)
#get NA indxes after exclude outliers
NA_idxes <- which(is.na(updated_UrineOutput_df_OutlierExcluded[,"UrineOutput"])==T)
not_imputed_df <- Final_UrineOutput_df[-NA_idxes,]
sample_idxes <- sample(nrow(not_imputed_df),20)
sample_df <- not_imputed_df[sample_idxes,]
write.csv(sample_df,paste0("/Users/lucasliu/Desktop/","UrineOutput_Sample.csv"),row.names = F)
