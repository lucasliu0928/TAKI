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
weight_df <- read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_BMI_NOTimputed.csv"),stringsAsFactors = F)

#4. IO
raw_IO_df <- read.csv(paste0(raw_dir,"IO_TOTALS.csv"),stringsAsFactors = F)



##########################################################################################
#Load UK raw IO_TOTALS
#Features to extract : Compute FluidOverload ((TOTAL_in - TOTAL_out)*100/(weight))
#                      Weight is imputed

#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. Compute FluidOverload
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#1. Get raw available values
IO_TOTALS_D0_INVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D0_INVALUE","IO_TOTALS_D0_INVALUE")
IO_TOTALS_D0_OUTVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D0_OUTVALUE","IO_TOTALS_D0_OUTVALUE")
IO_TOTALS_D1_INVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D1_INVALUE","IO_TOTALS_D1_INVALUE")
IO_TOTALS_D1_OUTVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D1_OUTVALUE","IO_TOTALS_D1_OUTVALUE")
IO_TOTALS_D2_INVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D2_INVALUE","IO_TOTALS_D2_INVALUE")
IO_TOTALS_D2_OUTVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D2_OUTVALUE","IO_TOTALS_D2_OUTVALUE")
IO_TOTALS_D3_INVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D3_INVALUE","IO_TOTALS_D3_INVALUE")
IO_TOTALS_D3_OUTVALUE_df <- get_raw_var_values_1option_func(raw_IO_df,analysis_ID,"IO_TOTALS_D3_OUTVALUE","IO_TOTALS_D3_OUTVALUE")

All_IO_df <- cbind(IO_TOTALS_D0_INVALUE_df,IO_TOTALS_D0_OUTVALUE_df,
                   IO_TOTALS_D1_INVALUE_df,IO_TOTALS_D1_OUTVALUE_df,
                   IO_TOTALS_D2_INVALUE_df,IO_TOTALS_D2_OUTVALUE_df,
                   IO_TOTALS_D3_INVALUE_df,IO_TOTALS_D3_OUTVALUE_df)

All_IO_df <- All_IO_df[,-which(colnames(All_IO_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one

#convert unit to liters
for (j in 2:length(All_IO_df)){
  All_IO_df[,j] <-  All_IO_df[,j]/1000
}

#2.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_IO_df <- remove_featureValue(All_IO_df,All_time_df)
table(updated_IO_df$Excluded_Feature)

#3.Compute Total in and Total out using the values in ICU which was updated in step 2. in updated_IO_df
in_col_names <- c("IO_TOTALS_D0_INVALUE","IO_TOTALS_D1_INVALUE","IO_TOTALS_D2_INVALUE","IO_TOTALS_D3_INVALUE")
out_col_names <- c("IO_TOTALS_D0_OUTVALUE", "IO_TOTALS_D1_OUTVALUE", "IO_TOTALS_D2_OUTVALUE","IO_TOTALS_D3_OUTVALUE")

updated_IO_df$TOTAL_IN <- NA
updated_IO_df$TOTAL_OUT <- NA
updated_IO_df$Weight_KG <- NA
updated_IO_df$FluidOverload_inPercentage <- NA

for (i in 1:nrow(updated_IO_df)){
  curr_id <- updated_IO_df[i,"STUDY_PATIENT_ID"]
  
  curr_weight <- weight_df[which(weight_df[,"STUDY_PATIENT_ID"] == curr_id),"INITIAL_WEIGHT_KG"]
  
  all_ins <- updated_IO_df[i,in_col_names]
  all_outs <- updated_IO_df[i,out_col_names]
  
  if (all(is.na(all_ins)==T) == T & all(is.na(all_outs)==T) == T){ #if no in or out values is available
    curr_TOTAL_in <- NA
    curr_TOTAL_out <- NA
    curr_val <- NA
    
  }else{
    curr_TOTAL_in <- sum(all_ins,na.rm = T)
    curr_TOTAL_out <- sum(all_outs,na.rm = T)
    curr_val <- (curr_TOTAL_in - curr_TOTAL_out)*100/(curr_weight)
  }
  updated_IO_df[i,"TOTAL_IN"] <- curr_TOTAL_in
  updated_IO_df[i,"TOTAL_OUT"] <- curr_TOTAL_out
  updated_IO_df[i,"Weight_KG"] <- curr_weight
  updated_IO_df[i,"FluidOverload_inPercentage"] <- curr_val
}

write.csv(updated_IO_df,paste0(outdir,"All_FluidOverLoad_NOTImputed.csv"))

#4. Compute missing
feature_columns <-  c("FluidOverload_inPercentage")
missing_table <- get_missing_rate_table(updated_IO_df,feature_columns)
missing_table


#5.imputation median for feature column
Final_IO_df <- median_imputation_func(updated_IO_df,feature_columns)
missing_table2 <- get_missing_rate_table(Final_IO_df,feature_columns)
missing_table2

#remove features not needed for model
exlucde_indxes <- which(colnames(Final_IO_df) %in% c("Excluded_Feature"))
Final_IO_df <- Final_IO_df[,-exlucde_indxes]
write.csv(Final_IO_df,paste0(outdir,"All_FluidOverLoad_Imputed.csv"))

###########################################################################################
#Random select 20 of not imputated for checking
###########################################################################################
#'@NOTE: #FluidOverload could be missing if no weight or IO

set.seed(123)
imputed_idxes <- which(is.na(Final_IO_df$TOTAL_IN)==T)
not_imputed_df <- Final_IO_df[-imputed_idxes,]
sample_idxes <- sample(nrow(not_imputed_df),20)
sample_df <- not_imputed_df[sample_idxes,]
write.csv(sample_df,paste0("/Users/lucasliu/Desktop/","FluidOverload_Sample.csv"),row.names = F)
