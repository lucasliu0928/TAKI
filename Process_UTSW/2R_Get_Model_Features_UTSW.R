library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

get_feature_forPt2 <- function(pt_id, input_df,feature_name,ID_colname){
  feature_value <- input_df[which(input_df[,ID_colname] == pt_id),feature_name]
  return(feature_value)
}


#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/Model_Feature_Outcome/"


##########################################################################################
#A. Load data
##########################################################################################
#I.Lucas's feature
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(data_dir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354
#2. Corrected Time df 
All_time_df <-read.csv(paste0(data_dir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#3.sCr and KDIGO
All_Scr_df <-read.csv(paste0(data_dir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)
All_KDIGO_df <-read.csv(paste0(data_dir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)

#4.onRRTinD0toD3
All_onRRT_df <-read.csv(paste0(data_dir,"All_onRRT_ICUD0toD3.csv"),stringsAsFactors = F)

#5. Load sepsis
All_sepsis_df <-read.csv(paste0(data_dir,"All_sepsis_Before_Or_At_Admission.csv"),stringsAsFactors = F)


#II.Xilong's Feature (Load not imputed data)
Xilong_df1 <-read.csv(paste0(data_dir,"xilong_extracted/Final dataset for Lucas part 1 07272021.csv"),stringsAsFactors = F)
Xilong_df2 <-read.csv(paste0(data_dir,"xilong_extracted/Final dataset for Lucas part 2 07272021.csv"),stringsAsFactors = F)
Xilong_df3 <-read.csv(paste0(data_dir,"xilong_extracted/All variables for each patients 07212021.csv"),stringsAsFactors = F)

#Change Elixhaser name
Elix_indxes <- which(colnames(Xilong_df3) %in% paste0("Elixhauser_",seq(0,30),"_F"))
colnames(Xilong_df3)[Elix_indxes] <- paste0("ELX_GRP_",seq(1,31))

#Recode race to white =0 , black =1 , and other = 2 
table(Xilong_df1$RACE)   #3:BLACK, 4:WHITE, 9:Other 
black_idxes <- which(Xilong_df1$RACE == 3)
white_idxes <- which(Xilong_df1$RACE == 4)

Xilong_df1[white_idxes,"RACE"] <- 0
Xilong_df1[black_idxes,"RACE"] <- 1
Xilong_df1[-c(white_idxes,black_idxes),"RACE"] <- 2
Xilong_df1$RACE <- as.numeric(Xilong_df1$RACE)
table(Xilong_df1$RACE)

#Convert Height to Meters
Xilong_df2$AVG_HEIGHT_Meters <- (Xilong_df2$AVG_HEIGHT)*0.0254

##########################################################################################
#1. Get All Clinical feature df
##########################################################################################
Feature_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 72))
colnames(Feature_df) <- c("STUDY_PATIENT_ID",
                          "Admit_sCr",
                          "AGE",
                          "Anemia_D1",
                          "Baseline_sCr",
                          "Bicarbonate_D1_LOW","Bicarbonate_D1_HIGH",
                          "Bilirubin_D1_HIGH",
                          "BMI",
                          "BUN_D0toD3_HIGH",
                          "FI02_D1_LOW","FI02_D1_HIGH",
                          "FluidOverload_inPercentage",
                          "GENDER",
                          "HR_D1_LOW","HR_D1_HIGH",
                          "Hematocrit_D1_LOW","Hematocrit_D1_HIGH",
                          "Hemoglobin_D1_LOW","Hemoglobin_D1_HIGH",
                          "ECMO_ICUD0toD3","IABP_ICUD0toD3","MV_ICUD0toD3","VAD_ICUD0toD3",
                          "MAP_D1_LOW","MAP_D1_HIGH",
                          "Admit_KDIGO_ICU",
                          "Nephrotoxin_ICUD0toD3","Vasopressor_ICUD0toD3",
                          "PCO2_D1_LOW" , "PCO2_D1_HIGH",
                          "Peak_SCr_inICU_D0_D3",
                          "PH_D1_LOW","PH_D1_HIGH",
                          "Platelets_D1_LOW",
                          "PO2_D1_LOW", "PO2_D1_HIGH",
                          "Potassium_D1_LOW","Potassium_D1_HIGH",
                          "RACE",
                          "RESP_RATE_D1_LOW","RESP_RATE_D1_HIGH",
                          "Sepsis_Before_or_At_Admission",
                          "Sodium_D1_LOW","Sodium_D1_HIGH",
                          "Temperature_D1_LOW","Temperature_D1_HIGH",
                          "UrineOutput_D0toD3","UrineFlow_D0toD3",
                          "WBC_D1_LOW","WBC_D1_HIGH",
                          "HEIGHT_Meters","INITIAL_WEIGHT_KG",
                          "Hours_inICUD0toD3",
                          "Mechanical_Hemodynamic_Support",
                          "Unplanned_Admission",
                          "MAX_KDIGO_ICU_D0toD3",
                          "onRRT_ICUD0toD3",
                          "LAST_KDIGO_ICU_D0toD3",
                          "ELX_GRP_1","ELX_GRP_5","ELX_GRP_6","ELX_GRP_7","ELX_GRP_10","ELX_GRP_11","ELX_GRP_12",
                          "ELX_GRP_15","ELX_GRP_16","ELX_GRP_17","ELX_GRP_19","ELX_GRP_21","ELX_GRP_31")


for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  Feature_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Lucas'x Extracion
  #sCr
  Feature_df[i,"Admit_sCr"] <- get_feature_forPt2(curr_id,All_Scr_df,"AdmitICU_SCr","STUDY_PATIENT_ID")
  Feature_df[i,"Baseline_sCr"] <- get_feature_forPt2(curr_id,All_Scr_df,"Baseline_SCr","STUDY_PATIENT_ID")
  Feature_df[i,"Peak_SCr_inICU_D0_D3"] <- get_feature_forPt2(curr_id,All_Scr_df,"Peak_SCr_inICU_D0_D3","STUDY_PATIENT_ID")
  
  #KDIGO
  Feature_df[i,"Admit_KDIGO_ICU"] <- get_feature_forPt2(curr_id,All_KDIGO_df,"Admit_KDIGO_ICU","STUDY_PATIENT_ID")
  Feature_df[i,"MAX_KDIGO_ICU_D0toD3"] <- get_feature_forPt2(curr_id,All_KDIGO_df,"MAX_KDIGO_ICU_D0toD3","STUDY_PATIENT_ID")
  Feature_df[i,"LAST_KDIGO_ICU_D0toD3"] <- get_feature_forPt2(curr_id,All_KDIGO_df,"LAST_KDIGO_ICU_D0toD3","STUDY_PATIENT_ID")
  
  #Septic
  Feature_df[i,"Sepsis_Before_or_At_Admission"] <-get_feature_forPt2(curr_id,All_sepsis_df,"Sepsis_Before_or_At_Admission","STUDY_PATIENT_ID")
  
  #Xilong's extraction
  #demo
  Feature_df[i,"AGE"]    <- get_feature_forPt2(curr_id,Xilong_df1,"age","PATIENT_NUM")
  Feature_df[i,"RACE"]   <- get_feature_forPt2(curr_id,Xilong_df1,"RACE","PATIENT_NUM")
  Feature_df[i,"GENDER"] <- get_feature_forPt2(curr_id,Xilong_df1,"gender","PATIENT_NUM")
  
  #All_ECMO_IABP_MV_VAD_df
  Feature_df[i,"ECMO_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"ECMO","PATIENT_NUM")
  Feature_df[i,"IABP_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"IABP","PATIENT_NUM")
  Feature_df[i,"MV_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"MV","PATIENT_NUM")
  Feature_df[i,"VAD_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"VAD","PATIENT_NUM")
  
  #"Mechanical_Hemodynamic_Support"
  if (Feature_df[i,"VAD_ICUD0toD3"]  == 1 | Feature_df[i,"IABP_ICUD0toD3"] == 1 | Feature_df[i,"ECMO_ICUD0toD3"] == 1 ){
    Feature_df[i,"Mechanical_Hemodynamic_Support"] <- 1
  }else{
    Feature_df[i,"Mechanical_Hemodynamic_Support"] <- 0
  }
  
  #All_HT_WT_RESP_FIO2_df
  Feature_df[i,"FI02_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_FIO2","patient_num")
  Feature_df[i,"FI02_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_FIO2","patient_num")
  Feature_df[i,"RESP_RATE_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_RESPIRATORYRATE","patient_num")
  Feature_df[i,"RESP_RATE_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_RESPIRATORYRATE","patient_num")
  Feature_df[i,"HEIGHT_Meters"] <-   get_feature_forPt2(curr_id,Xilong_df2,"AVG_HEIGHT_Meters","patient_num")
  Feature_df[i,"INITIAL_WEIGHT_KG"] <- get_feature_forPt2(curr_id,Xilong_df2,"AVG_WEIGHT","patient_num")
  
  #BMI
  curr_weight <- Feature_df[i,"INITIAL_WEIGHT_KG"]
  curr_height <- Feature_df[i,"HEIGHT_Meters"]
  curr_BMI <- curr_weight/(curr_height^2)
  Feature_df[i,"BMI"] <- curr_BMI

  #Fluid Overload (Net_wMissing_L is in ml so divided by 1000 to convert it to L)
  curr_net <- get_feature_forPt2(curr_id,Xilong_df2,"Net_wMissing_L","patient_num")/1000
  Feature_df[i,"FluidOverload_inPercentage"] <-  curr_net*100 /(curr_weight)
  
  #"Hours_inICUD0toD3"
  Feature_df[i,"Hours_inICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"ICU_hour_D0_D3orDIS","PATIENT_NUM")
  
  #UrineOutput : Total urine output ICU D0-D3 / measurement time (ml / hr)
  #Urine Flow :  Total urine output ICU D0-D3 / weight / measurement time (ml / kg / hr)
  #Use "UOP_wmissing" in ml, time: ICU_hour_D0_D3orDIS, weight: AVG_WEIGHT
  curr_uop <- get_feature_forPt2(curr_id,Xilong_df2,"UOP_wmissing","patient_num")
  curr_m_time <-  Feature_df[i,"Hours_inICUD0toD3"]
  Feature_df[i,"UrineOutput_D0toD3"] <- curr_uop/curr_m_time
  Feature_df[i,"UrineFlow_D0toD3"] <- curr_uop/curr_weight/curr_m_time
  
  #All_MAP_TEMP_HR_df
  Feature_df[i,"HR_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_HEARTRATE","patient_num")
  Feature_df[i,"HR_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_HEARTRATE","patient_num")
  Feature_df[i,"MAP_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_MAP","patient_num")
  Feature_df[i,"MAP_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_MAP","patient_num")
  Feature_df[i,"Temperature_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_TEMPERATURE","patient_num")
  Feature_df[i,"Temperature_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_TEMPERATURE","patient_num")

  #All_onRRT_df
  Feature_df[i,"onRRT_ICUD0toD3"] <- get_feature_forPt2(curr_id,All_onRRT_df,"onRRT_ICUD0toD3","STUDY_PATIENT_ID")
  
  #All_Nephrotoxin_Vasopressor_df
  Feature_df[i,"Nephrotoxin_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"Nephrotoxins","PATIENT_NUM")
  Feature_df[i,"Vasopressor_ICUD0toD3"] <- get_feature_forPt2(curr_id,Xilong_df1,"Vasopressors","PATIENT_NUM")
  
  #All_pO2_pCO2_pH_df
  Feature_df[i,"PO2_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_PO2","patient_num")
  Feature_df[i,"PO2_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_PO2","patient_num")
  Feature_df[i,"PCO2_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_PCO2","patient_num")
  Feature_df[i,"PCO2_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_PCO2","patient_num")
  Feature_df[i,"PH_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_PH","patient_num")
  Feature_df[i,"PH_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_PH","patient_num")
  
  #Labs
  Feature_df[i,"Bilirubin_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_BILIRUBIN","patient_num")
  Feature_df[i,"Platelets_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_PLATELETS","patient_num")
  Feature_df[i,"Sodium_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_SODIUM","patient_num")
  Feature_df[i,"Sodium_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_SODIUM","patient_num")
  
  Feature_df[i,"Potassium_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_POTASSIUM","patient_num")
  Feature_df[i,"Potassium_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_POTASSIUM","patient_num")
  
  Feature_df[i,"Hematocrit_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_HEMATOCRIT","patient_num")
  Feature_df[i,"Hematocrit_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_HEMATOCRIT","patient_num")
  
  Feature_df[i,"Hemoglobin_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_HEMOGLOBIN","patient_num")
  Feature_df[i,"Hemoglobin_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_HEMOGLOBIN","patient_num")
  
  Feature_df[i,"WBC_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_WBC","patient_num")
  Feature_df[i,"WBC_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_WBC","patient_num")
  
  Feature_df[i,"Bicarbonate_D1_LOW"] <- get_feature_forPt2(curr_id,Xilong_df2,"MIN_HCO3","patient_num")
  Feature_df[i,"Bicarbonate_D1_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_HCO3","patient_num")
  
  
  Feature_df[i,"BUN_D0toD3_HIGH"] <- get_feature_forPt2(curr_id,Xilong_df2,"MAX_BUN","patient_num")
  
  ## Compute Anemia if Hematocrit_D1_LOW and Hemoglobin_D1_LOW and Gender is not Missing
  curr_lowest_Hematocrit <-  Feature_df[i,"Hematocrit_D1_LOW"]
  curr_lowest_Hemoglobin <-  Feature_df[i,"Hemoglobin_D1_LOW"]
  curr_gender <- Feature_df[i,"GENDER"]
  if(is.na(curr_gender) == T | is.na(curr_lowest_Hematocrit) == T | is.na(curr_lowest_Hemoglobin) == T){
    curr_flag <- NA
  }else if (curr_gender == 1 & (curr_lowest_Hematocrit < 39 | curr_lowest_Hemoglobin < 18)){ #For male
    curr_flag <- 1
  }else if (curr_gender == 0 & (curr_lowest_Hematocrit < 36 | curr_lowest_Hemoglobin < 12)){ #for female
    curr_flag <- 1
  }else {
    curr_flag <- 0
  }

  Feature_df[i,"Anemia_D1"] <- curr_flag
  
  #All_ELIX_df
  elix_names <- c("ELX_GRP_1","ELX_GRP_5","ELX_GRP_6","ELX_GRP_7","ELX_GRP_10","ELX_GRP_11","ELX_GRP_12",
                  "ELX_GRP_15","ELX_GRP_16","ELX_GRP_17","ELX_GRP_19","ELX_GRP_21","ELX_GRP_31")
  Feature_df[i,elix_names] <- get_feature_forPt2(curr_id,Xilong_df3,elix_names,"STUDY_PATIENT_ID")

  
  #Unplanned_Admission
  Feature_df[i,"Unplanned_Admission"] <- get_feature_forPt2(curr_id,Xilong_df1,"unPlannedAdmission","PATIENT_NUM")
}

##########################################################################################
#2. Compute missing before imputation
##########################################################################################
missing_table <- get_missing_rate_table(Feature_df,colnames(Feature_df))
missing_table
write.csv(Feature_df,paste0(outdir,"All_Feature_NOTimputed.csv"),row.names = F)


##########################################################################################
#3.imputation median except Anemia
##########################################################################################
features_cols <- colnames(Feature_df)[-which(colnames(Feature_df) %in% c("STUDY_PATIENT_ID","Anemia_D1"))]
Imputed_Feature_df <- median_imputation_func(Feature_df,features_cols)

##########################################################################################
#4. Compute Anemia that was missing using imputed Hematocrit_D1_LOW and Hemoglobin_D1_LOW
##########################################################################################
missing_idxes<- which(is.na(Imputed_Feature_df$Anemia_D1) == T)
for (i in 1:length(missing_idxes)){
  curr_indxes <- missing_idxes[i]
  
  curr_lowest_Hematocrit <- Imputed_Feature_df[curr_indxes,"Hematocrit_D1_LOW"]
  curr_lowest_Hemoglobin <- Imputed_Feature_df[curr_indxes,"Hemoglobin_D1_LOW"]
  curr_gender <- Imputed_Feature_df[curr_indxes,"GENDER"]
  
  if (curr_gender == 1 & (curr_lowest_Hematocrit < 39 | curr_lowest_Hemoglobin < 18)){ #for MALE
    curr_flag <- 1
  }else if (curr_gender == 0 & (curr_lowest_Hematocrit < 36 | curr_lowest_Hemoglobin < 12)){#for FEMALE
    curr_flag <- 1
  }else {
    curr_flag <- 0
  }
  
  Imputed_Feature_df[curr_indxes,"Anemia_D1"] <- curr_flag
}


##########################################################################################
#5.Missing table after imputation
##########################################################################################
missing_table2 <- get_missing_rate_table(Imputed_Feature_df,colnames(Imputed_Feature_df))
missing_table2
write.csv(Imputed_Feature_df,paste0(outdir,"All_Feature_imputed.csv"),row.names = F)


##########################################################################################
#6.Max min norm
##########################################################################################
features_cols <- colnames(Feature_df)[-which(colnames(Feature_df) %in% c("STUDY_PATIENT_ID"))]
Feature_df_normed <- Imputed_Feature_df
for (j in 1:length(features_cols)){
  curr_f <- features_cols[j]
  Feature_df_normed[,curr_f] <- min_max_func(Feature_df_normed[,curr_f])
}

write.csv(Feature_df_normed,paste0(outdir,"All_Feature_imputed_normed.csv"),row.names = F)


##########################################################################################
#7. Get MAX KIDGO ICUD0_D3
##########################################################################################
MAX_KDIGO_ICUD0toD3 <- Feature_df_normed[,c("STUDY_PATIENT_ID","MAX_KDIGO_ICU_D0toD3")]
write.csv(MAX_KDIGO_ICUD0toD3,paste0(outdir,"All_MAX_KDIGO_ICUD0toD3_normed.csv"),row.names = F)

##########################################################################################
#2. Get SOFA and APACHE
##########################################################################################
All_SOFA_APACHE_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID) ,ncol = 3))
colnames(All_SOFA_APACHE_df) <- c("STUDY_PATIENT_ID","SOFA_TOTAL","APACHE_TOTAL")
for(i in 1:nrow(All_SOFA_APACHE_df)){
  curr_id <-analysis_ID[i]
  All_SOFA_APACHE_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  curr_idx <- which(Xilong_df3[,"STUDY_PATIENT_ID"] == curr_id)
  All_SOFA_APACHE_df[i,"SOFA_TOTAL"] <- Xilong_df3[curr_idx,"SOFA"]
  All_SOFA_APACHE_df[i,"APACHE_TOTAL"] <- Xilong_df3[curr_idx,"APACHE"]
  
}


#6.Max min norm
All_SOFA_APACHE_df_normed <- All_SOFA_APACHE_df
features_cols <- c("SOFA_TOTAL","APACHE_TOTAL")
for (j in 1:length(features_cols)){
  curr_f <- features_cols[j]
  All_SOFA_APACHE_df_normed[,curr_f] <- min_max_func(All_SOFA_APACHE_df_normed[,curr_f])
}

SOFA_TOTAL_normed <- All_SOFA_APACHE_df_normed[,c("STUDY_PATIENT_ID","SOFA_TOTAL")]
APACHE_TOTAL_normed <- All_SOFA_APACHE_df_normed[,c("STUDY_PATIENT_ID","APACHE_TOTAL")]

write.csv(SOFA_TOTAL_normed,paste0(outdir,"All_SOFA_TOTAL_normed.csv"),row.names = F)
write.csv(APACHE_TOTAL_normed,paste0(outdir,"All_APACHE_TOTAL_normed.csv"),row.names = F)
