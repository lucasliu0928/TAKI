source("TAKI_Ultility.R")

compute_perf_externalV_func <- function(N_sampling,pred_df_CKD){
  eachSample_perf_tb <- compute_performance_ExternalValidation_func(N_sampling,pred_df_CKD) ##Compute perforamnce for each sampling
  eachSample_perf_tb[which(is.na(eachSample_perf_tb)==T,arr.ind = T)] <- 0 #basicaly prediction only 1 class in these samples
  CI_perf_tb <- perf_Mean_CI_func(eachSample_perf_tb[,2:13])
  return(CI_perf_tb)
}

compute_perf_CV_func <- function(N_sampling,NFolds, pred_df_CKD){
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,pred_df_CKD) ##Compute perforamnce for each fold with each sampling
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14]) ##get CI and mean perforamnce
  return(CI_perf_tb)
}

report_outcome_instanceN <- function(pred_df){
  pred_df_all <- pred_df[which(pred_df[,"TrainingSample_Index"]=="S1"),] #for One Training Cycle, it is the same for diffrernt S number
  print(table(pred_df_all[,"Label"]))
  N_POS <- length(which(pred_df_all[,"Label"]==1))
  N_TOTAL <- nrow(pred_df_all)
  POS_RATIO <- round(N_POS/N_TOTAL*100,2)
  #print(paste0("N_POS:",N_POS))
  #print(paste0("N_POS (%):",N_POS , "(",POS_RATIO,")"))
  
  return(paste0("N_POS (%):",N_POS , "(",POS_RATIO,")"))
}

get_CKDandnoCKD_IDs <- function(ckd_file){
  #Load CKD ino
  ckd_df  <- read.csv(ckd_file,stringsAsFactors = F)
  
  #CKD and no CKD patient IDs
  ckd_ids_EGFR <-  ckd_df[which(ckd_df[,"CKD"] == 1),"STUDY_PATIENT_ID"]
  nockd_ids_EGFR <- ckd_df[which(ckd_df[,"CKD"] == 0),"STUDY_PATIENT_ID"]
  
  ckd_ids_ICD  <-  ckd_df[which(ckd_df[,"CKD_UseICDCodes"] == 1),"STUDY_PATIENT_ID"]
  nockd_ids_ICD <- ckd_df[which(ckd_df[,"CKD_UseICDCodes"] == 0),"STUDY_PATIENT_ID"]
  
  return(list("CKD_EGFR" = ckd_ids_EGFR,
              "noCKD_EGFR" = nockd_ids_EGFR,
              "CKD_ICD"   =  ckd_ids_ICD,
              "noCKD_ICD" = nockd_ids_ICD))
}


compute_perf_subsetsPts <- function(pred_file,subset_ID_List,N_sampling,NFolds,pop_name){
  # 
  # pred_file <- prediction_file
  # subset_ID_List <- CKD_noCKD_ID_lists
  # 
  #Load prediction df for all patients
  pred_df <- read.csv(pred_file,stringsAsFactors = F)
  
  perftb_list <- list()
  for (i in 1:length(subset_ID_List)){
    curr_ids         <- subset_ID_List[[i]]
    curr_cohort_name <- names(subset_ID_List)[i]
    curr_perd_df     <- pred_df[which(pred_df[,"ID"] %in% curr_ids),] #Prediction df for CKD/noCKD patients
    
    #Report Mortality Ratio
    pos_ratio <- report_outcome_instanceN(curr_perd_df)
    pos_ratio_df <- data.frame(pos_ratio)
    colnames(pos_ratio_df) <- "Mean_(95CI)"
    rownames(pos_ratio_df) <- "POS_Perc"
    
    #Compute performance
    if (pop_name == "UK"){
       perftb <- compute_perf_CV_func(N_sampling,NFolds, curr_perd_df)
    }else if (pop_name == "UTSW"){
      perftb <- compute_perf_externalV_func(N_sampling,curr_perd_df)
    }
    perftb <- rbind(perftb,pos_ratio_df)
    colnames(perftb) <- paste0(curr_cohort_name,"_",colnames(perftb))
    perftb_list[[i]] <- perftb
  }
  
  all_perf_tb <- do.call(cbind,perftb_list)
  
  return(all_perf_tb)
}

###Directory
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/"

##########################################################################################
#    Cross validation UK for CKD pateints
##########################################################################################
#1. get CKD and no CKD patient IDs
cohort_name <- "uky"
ckd_file <- paste0(data_dir,cohort_name,"/","All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv")
CKD_noCKD_ID_lists <- get_CKDandnoCKD_IDs(ckd_file)

#2. Mortality prediction
#Clinical model : SelectedClinicalFeature15Vars
# Method:  RF
folder_name <- "CV_performance/mortality"
model_name  <- "SelectedClinicalFeature15Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
NFolds <- 10
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")

mortality_perf_tb <- compute_perf_subsetsPts(prediction_file,CKD_noCKD_ID_lists,N_sampling,NFolds,"UK")
write.csv(mortality_perf_tb, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_And_noCKD_PTs",".csv"),row.names = T)


#3. MAKE prediction
#Clinical model : SelectedClinicalFeature14Vars
#Method:  RF
folder_name <- "CV_performance/make120_drop50"
model_name  <- "SelectedClinicalFeature14Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
NFolds <- 10
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")

#Compute perforance
MAKE_perf_tb <- compute_perf_subsetsPts(prediction_file,CKD_noCKD_ID_lists,N_sampling,NFolds,"UK")
write.csv(MAKE_perf_tb, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_And_noCKD_PTs",".csv"),row.names = T)


##########################################################################################
#    External validation UTSW for CKD pateints
##########################################################################################
#1. get CKD patiets
cohort_name <- "utsw"
ckd_file <- paste0(data_dir,cohort_name,"/","All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv")
CKD_noCKD_ID_lists <- get_CKDandnoCKD_IDs(ckd_file)

#2. Mortality prediction
#Clinical model : SelectedClinicalFeature15Vars
# Method:  RF
folder_name <- "ExternalV_performance/mortality"
model_name  <- "SelectedClinicalFeature15Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")

#Compute perforance
mortality_perf_tb <- compute_perf_subsetsPts(prediction_file,CKD_noCKD_ID_lists,N_sampling,0,"UTSW")
write.csv(mortality_perf_tb, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_And_noCKD_PTs",".csv"),row.names = T)

#3. MAKE prediction
#Clinical model : SelectedClinicalFeature14Vars
#Method:  RF
folder_name <- "ExternalV_performance/make120_drop50"
model_name  <- "SelectedClinicalFeature14Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")

#Compute perforance
MAKE_perf_tb <- compute_perf_subsetsPts(prediction_file,CKD_noCKD_ID_lists,N_sampling,0,"UTSW")
write.csv(MAKE_perf_tb, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_And_noCKD_PTs",".csv"),row.names = T)
