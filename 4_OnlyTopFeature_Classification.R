source("TAKI_Ultility.R")


get_Topfeature_Data_func <- function(feature_importance_dir,method_name,n_top,analysis_df,outcome_colname){
  feature_importance <- read.xlsx(paste0(feature_importance_dir,method_name, ".xlsx"),sheet = 1)
  TOP_Features <- feature_importance$Feature[1:n_top]
  updated_analysis_df <- analysis_df[,c(TOP_Features,outcome_colname)]
  return(updated_analysis_df)
}



###Only train and predict using top features found by each method
main <- function(outdir,top_f_dir,UK_dir,feature_file,outcome_file,outcome_colname,outfile_pname,UTSW_dir,UTSW_feature_file,UTSW_outcome_file,UTSW_outcome_colname){
  ####################################################################################################
  ##### UK data and outcome
  ####################################################################################################
  #Load feature data
  feature_df <- read.csv(paste0(UK_dir,feature_file),stringsAsFactors = F)
  #Load Outcome data
  outcome_df <- read.csv(paste0(UK_dir,outcome_file),stringsAsFactors = F)
  updated_outcome_df <- outcome_df[which(outcome_df[,"id"] %in% feature_df[,"STUDY_PATIENT_ID"]),]
  #Make outcome
  updated_outcome_df$MAKE <- NA
  MAKE1_idxes <- which(updated_outcome_df[,"died_in_window"] == 1 |
                         updated_outcome_df[,"gfr_drop_50"] == 1 |
                         updated_outcome_df[,"rrt_48hr"] == 1 |
                         updated_outcome_df[,"esrd_manual_revision"] == 1|
                         updated_outcome_df[,"esrd_scm"] == 1|
                         updated_outcome_df[,"esrd_usrds"] == 1)
  updated_outcome_df$MAKE[MAKE1_idxes] <- 1
  updated_outcome_df$MAKE[-MAKE1_idxes] <- 0
  table(updated_outcome_df$MAKE)
  
  #Add outcome to feature data
  feature_df[,outcome_colname] <- updated_outcome_df[match(updated_outcome_df[,"id"],feature_df[,"STUDY_PATIENT_ID"]),outcome_colname]
  rownames(feature_df) <- feature_df$STUDY_PATIENT_ID
  feature_df <- feature_df[,-1] #remove ID col
  
  
  ####################################################################################################
  ##### UTSW validation data and outcome
  ####################################################################################################
  #Load feature data
  UTSW_feature_df <- read.csv(paste0(UTSW_dir,UTSW_feature_file),stringsAsFactors = F)
  
  #Load Outcome data
  UTSW_outcome_df <- read.csv(paste0(UTSW_dir,UTSW_outcome_file),stringsAsFactors = F)
  updated_UTSW_outcome_df <- UTSW_outcome_df[which(UTSW_outcome_df[,"STUDY_PATIENT_ID"] %in% UTSW_feature_df[,"STUDY_PATIENT_ID"]),]
  
  #Add outcome to feature data
  UTSW_feature_df[,UTSW_outcome_colname] <- updated_UTSW_outcome_df[match(updated_UTSW_outcome_df[,"STUDY_PATIENT_ID"],UTSW_feature_df[,"STUDY_PATIENT_ID"]),UTSW_outcome_colname]
  rownames(UTSW_feature_df) <- UTSW_feature_df$STUDY_PATIENT_ID
  UTSW_feature_df <- UTSW_feature_df[,-1] #remove ID col
  #change utsw outcome name to uky outcome name
  colnames(UTSW_feature_df)[which(colnames(UTSW_feature_df) == UTSW_outcome_colname)] <- outcome_colname
  
  
  #analysis Id
  analysis_df <- feature_df
  validation_df <- UTSW_feature_df

  #################################################
  ####### 10-fold cv Training And prediction ####### 
  ################################################# 
  upsample_flag <- 0 #ds
  N_sampling <- 10
  # SVM 
  SVM_analysis_df <- get_Topfeature_Data_func(top_f_dir,"SVM",25,analysis_df,outcome_colname)
  SVM_validation_df <- get_Topfeature_Data_func(top_f_dir,"SVM",25,validation_df,outcome_colname)
  
  pred_table_SVM <- cv_func(SVM_analysis_df,outcome_colname,"SVM",SVM_validation_df,upsample_flag,N_sampling)
  UKY_perd_table <- pred_table_SVM[[1]]
  UTSW_perd_table <- pred_table_SVM[[2]]
  final_mportance_matrix <- pred_table_SVM[[3]]
  write.csv(UKY_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVMTOP_UKY.csv"))
  write.csv(UTSW_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVMTOP_UTSW.csv"))

  # RF
  RF_analysis_df <- get_Topfeature_Data_func(top_f_dir,"RF",25,analysis_df,outcome_colname)
  RF_validation_df <- get_Topfeature_Data_func(top_f_dir,"RF",25,validation_df,outcome_colname)
  pred_table_RF <- cv_func(RF_analysis_df,outcome_colname,"RF",RF_validation_df,upsample_flag,N_sampling)
  UKY_perd_table_RF <- pred_table_RF[[1]]
  UTSW_perd_table_RF <- pred_table_RF[[2]]
  final_mportance_matrix <- pred_table_RF[[3]]
  write.csv(UKY_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RFTOP_UKY.csv"))
  write.csv(UTSW_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RFTOP_UTSW.csv"))

  # Logreg
  #LOGREG
  Logreg_analysis_df <- get_Topfeature_Data_func(top_f_dir,"LogReg",25,analysis_df,outcome_colname)
  Logreg_validation_df <- get_Topfeature_Data_func(top_f_dir,"Logreg",25,validation_df,outcome_colname)
  pred_table_Logreg <- cv_func(Logreg_analysis_df,outcome_colname,"LogReg",Logreg_validation_df,upsample_flag,N_sampling)
  UKY_perd_table_Logreg <- pred_table_Logreg[[1]]
  UTSW_perd_table_Logreg <- pred_table_Logreg[[2]]
  final_mportance_matrix <- pred_table_Logreg[[3]]
  write.csv(UKY_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_LogregTOP_UKY.csv"))
  write.csv(UTSW_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_LogregTOP_UTSW.csv"))

  
  # Xgboost
  #XGB
  XGB_analysis_df <- get_Topfeature_Data_func(top_f_dir,"XGB",25,analysis_df,outcome_colname)
  XGB_validation_df <- get_Topfeature_Data_func(top_f_dir,"XGB",25,validation_df,outcome_colname)
  pred_table_xgb<- cv_func(XGB_analysis_df,outcome_colname,"XGB",XGB_validation_df,upsample_flag,N_sampling)
  UKY_perd_table_xgb <- pred_table_xgb[[1]]
  UTSW_perd_table_xgb <- pred_table_xgb[[2]]
  final_mportance_matrix <- pred_table_xgb[[3]]
  
  write.csv(UKY_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_XgbTOP_UKY.csv"))
  write.csv(UTSW_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_XgbTOP_UTSW.csv"))

}

####################################################################################### 
######                           mortality Prediction                      ############
#1. SOFA_SUM_norm.csv
#2. APACHE_SUM_norm.csv
#3. clinical_model_mortality_norm.csv
#4. clinical_model_mortality_wTrajectory_norm.csv
####################################################################################### 
#User input
experi_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/"
#data dir 
UK_dir <- paste0(experi_dir, "TAKI_Data/uky/")
UTSW_dir <- paste0(experi_dir, "TAKI_Data/utsw/")
#out dir
outdir <- paste0(experi_dir, "Intermediate_Results/Prediction_results0222/mortality/")

#Outcome file :
#UK
outcome_file <- "Old_outcomes.csv"
outcome_colname <- "died_inp"
#UTSW
UTSW_outcome_file <- "old_outcomes.csv"
UTSW_outcome_colname <- "Died"

#feature file list
#feature_file_list <- c("SOFA_SUM_norm.csv","APACHE_SUM_norm.csv","clinical_model_mortality_norm.csv","clinical_model_mortality_wTrajectory_norm.csv")

#Feature file : Only do it for clinical with trajectory model
top_f_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0222/mortality_importance/AVG_Importance_mortality_clinical_model_wTrajectory_"
feature_file <- "clinical_model_mortality_wTrajectory_norm.csv"    #UK
UTSW_feature_file <- "clinical_model_mortality_wTrajectory_norm.csv"   #UTSW
#out file name
outfile_pname <- gsub("_make|_mortality|_norm.csv","",feature_file)
main(outdir,top_f_dir,UK_dir,feature_file,outcome_file,outcome_colname,outfile_pname,UTSW_dir,UTSW_feature_file,UTSW_outcome_file,UTSW_outcome_colname)



####################################################################################### 
###############                    MAKE preidction                         ############
#1. max_kdigo_d03_norm.csv
#1. clinical_model_make_norm.csv
#1. clinical_model_make_wTrajectory_norm.csv
####################################################################################### 

#User input
experi_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/"
#data dir 
UK_dir <- paste0(experi_dir, "TAKI_Data/uky/")
UTSW_dir <- paste0(experi_dir, "TAKI_Data/utsw/")
#out dir
outdir <- paste0(experi_dir, "Intermediate_Results/Prediction_results0222/make/")

#Outcome file :
#UK
outcome_file <- "Old_outcomes.csv"
outcome_colname <- "MAKE"
#UTSW
UTSW_outcome_file <- "old_outcomes.csv"
UTSW_outcome_colname <- "MAKE"

#feature file list
#feature_file_list <- c('max_kdigo_d03_norm.csv',"clinical_model_make_norm.csv","clinical_model_make_wTrajectory_norm.csv")
#Feature file : Only do it for clinical with trajectory model
feature_file <- "clinical_model_make_wTrajectory_norm.csv"    #UK
UTSW_feature_file <- "clinical_model_make_wTrajectory_norm.csv"   #UTSW
top_f_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0222/make_importance/AVG_Importance_MAKE_clinical_model_wTrajectory_"

#out file name
outfile_pname <- gsub("_make|_mortality|_norm.csv","",feature_file)
main(outdir,top_f_dir,UK_dir,feature_file,outcome_file,outcome_colname,outfile_pname,UTSW_dir,UTSW_feature_file,UTSW_outcome_file,UTSW_outcome_colname)

