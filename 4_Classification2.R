source("TAKI_Ultility.R")


main <- function(outdir,outdir2,UK_dir,UTSW_dir, feature_file,outcome_file,outcome_colname,outfile_pname){
  ####################################################################################################
  ##### UK data and outcome
  ####################################################################################################
  #Load feature data
  feature_df <- read.csv(paste0(UK_dir,feature_file),stringsAsFactors = F)
  #Load Outcome data
  outcome_df <- read.csv(paste0(UK_dir,outcome_file),stringsAsFactors = F)

  #Add outcome to feature data
  feature_df[,outcome_colname] <- outcome_df[match(feature_df[,"STUDY_PATIENT_ID"],outcome_df[,"STUDY_PATIENT_ID"]),outcome_colname]
  rownames(feature_df) <- feature_df$STUDY_PATIENT_ID
  feature_df <- feature_df[,-1] #remove ID col
  
  
  ####################################################################################################
  ##### UTSW validation data and outcome
  ####################################################################################################
  #Load feature data
  UTSW_feature_df <- read.csv(paste0(UTSW_dir,feature_file),stringsAsFactors = F)
  
  #Load Outcome data
  UTSW_outcome_df <- read.csv(paste0(UTSW_dir,outcome_file),stringsAsFactors = F)

  #Add outcome to feature data
  UTSW_feature_df[,outcome_colname] <- UTSW_outcome_df[match(UTSW_feature_df[,"STUDY_PATIENT_ID"],UTSW_outcome_df[,"STUDY_PATIENT_ID"]),outcome_colname]
  rownames(UTSW_feature_df) <- UTSW_feature_df$STUDY_PATIENT_ID
  UTSW_feature_df <- UTSW_feature_df[,-1] #remove ID col
  
  #analysis df
  analysis_df <- feature_df
  validation_df <- UTSW_feature_df
  
  #Get folds indexes table 
  Idxes_fold_df <- create_fold_func(analysis_df,10)
  
  #################################################
  ####### 10-fold cv Training And prediction ####### 
  ################################################# 
  upsample_flag <- 0 #0 = downsampling
  N_sampling <- 10
  # SVM 
  pred_table_SVM <- cv_func(analysis_df,Idxes_fold_df,outcome_colname,"SVM",validation_df,upsample_flag,N_sampling)
  UKY_perd_table <- pred_table_SVM[[1]]
  UTSW_perd_table <- pred_table_SVM[[2]]
  final_mportance_matrix <- pred_table_SVM[[3]]
  write.csv(UKY_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UKY.csv"))
  write.csv(UTSW_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_SVM.csv"))
  
  # RF
  pred_table_RF <- cv_func(analysis_df,Idxes_fold_df,outcome_colname,"RF",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_RF <- pred_table_RF[[1]]
  UTSW_perd_table_RF <- pred_table_RF[[2]]
  final_mportance_matrix <- pred_table_RF[[3]]
  write.csv(UKY_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UKY.csv"))
  write.csv(UTSW_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_RF.csv"))
  
  # Logreg
  pred_table_Logreg <- cv_func(analysis_df,Idxes_fold_df,outcome_colname,"LogReg",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_Logreg <- pred_table_Logreg[[1]]
  UTSW_perd_table_Logreg <- pred_table_Logreg[[2]]
  final_mportance_matrix <- pred_table_Logreg[[3]]
  write.csv(UKY_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UKY.csv"))
  write.csv(UTSW_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_Logreg.csv"))
  
  
  # Xgboost
  pred_table_xgb<- cv_func(analysis_df,Idxes_fold_df,outcome_colname,"XGB",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_xgb <- pred_table_xgb[[1]]
  UTSW_perd_table_xgb <- pred_table_xgb[[2]]
  final_mportance_matrix <- pred_table_xgb[[3]]
  
  write.csv(UKY_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_Xgb_UKY.csv"))
  write.csv(UTSW_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_Xgb_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_XGB.csv"))
  
  
  # #Only do top feature model if the dataset have >25 features, in our study, it is the clinical  + trajectory feature model
  # if (ncol(analysis_df) > 26){ #inlcuding outcome column
  #     # Xgboost topfeature
  #     pred_table_xbg_top<- cv_func(analysis_df,outcome_colname,"XGB_TOP",validation_df,upsample_flag,N_sampling)
  #     UKY_perd_table_xbg_top <- pred_table_xbg_top[[1]]
  #     UTSW_perd_table_xbg_top <- pred_table_xbg_top[[2]]
  #     final_mportance_matrix <- pred_table_xbg_top[[3]]
  #     write.csv(UKY_perd_table_xbg_top,paste0(outdir,outfile_pname,"_pred_table_xbg_top_UKY.csv"))
  #     write.csv(UTSW_perd_table_xbg_top,paste0(outdir,outfile_pname,"_pred_table_xbg_top_UTSW.csv"))
  #     write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_XGBTOP.csv"))
  #     
  #     
  #     # SVM topfeature
  #     pred_table_SVM_top<- cv_func(analysis_df,outcome_colname,"SVM_TOP",validation_df,upsample_flag,N_sampling)
  #     UKY_perd_table_SVM_top <- pred_table_SVM_top[[1]]
  #     UTSW_perd_table_SVM_top <- pred_table_SVM_top[[2]]
  #     final_mportance_matrix <- pred_table_SVM_top[[3]]
  #     write.csv(UKY_perd_table_SVM_top,paste0(outdir,outfile_pname,"_pred_table_SVM_top_UKY.csv"))
  #     write.csv(UTSW_perd_table_SVM_top,paste0(outdir,outfile_pname,"_pred_table_SVM_top_UTSW.csv"))
  #     write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_SVMTOP.csv"))
  #     
  #     # RF topfeature
  #     pred_table_RF_top<- cv_func(analysis_df,outcome_colname,"RF_TOP",validation_df,upsample_flag,N_sampling)
  #     UKY_perd_table_RF_top <- pred_table_RF_top[[1]]
  #     UTSW_perd_table_RF_top <- pred_table_RF_top[[2]]
  #     final_mportance_matrix <- pred_table_RF_top[[3]]
  #     write.csv(UKY_perd_table_RF_top,paste0(outdir,outfile_pname,"_pred_table_RF_top_UKY.csv"))
  #     write.csv(UTSW_perd_table_RF_top,paste0(outdir,outfile_pname,"_pred_table_RF_top_UTSW.csv"))
  #     write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_RFTOP.csv"))
  #     
  #     # Logreg topfeature
  #     pred_table_LogReg_top<- cv_func(analysis_df,outcome_colname,"LogReg_TOP",validation_df,upsample_flag,N_sampling)
  #     UKY_perd_table_LogReg_top <- pred_table_LogReg_top[[1]]
  #     UTSW_perd_table_LogReg_top <- pred_table_LogReg_top[[2]]
  #     final_mportance_matrix <- pred_table_LogReg_top[[3]]
  #     write.csv(UKY_perd_table_LogReg_top,paste0(outdir,outfile_pname,"_pred_table_LogReg_top_UKY.csv"))
  #     write.csv(UTSW_perd_table_LogReg_top,paste0(outdir,outfile_pname,"_pred_table_LogReg_top_UTSW.csv"))
  #     write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_LogRegTOP.csv"))
  # }
}

####################################################################################### 
######                           mortality Prediction                      ############
#1. Mortality_SOFA_SUM_norm.csv
#2. Mortality_APACHE_SUM_norm.csv
#3. Mortality_selected_features_norm.csv
####################################################################################### 
#User input
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/Updated_features/"
#data dir 
UK_dir <- paste0(data_dir, "uky/")
UTSW_dir <- paste0(data_dir, "utsw/")

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/"
outdir <- paste0(out_dir,"mortality/")
outdir2 <- paste0(out_dir,"mortality_importance/")

#Outcome file :
outcome_file <- "updated_outcome.csv"
outcome_colname <- "died_inp"

#feature file list
feature_file_list <- c("Mortality_SOFA_SUM_norm.csv",
                       "Mortality_APACHE_SUM_norm.csv",
                       "Mortality_selected_features_norm.csv")

for (f in 1:length(feature_file_list)){
  #Feature file :
  feature_file <- feature_file_list[[f]]    
  #out file name
  outfile_pname <- gsub("_make|_mortality|_norm.csv","",feature_file)
  main(outdir,outdir2,UK_dir,UTSW_dir,feature_file,outcome_file,outcome_colname,outfile_pname)
}


####################################################################################### 
###############                    MAKE preidction                         ############
#1."MAKE_selected_features_norm_option1_withPH.csv.csv",
#2."MAKE_selected_features_norm_option2_withMechHemoSup.csv",
#3."MAKE_selected_features_norm_option3_withPH_And_MechHemoSup",
#4."max_kdigo_d03_norm.csv",

####################################################################################### 
#User input
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/Updated_features/"
#data dir 
UK_dir <- paste0(data_dir, "uky/")
UTSW_dir <- paste0(data_dir, "utsw/")

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/"
outdir <- paste0(out_dir,"make/")
outdir2 <- paste0(out_dir,"make_importance/")

#Outcome file :
outcome_file <- "updated_outcome.csv"
outcome_colname <- "MAKE"

#feature file list
feature_file_list <- c("MAKE_selected_features_norm_option1_withPH.csv.csv",
                       "MAKE_selected_features_norm_option2_withMechHemoSup.csv",
                       "MAKE_selected_features_norm_option3_withPH_And_MechHemoSup.csv",
                       "MAKE_max_kdigo_d03_norm.csv")

for (f in 1:length(feature_file_list)){
  #Feature file :
  feature_file <- feature_file_list[[f]]    #UK
  #out file name
  outfile_pname <- gsub("_make|_mortality|_norm.csv","",feature_file)
  
  main(outdir,outdir2,UK_dir,UTSW_dir,feature_file,outcome_file,outcome_colname,outfile_pname)
}
