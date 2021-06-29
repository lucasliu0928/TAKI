source("TAKI_Ultility.R")


train_model <- function(outdir,outdir2,data_dir, feature_file,outcome_file,outcome_colname,outfile_pname){
  ####################################################################################################
  ##### UK data and outcome
  ####################################################################################################
  #Load feature data
  feature_df <- read.csv(paste0(data_dir,feature_file),stringsAsFactors = F)
  #Load Outcome data
  outcome_df <- read.csv(paste0(data_dir,outcome_file),stringsAsFactors = F)
  
  #Add outcome to feature data
  feature_df[,outcome_colname] <- outcome_df[match(outcome_df[,"STUDY_PATIENT_ID"],feature_df[,"STUDY_PATIENT_ID"]),outcome_colname]
  rownames(feature_df) <- feature_df$STUDY_PATIENT_ID
  feature_df <- feature_df[,-1] #remove ID col
  
  #train df
  train_df <- feature_df
  #Get folds indexes table 
  Idxes_fold_df <- create_fold_func(train_df)
  
  #################################################
  ####### 10-fold cv Training And prediction ####### 
  ################################################# 
  upsample_flag <- 0 #0 = downsampling
  N_sampling <- 10
  # SVM 
  pred_table_SVM <- cv_func(train_df,Idxes_fold_df,outcome_colname,"SVM",validation_df,upsample_flag,N_sampling)
  UKY_perd_table <- pred_table_SVM[[1]]
  UTSW_perd_table <- pred_table_SVM[[2]]
  final_mportance_matrix <- pred_table_SVM[[3]]
  write.csv(UKY_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UKY.csv"))
  write.csv(UTSW_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_SVM.csv"))
  
  # RF
  pred_table_RF <- cv_func(train_df,Idxes_fold_df,outcome_colname,"RF",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_RF <- pred_table_RF[[1]]
  UTSW_perd_table_RF <- pred_table_RF[[2]]
  final_mportance_matrix <- pred_table_RF[[3]]
  write.csv(UKY_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UKY.csv"))
  write.csv(UTSW_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_RF.csv"))
  
  # Logreg
  pred_table_Logreg <- cv_func(train_df,Idxes_fold_df,outcome_colname,"LogReg",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_Logreg <- pred_table_Logreg[[1]]
  UTSW_perd_table_Logreg <- pred_table_Logreg[[2]]
  final_mportance_matrix <- pred_table_Logreg[[3]]
  write.csv(UKY_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UKY.csv"))
  write.csv(UTSW_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_Logreg.csv"))
  
  
  # Xgboost
  pred_table_xgb<- cv_func(train_df,Idxes_fold_df,outcome_colname,"XGB",validation_df,upsample_flag,N_sampling)
  UKY_perd_table_xgb <- pred_table_xgb[[1]]
  UTSW_perd_table_xgb <- pred_table_xgb[[2]]
  final_mportance_matrix <- pred_table_xgb[[3]]
  
  write.csv(UKY_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_Xgb_UKY.csv"))
  write.csv(UTSW_perd_table_xgb,paste0(outdir,outfile_pname,"_pred_table_Xgb_UTSW.csv"))
  write.csv(final_mportance_matrix,paste0(outdir2,outcome_colname,"_",outfile_pname,"_importance_matrix_XGB.csv"))

}



####################################################################################### 
######                           mortality Prediction                      ############
#1. SOFA_SUM_norm.csv
#2. APACHE_SUM_norm.csv
#3. All_Feature_imputed_normed.csv
####################################################################################### 
#User input
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
outdir <- paste0(out_dir,"mortality/")
outdir2 <- paste0(out_dir,"mortality_importance/")

#Outcome file :
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#feature file list
feature_file <- c("All_Feature_imputed_normed.csv")

#1.Load feature data
feature_df <- read.csv(paste0(data_dir,feature_file),stringsAsFactors = F)
#2. Load Outcome data
outcome_df <- read.csv(paste0(data_dir,outcome_file),stringsAsFactors = F)

#Add outcome to feature data
feature_df[,outcome_colname] <- outcome_df[match(outcome_df[,"STUDY_PATIENT_ID"],feature_df[,"STUDY_PATIENT_ID"]),outcome_colname]
rownames(feature_df) <- feature_df$STUDY_PATIENT_ID
feature_df <- feature_df[,-1] #remove ID col

#train df
train_df <- feature_df
train_df <- code_Label_YN_func(train_df,outcome_colname)
outcome_index <- which(colnames(feature_df) == outcome_colname)

#sampling
sampled_train_data <- Model_sampling_func(upsample_flag,curr_train_data,outcome_colname,seed_num)

#SVM 
train_X <-  train_df[,-outcome_index]
train_Y <-  train_df[,outcome_index]

#SVM model
model_svm  <- train(train_X, train_Y,method='svmRadial' , trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
importance_matrix_svm <- get_feature_importance_for_SVMRF(model_svm,scale_flag=T)

#RF
model_rf <- train(train_X, train_Y, method='rf', trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
importance_matrix_rf <- get_feature_importance_for_SVMRF(model_rf,scale_flag=T)

#LR
model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = train_df, family = binomial)
importance_matrix_logreg <- get_feature_importance_for_Logreg(model_logreg)

#XGB
xgb_res <- train_xgboost(train_X,train_Y,list(booster = "gbtree","objective" = "reg:logistic"),num_rounds = 10,upsample_flag)
model_xgb <- xgb_res[[1]] #model with all features
importance_matrix_xgb <- xgb_res[[2]]
curr_model <- model_xgb
curr_importance_matrix <- importance_matrix_xgb
curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)


#compute importance 
get_feature_importance_for_SVMRF <- function(trained_model,scale_flag){
  #trained_model <- model_svm
    
  #get importance matrix
  import_res <- varImp(trained_model, scale = scale_flag) #The area under ROC is used as the measure of variable importance for each predictor
  importance_matrix <- import_res$importance
  
  #Add feature name and remove rownames
  importance_matrix$Feature <- rownames(importance_matrix)
  rownames(importance_matrix) <- NULL
  #change column name
  if (scale_flag == T){
    colnames(importance_matrix)[1] <- "Importance_Scaled0_100"
    importance_matrix <- importance_matrix[,c("Feature","Importance_Scaled0_100")]
    
  }else{
    colnames(importance_matrix)[1] <- "Importance_NOT_Scaled"
    importance_matrix <- importance_matrix[,c("Feature","Importance_NOT_Scaled")]
    
  }

  return(importance_matrix)
}


get_feature_importance_for_Logreg <- function(trained_model){
  #get model coeffient
  model_coef <- as.data.frame(trained_model$coefficients)
  
  #get importance matrix
  importance_matrix <- cbind(rownames(model_coef),model_coef)
  importance_matrix <- importance_matrix[- which(rownames(importance_matrix) == "(Intercept)"),] #remove intercept
  colnames(importance_matrix) <- c("Feature","Coeff")
  rownames(importance_matrix) <- NULL
  return(importance_matrix)
}


get_feature_importance_for_XGboost <- function(trained_model,scale_flag){
  #trained_model <- model_svm
  
  #get importance matrix
  import_res <- varImp(trained_model, scale = scale_flag) #The area under ROC is used as the measure of variable importance for each predictor
  importance_matrix <- import_res$importance
  
  #Add feature name and remove rownames
  importance_matrix$Feature <- rownames(importance_matrix)
  rownames(importance_matrix) <- NULL
  #change column name
  if (scale_flag == T){
    colnames(importance_matrix)[1] <- "Importance_Scaled0_100"
    importance_matrix <- importance_matrix[,c("Feature","Importance_Scaled0_100")]
    
  }else{
    colnames(importance_matrix)[1] <- "Importance_NOT_Scaled"
    importance_matrix <- importance_matrix[,c("Feature","Importance_NOT_Scaled")]
    
  }
  
  return(importance_matrix)
}
