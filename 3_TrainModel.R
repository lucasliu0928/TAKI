source("TAKI_Ultility.R")

#Train model of choice and return model and important matrix
train_models <- function(train_data,outcome_colname,model_name){
  
  #Outcome index 
  outcome_index <- which(colnames(feature_df) == outcome_colname)
  
  #Train data part
  train_X <-  train_data[,-outcome_index]
  #Train label
  train_Y <-  train_data[,outcome_index]
  
  if (model_name == "SVM"){
    trained_model  <- train(train_X, train_Y,method='svmRadial' , trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
    importance_matrix <- get_feature_importance_for_SVMRF(trained_model,scale_flag=T)
  }else if (model_name == "RF"){
    trained_model <- train(train_X, train_Y, method='rf', trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
    importance_matrix <- get_feature_importance_for_SVMRF(trained_model,scale_flag=T)
  }else if (model_name == "LogReg"){
    trained_model <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = train_data, family = binomial)
    importance_matrix <- get_feature_importance_for_Logreg(trained_model)
  }else if (model_name == "XGB"){
    xgb_res <- train_xgboost(train_X,train_Y,list(booster = "gbtree","objective" = "reg:logistic"),num_rounds = 10)
    trained_model <- xgb_res[[1]] 
    importance_matrix <- xgb_res[[2]]
    importance_matrix<- scale_0to100_func(importance_matrix) #scale 0-100
  }
  return(list(trained_model,importance_matrix))
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

c("c","a","b")[order(match(c("a","b","c"),c("c","a","b")))]

construct_model_data_func <- function(data_dir,feature_file,outcome_file,outcome_colname){
  #1.Load feature data
  feature_df <- read.csv(paste0(data_dir,feature_file),stringsAsFactors = F)
  #2. Load Outcome data
  outcome_df <- read.csv(paste0(data_dir,outcome_file),stringsAsFactors = F)
  outcome_df <- outcome_df[match(feature_df[,"STUDY_PATIENT_ID"],outcome_df[,"STUDY_PATIENT_ID"]),] #  #reorder outcome to match ID
  
  identical(outcome_df$STUDY_PATIENT_ID,feature_df$STUDY_PATIENT_ID)
  
  #3.Add outcome to feature data as train data
  train_data <- feature_df
  train_data[,outcome_colname] <- outcome_df[,outcome_colname]
  
  #4.Add ID as row name, and remove ID col
  rownames(train_data) <- train_data$STUDY_PATIENT_ID #add ID as
  train_data <- train_data[,-1]


  
  feature_df[,outcome_colname] <- outcome_df[match(outcome_df[,"STUDY_PATIENT_ID"],feature_df[,"STUDY_PATIENT_ID"]),outcome_colname]

}
#train df
train_df <- feature_df
train_df <- code_Label_YN_func(train_df,outcome_colname)

#sampling
seed_num <- 1
upsample_flag <- 0
train_df_sampled <- Data_Sampling_Func(upsample_flag,train_df,outcome_colname,seed_num)

train_models(train_df_sampled,outcome_colname,"XGB")



#1.Down sampling 10 times on entire train data, get the average importance matrix
#2.10-folds CV +  Down sampling 10 times, each intance get 10 predicted results
#3.