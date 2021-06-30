source("TAKI_Ultility.R")
library(rms)
library(PredictABEL)
library(pROC) #can also use this one for delong's methods
library(Rmisc)
library(caret)

#this script do 10 folds CV on UK data 
#1. for each fold , down sampling 10 time, each instance get 10 predicted results
#2. compute confidence interval for performance metrics for each fold with each sampling index

#prediction using model of choose
prediction_usingTrainedModels <- function(test_data,outcome_colname,trained_model,model_name){
  #test_data <- curr_test_data
  #trained_model <- model_SVM
  
  #Outcome index 
  outcome_index <- which(colnames(test_data) == outcome_colname)
  
  #test data part
  test_X <-  test_data[,-outcome_index]
  #test label
  test_Y <-  test_data[,outcome_index]
  
  if (model_name == "SVM" | model_name == "RF"){
    #prediction 
    pred_res <- predict(trained_model, newdata = test_X,type = "prob")  
    pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
    
  }else if (model_name == "LogReg"){
    pred_res <- predict(trained_model, test_data, type='response')  #do not need exclude label name from test here, it will not use it for prediciton
    pred_prob <- as.numeric(pred_res)
  }else if (model_name == "XGB"){
    pred_prob <- predict_xgboost(trained_model,test_X,test_Y,"prob")
  }
  return(pred_prob)
}

#Data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
#feature file and outcome file names
outcome_file <- "All_outcome.csv"


####################################################################################### 
######                           Mortality Prediction                      ############
#feature file: All_Feature_imputed_normed.csv, SOFA.csv, APACHE.csv
#Outcome file: All_outcome.csv
####################################################################################### 
feature_file <- c("All_Feature_imputed_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/All_Clinical_Feature/")

#Outcome column name
outcome_colname <- "Death_inHOSP"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$Death_inHOSP)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  final_pred <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  
  write.csv(final_pred, paste0(outdir1,"Prediction_table_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_table_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_table_AVG_CI_", model_name, ".csv"),row.names = T)
}