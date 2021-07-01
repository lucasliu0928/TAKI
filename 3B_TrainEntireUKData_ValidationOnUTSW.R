source("TAKI_Ultility.R")
#this script use entire UK data plus down sampleing, and validation on utsw data


#Data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"

#feature file and outcome file names
feature_file <- c("All_Feature_imputed_normed.csv")
outcome_file <- "All_outcome.csv"

####################################################################################### 
######                           Mortality Prediction                      ############
#feature file: All_Feature_imputed_normed.csv
#Outcome file: All_outcome.csv
####################################################################################### 
#Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/All_Clinical_Feature/importance/")

#Outcome column name
outcome_colname <- "Death_inHOSP"

#1.Get model data
train_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$Death_inHOSP)

#2.For each model, do down sampling 10 times on entire UK data, get the average importance matrix
upsample_flag <- 0 #down sample
model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  importance_matrix_list <- list(NA)
  for (s in 1:10){
    seed_num <- s
    #Get sampled data
    train_data_sampled <- Data_Sampling_Func(upsample_flag,train_data,outcome_colname,seed_num)
    #train model
    res <- train_models(train_data_sampled,outcome_colname,model_name)
    curr_model <- res[[1]]
    curr_importance_matrix <- res[[2]]
    curr_importance_matrix$Sample_Indxes <- s
    importance_matrix_list[[s]] <- curr_importance_matrix
  }
  
  #3.get importance matrix for every sampling results
  all_importance_matrix <- do.call(rbind,importance_matrix_list)
  table(all_importance_matrix$Feature)
  
  #4.compute average importance for each feature
  feature_indexes<- which(colnames(train_data) != outcome_colname)
  average_importance_df <- as.data.frame(matrix(NA, nrow = length(feature_indexes), ncol = 2))
  colnames(average_importance_df) <- c("Feature","AVG_Importance")
  for (f in 1:length(feature_indexes)){
    curr_f <- colnames(train_data)[f]
    curr_f_importances_matrix <- all_importance_matrix[which(all_importance_matrix[,"Feature"] == curr_f),]
    if (nrow(curr_f_importances_matrix)  == 0){ #it is possible in XGB, the feature did not show up in importance matrix, cuz it is not importance
      curr_f_avg_importances <- 0
    }else{
       curr_f_avg_importances <- mean(curr_f_importances_matrix[,2])
    }
    average_importance_df[f,"Feature"] <- curr_f
    average_importance_df[f,"AVG_Importance"] <- curr_f_avg_importances
    
}
  
  #scale 0 to 100
  if (model_name != "LogReg"){
    average_importance_df <- scale_0to100_func(average_importance_df,"AVG_Importance") #scale 0-100
  }
  
  #Reorder
  average_importance_df <- average_importance_df[order(abs(average_importance_df$AVG_Importance),decreasing = T),]
  
  write.csv(average_importance_df, paste0(outdir1,"importance_matrix_", model_name, ".csv"))
}



####################################################################################### 
######              MAKE 120 with drop 30   Prediction                     ############
#feature file: All_Feature_imputed_normed.csv
#Outcome file: All_outcome.csv
####################################################################################### 
#Outdir for mortality
outdir2 <- paste0(out_dir,"/make120_drop30/All_Clinical_Feature/importance/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop30"

#1.Get model data
train_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$MAKE_HOSP120_Drop30)

#2.For each model, do down sampling 10 times on entire UK data, get the average importance matrix
upsample_flag <- 0 #down sample
model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  importance_matrix_list <- list(NA)
  for (s in 1:10){
    seed_num <- s
    #Get sampled data
    train_data_sampled <- Data_Sampling_Func(upsample_flag,train_data,outcome_colname,seed_num)
    #train model
    res <- train_models(train_data_sampled,outcome_colname,model_name)
    curr_model <- res[[1]]
    curr_importance_matrix <- res[[2]]
    curr_importance_matrix$Sample_Indxes <- s
    importance_matrix_list[[s]] <- curr_importance_matrix
  }
  
  #3.get importance matrix for every sampling results
  all_importance_matrix <- do.call(rbind,importance_matrix_list)
  table(all_importance_matrix$Feature)
  
  #4.compute average importance for each feature
  feature_indexes<- which(colnames(train_data) != outcome_colname)
  average_importance_df <- as.data.frame(matrix(NA, nrow = length(feature_indexes), ncol = 2))
  colnames(average_importance_df) <- c("Feature","AVG_Importance")
  for (f in 1:length(feature_indexes)){
    curr_f <- colnames(train_data)[f]
    curr_f_importances_matrix <- all_importance_matrix[which(all_importance_matrix[,"Feature"] == curr_f),]
    if (nrow(curr_f_importances_matrix)  == 0){ #it is possible in XGB, the feature did not show up in importance matrix, cuz it is not importance
      curr_f_avg_importances <- 0
    }else{
      curr_f_avg_importances <- mean(curr_f_importances_matrix[,2])
    }
    average_importance_df[f,"Feature"] <- curr_f
    average_importance_df[f,"AVG_Importance"] <- curr_f_avg_importances
    
  }
  
  #scale 0 to 100
  if (model_name != "LogReg"){
    average_importance_df <- scale_0to100_func(average_importance_df,"AVG_Importance") #scale 0-100
  }
  
  #Reorder
  average_importance_df <- average_importance_df[order(abs(average_importance_df$AVG_Importance),decreasing = T),]
  
  write.csv(average_importance_df, paste0(outdir2,"importance_matrix_", model_name, ".csv"))
}


####################################################################################### 
######              MAKE 120 with drop 50   Prediction                     ############
#feature file: All_Feature_imputed_normed.csv
#Outcome file: All_outcome.csv
####################################################################################### 
#Outdir for mortality
outdir3 <- paste0(out_dir,"/make120_drop50/All_Clinical_Feature/importance/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop50"

#1.Get model data
train_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$MAKE_HOSP120_Drop50)

#2.For each model, do down sampling 10 times on entire UK data, get the average importance matrix
upsample_flag <- 0 #down sample
model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  importance_matrix_list <- list(NA)
  for (s in 1:10){
    seed_num <- s
    #Get sampled data
    train_data_sampled <- Data_Sampling_Func(upsample_flag,train_data,outcome_colname,seed_num)
    #train model
    res <- train_models(train_data_sampled,outcome_colname,model_name)
    curr_model <- res[[1]]
    curr_importance_matrix <- res[[2]]
    curr_importance_matrix$Sample_Indxes <- s
    importance_matrix_list[[s]] <- curr_importance_matrix
  }
  
  #3.get importance matrix for every sampling results
  all_importance_matrix <- do.call(rbind,importance_matrix_list)
  table(all_importance_matrix$Feature)
  
  #4.compute average importance for each feature
  feature_indexes<- which(colnames(train_data) != outcome_colname)
  average_importance_df <- as.data.frame(matrix(NA, nrow = length(feature_indexes), ncol = 2))
  colnames(average_importance_df) <- c("Feature","AVG_Importance")
  for (f in 1:length(feature_indexes)){
    curr_f <- colnames(train_data)[f]
    curr_f_importances_matrix <- all_importance_matrix[which(all_importance_matrix[,"Feature"] == curr_f),]
    if (nrow(curr_f_importances_matrix)  == 0){ #it is possible in XGB, the feature did not show up in importance matrix, cuz it is not importance
      curr_f_avg_importances <- 0
    }else{
      curr_f_avg_importances <- mean(curr_f_importances_matrix[,2])
    }
    average_importance_df[f,"Feature"] <- curr_f
    average_importance_df[f,"AVG_Importance"] <- curr_f_avg_importances
    
  }
  
  #scale 0 to 100
  if (model_name != "LogReg"){
    average_importance_df <- scale_0to100_func(average_importance_df,"AVG_Importance") #scale 0-100
  }
  
  #Reorder
  average_importance_df <- average_importance_df[order(abs(average_importance_df$AVG_Importance),decreasing = T),]
  
  write.csv(average_importance_df, paste0(outdir3,"importance_matrix_", model_name, ".csv"))
}