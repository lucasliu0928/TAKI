library(pROC)
library(caret)
library(xgboost)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(caTools)

code_Label_YN_func <- function(input_df,label_colname){
  #Recode label as Y for 1, N for 0, cuz for caret package, we cannot use 1 or 0 for outcome
  
  input_df[,label_colname][which(input_df[,label_colname] == 1)] <- "Y"
  input_df[,label_colname][which(input_df[,label_colname] == 0)] <- "N"
  input_df[,label_colname] <-as.factor(input_df[,label_colname])
  return(input_df)
}


code_Label_10_func <- function(input_df,label_colname){
  #Reocde label from YN back to 10 for performance computation
  input_df[,label_colname] <- as.character(input_df[,label_colname])
  input_df[,label_colname][which(input_df[,label_colname] == "Y")] <- 1
  input_df[,label_colname][which(input_df[,label_colname] == "N")] <- 0
  return(input_df)
}

create_fold_func <- function(analysis_df){
  #this function returns row index of analysis_df for each fold
  #create 10 folds
  set.seed(123)
  Idx_Folds <- createFolds(1:nrow(analysis_df), k = 10)
  Fold_list <- list()
  for (i in 1:10){
    curr_ids <- Idx_Folds[[i]]
    curr_fold_name <- rep(i,length(curr_ids))
    Fold_list[[i]] <- cbind(curr_ids,curr_fold_name)
  }
  
  Idxes_fold_df <- do.call(rbind.data.frame,Fold_list)
  colnames(Idxes_fold_df) <- c("Row_indxes","Fold")
  
  return(Idxes_fold_df)
}

#Sampling function
Model_sampling_func <- function(upsample_flag,train_data,label_col_name,seed_num){
  # upsample_flag <- 0
  # train_data <- curr_train_data
  # label_col_name <- outcome_colname
  # seed_num <- 1
  
  #Get label col index
  label_col_index <- which(colnames(train_data) == label_col_name)
  
  #Sampling
  if(upsample_flag==1){ #upsampling
    set.seed(seed_num)
    up_train <- upSample(x = train_data[, -label_col_index],
                         y = as.factor(train_data[,label_col_name]), yname = label_col_name)  
    sampled_train_data <- up_train
    
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_num)
    down_train <- downSample(x = train_data[, -label_col_index],
                             y = as.factor(train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
    
  }else{
    original_train <- train_data
    sampled_train_data <- original_train
  }
  
  #also return real feature name since sampling removed feature names
  colnames(sampled_train_data) <- colnames(train_data)
  
  return(sampled_train_data)
}

#Convert prediction probability to prediction class, threshold = 0.5
get_pred_class_func <- function(predicted_prob){
  pred_class <- predicted_prob
  pred_class[which(predicted_prob >=0.5)] <- 1
  pred_class[which(predicted_prob < 0.5)] <- 0
  pred_class <- as.factor(pred_class)
  return(pred_class)
}

#XGboost functions
#return traingined model and importnce matrix
train_xgboost<-function(train_data_part,train_label,xgb_params,num_rounds,upsample_flag){
  #recode train label back to 0 and 1
  train_label <- as.character(train_label)
  train_label[which(train_label == "Y")] <- 1
  train_label[which(train_label == "N")] <- 0
  train_label <- as.numeric(train_label)
  
  #factorize the label column only if using "reg:squarederror"
  curr_objective <- xgb_params$objective
  if (curr_objective == "reg:squarederror"){
    train_label <- as.factor(train_label)
  }
  
  train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  trained_model <- xgb.train(params = xgb_params,data = train_matrix,nrounds = num_rounds)
  importance_matrix = xgb.importance(model = trained_model)
  
  return(list(trained_model,importance_matrix[,1:2]))
  
}


predict_xgboost <- function(curr_model,test_data_part,test_label,pred_type){
  dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
  predicted_prob <- predict(curr_model, dtest, type = pred_type)
  return(predicted_prob)
}

cv_func <- function(analysis_df,outcome_colname,model_name,validation_df,upsample_flag,N_sampling){
  # analysis_df <- feature_df
  # outcome_colname <- "MAKE"
  # model_name <- "SVM"
  # validation_df <- UTSW_feature_df
  # upsample_flag <- 0
  # N_sampling <- 5
  # 
  #Get folds indexes table 
  Idxes_fold_df <- create_fold_func(analysis_df)
  
  #code factor as Y for 1, and N for 0, for caret package, we cannot use 1 or 0 for outcome
  #For UK df
  analysis_df <- code_Label_YN_func(analysis_df,outcome_colname)
  outcome_index <- which(colnames(analysis_df) == outcome_colname)
  #For utsw validation df
  validation_df <- code_Label_YN_func(validation_df,outcome_colname)
  outcome_index_utsw <- which(colnames(validation_df) == outcome_colname)
  
  
  
  #Training and prediction
  All_sampling_results_perFold <- list(NA)
  All_sampling_results_perFold_validation <- list(NA)
  All_sampling_importance_matrix_perFold <- list(NA)
  for (i in 1:10){ #10 Fold
    curr_test_data <- analysis_df[which(Idxes_fold_df[,"Fold"] == i),]
    curr_train_data <-  analysis_df[which(Idxes_fold_df[,"Fold"] != i),]
    
    #sampling for traning data
    sampling_pred_table_list <- list(NA)
    sampling_pred_table_Validation_list <- list(NA)
    sampling_importance_matrix_list <- list(NA)
    for (s in 1:N_sampling){
      print(paste0("Fold",i,": Sampling",s))
      seed_num <- s*i
      sampled_train_data <- Model_sampling_func(upsample_flag,curr_train_data,outcome_colname,seed_num)
      
      #For data has one feature column
      if (ncol(sampled_train_data) == 2){
        train_data_part <- as.data.frame(sampled_train_data[,-outcome_index])
        colnames(train_data_part) <- colnames(sampled_train_data)[1]
      }else{
        train_data_part <- sampled_train_data[,-outcome_index]
      }
      
      if (ncol(curr_test_data) == 2){
        test_data_part <- as.data.frame(curr_test_data[,-outcome_index])
        colnames(test_data_part) <- colnames(curr_test_data)[1]
      }else{
        test_data_part <- curr_test_data[,-outcome_index]
      }
      
      if (ncol(validation_df) == 2){
        val_data_part <- as.data.frame(validation_df[,-outcome_index_utsw])
        colnames(val_data_part) <- colnames(validation_df)[1]
      }else{
        val_data_part <- validation_df[,-outcome_index_utsw]
      }
      
      #Model
      if (model_name == "SVM"){
        model_svm  <- train(train_data_part, sampled_train_data[,outcome_index],method='svmPoly' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
      }else if (model_name == "RF"){
        model_rf <- train(train_data_part, sampled_train_data[,outcome_index], method='rf',
                          trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
        curr_model <- model_rf
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
      }else if (model_name == "LogReg"){
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = sampled_train_data, family = binomial)
        curr_model <- model_logreg
      }else if (model_name == "XGB"){
        #first find feature importanance 
        num_rounds<- 10
        xgb_params <- list(booster = "gbtree","objective" = "reg:logistic")
        xgb_res <- train_xgboost(train_data_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_xgb <- xgb_res[[1]] #model with all features
        importance_matrix_xgb <- xgb_res[[2]]
        curr_model <- model_xgb
        curr_importance_matrix <- importance_matrix_xgb
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        
      }else if (model_name == "XGB_TOP"){
        #first find feature importanance 
        num_rounds<- 10
        xgb_params <- list(booster = "gbtree","objective" = "reg:logistic")
        xgb_res <- train_xgboost(train_data_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_xgb <- xgb_res[[1]] #model with all features
        importance_matrix_xgb <- xgb_res[[2]]
        
        #model with top 10 features 
        if (nrow(importance_matrix_xgb) >= 10){
          top_features <- as.character(unlist(importance_matrix_xgb[1:10,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_xgb[1:nrow(importance_matrix_xgb),"Feature"]))
        }
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- colnames(train_data_part[,top_features])
        model_top_xgb_res <- train_xgboost(new_train_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_top_xgb <- model_top_xgb_res[[1]] #model with top features
        importance_matrix_xgb_top <- xgb_res[[2]]
        curr_model <- model_top_xgb
        curr_importance_matrix <- importance_matrix_xgb_top
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
      }
      
      if (model_name == "XGB" | model_name == "XGB_TOP" | model_name == "SVM"| model_name == "RF"){
        sampling_importance_matrix_list[[s]] <- curr_importance_matrix
      }
      
      #prediction probabiliy of test data and validation data
      if (model_name =="SVM" | model_name =="RF" ){
        pred_res <- predict(curr_model, newdata = test_data_part,type = "prob")  
        pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
        
        pred_res <- predict(curr_model, newdata = val_data_part,type = "prob")  
        pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
        
      }else if (model_name == "XGB"){
        #convert to xgboost format 
        pred_prob <- predict_xgboost(curr_model,test_data_part,curr_test_data[,outcome_colname],"prob")
        pred_prob_validation <- predict_xgboost(curr_model,val_data_part,validation_df[,outcome_colname],"prob")
        
        
      }else if (model_name == "XGB_TOP"){
        #convert to xgboost format 
        new_test_data_part <- as.data.frame(test_data_part[,top_features])
        colnames(new_test_data_part) <- colnames(test_data_part[,top_features])
        pred_prob <- predict_xgboost(curr_model,new_test_data_part,curr_test_data[,outcome_colname],"prob")
        new_val_data_part <- as.data.frame(val_data_part[,top_features])
        colnames(new_val_data_part) <- colnames(val_data_part[,top_features])
        pred_prob_validation <- predict_xgboost(curr_model,new_val_data_part,validation_df[,outcome_colname],"prob")
        
      }else {
        pred_res <- predict(curr_model, curr_test_data, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob <- as.numeric(pred_res)
        
        pred_res <- predict(curr_model, validation_df, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob_validation <- as.numeric(pred_res)
      }
      
      
      #Prediction class
      pred_class <- get_pred_class_func(pred_prob) #testing 
      pred_class_validation <- get_pred_class_func(pred_prob_validation) #external validation
      
      
      #Code outcome of test back to 1 and 0
      curr_test_data <- code_Label_10_func(curr_test_data,outcome_colname)
      validation_df <- code_Label_10_func(validation_df,outcome_colname)
      
      #combine pred prob, pred class, acutal label, and fold index
      #Testing results
      pred_table <- cbind.data.frame(rownames(curr_test_data),pred_prob,pred_class,as.numeric(curr_test_data[,outcome_colname]),paste0("Fold",i),paste0("S",s))
      colnames(pred_table) <- c("ID","pred_prob","pred_class","Label","TestFold","TrainingSample_Index")
      sampling_pred_table_list[[s]] <- pred_table
      
      #External validation results
      pred_table_validation <- cbind.data.frame(rownames(validation_df),pred_prob_validation,pred_class_validation,as.numeric(validation_df[,outcome_colname]),paste0("Fold",i),paste0("S",s))
      colnames(pred_table_validation) <- c("ID","pred_prob","pred_class","Label","TestFold","TrainingSample_Index")
      sampling_pred_table_Validation_list[[s]] <- pred_table_validation
    } # N_sampling times sampling
    
    All_sampling_results_perFold[[i]] <- do.call(rbind,sampling_pred_table_list)
    All_sampling_results_perFold_validation[[i]] <- do.call(rbind,sampling_pred_table_Validation_list)
    All_sampling_importance_matrix_perFold[[i]] <- do.call(rbind,sampling_importance_matrix_list)
  }
  
  final_pred <- do.call(rbind,All_sampling_results_perFold)
  final_pred_validation <- do.call(rbind,All_sampling_results_perFold_validation)
  final_mportance_matrix<- do.call(rbind,All_sampling_importance_matrix_perFold)
  
  return(list(final_pred,final_pred_validation,final_mportance_matrix))
}
