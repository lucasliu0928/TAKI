library(pROC)
library(caret)
library(xgboost)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(caTools)

#Data repreocess
#1.Generate one score for SOFA and APACHE by taking the sum of SOFA/APACHE component
sum_score_func <- function(analysis_df,score_name){
  #Get score sum
  score_col_index <- which(grepl(score_name,colnames(analysis_df))) #find corresponding columns
  score_SUM <- rowSums(analysis_df[,score_col_index]) #take the row sum
  
  #get Id
  IDs <- analysis_df[,"STUDY_PATIENT_ID"]
  
  #return dataframe
  score_df <- cbind.data.frame(IDs,score_SUM)
  colnames(score_df) <- c("STUDY_PATIENT_ID",paste0(score_name,"_SUM"))
  return(score_df)
}


min_max_func <- function(feautre_col){
  normed_col <- (feautre_col-min(feautre_col,na.rm = T))/(max(feautre_col,na.rm = T)-min(feautre_col,na.rm = T))
  return(normed_col)
}


change_feature_name_func <- function(input_df,tochange_colname,target_name){
  col_idx <- which(colnames(input_df) == tochange_colname)
  colnames(input_df)[col_idx] <- target_name
  return(input_df)
}


#####
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
train_xgboost<-function(data_part,train_label,xgb_params,num_rounds,upsample_flag){
  # data_part <- train_data_part
  # train_label <- sampled_train_data[,outcome_index]
    
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
  
  train_matrix <- xgb.DMatrix(data = as.matrix(data_part), label = train_label)
  trained_model <- xgb.train(params = xgb_params,data = train_matrix,nrounds = num_rounds)
  importance_matrix = xgb.importance(model = trained_model)
  
  return(list(trained_model,importance_matrix[,1:2]))
  
}


predict_xgboost <- function(curr_model,test_data_part,test_label,pred_type){
  dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
  predicted_prob <- predict(curr_model, dtest, type = pred_type)
  return(predicted_prob)
}

cv_func <- function(analysis_df,Idxes_fold_df,outcome_colname,model_name,validation_df,upsample_flag,N_sampling){
  # analysis_df <- feature_df
  # outcome_colname <- "died_inp"
  # model_name <- "SVM"
  # validation_df <- UTSW_feature_df
  # upsample_flag <- 0
  # N_sampling <- 10

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
      
     
      if (ncol(sampled_train_data) == 2){  #For data has one feature column
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
        model_svm  <- train(train_data_part, sampled_train_data[,outcome_index],method='svmRadial' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        curr_importance_matrix <- import_res$importance
        curr_importance_matrix$Sample_Index <- paste0("Sample",s)
        curr_importance_matrix$Feature <- rownames(curr_importance_matrix)
        colnames(curr_importance_matrix)[1] <- "Importance_Scaled0_100"
        rownames(curr_importance_matrix) <- NULL
        curr_importance_matrix <- curr_importance_matrix[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
      }else if (model_name == "SVM_TOP"){
        model_svm  <- train(train_data_part, sampled_train_data[,outcome_index],method='svmPoly' , 
                            trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
        curr_model <- model_svm
        import_res <- varImp(curr_model, scale = TRUE)
        importance_matrix_SVM <- import_res$importance
        importance_matrix_SVM$Sample_Index <- paste0("Sample",s)
        importance_matrix_SVM$Feature <- rownames(importance_matrix_SVM)
        colnames(importance_matrix_SVM)[1] <- "Importance_Scaled0_100"
        rownames(importance_matrix_SVM) <- NULL
        importance_matrix_SVM <- importance_matrix_SVM[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
        #model with top 25 features 
        if (nrow(importance_matrix_SVM) >= 25){
          top_features <- as.character(unlist(importance_matrix_SVM[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_SVM[1:nrow(importance_matrix_SVM),"Feature"]))
        }
        
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_svm  <- train(new_train_part, sampled_train_data[,outcome_index],method='svmPoly' , 
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
      }else if (model_name == "RF_TOP"){
        model_rf <- train(train_data_part, sampled_train_data[,outcome_index], method='rf',
                          trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
        curr_model <- model_rf
        import_res <- varImp(curr_model, scale = TRUE)
        importance_matrix_RF <- import_res$importance
        importance_matrix_RF$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        importance_matrix_RF$Feature <- rownames(importance_matrix_RF)
        colnames(importance_matrix_RF)[1] <- "Importance_Scaled0_100"
        rownames(importance_matrix_RF) <- NULL
        importance_matrix_RF <- importance_matrix_RF[,c("Feature","Importance_Scaled0_100","Sample_Index")]
        
        #model with top 25 features 
        if (nrow(importance_matrix_RF) >= 25){
          top_features <- as.character(unlist(importance_matrix_RF[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_RF[1:nrow(importance_matrix_RF),"Feature"]))
        }
        
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_rf <- train(new_train_part, sampled_train_data[,outcome_index], method='rf',
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
        curr_coef <- as.data.frame(curr_model$coefficients)
        zero_coef_feautre <- rownames(curr_coef)[which(is.na(curr_coef$`curr_model$coefficients`) == T)]
        library(reghelper)
        beta_coef_res <- beta(model_logreg, skip = zero_coef_feautre)
        beta_coef <- as.data.frame(beta_coef_res$coefficients)
        m_idxes <- match(gsub(".z","",rownames(beta_coef)), rownames(curr_coef))
        
        curr_importance_matrix <- cbind(rownames(curr_coef),curr_coef)
        curr_importance_matrix$Beta_Coef <- NA
        curr_importance_matrix$Beta_Coef[m_idxes] <- beta_coef$Estimate
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(curr_importance_matrix) == "(Intercept)")
        curr_importance_matrix <- curr_importance_matrix[-inter_index,]
        colnames(curr_importance_matrix) <- c("Feature","Coeff","Beta_Coef","Sample_Index")
        rownames(curr_importance_matrix) <- NULL
        
      }else if (model_name == "LogReg_TOP"){
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = sampled_train_data, family = binomial)
        curr_model <- model_logreg
        curr_coef <- as.data.frame(curr_model$coefficients)
        importance_matrix_LR <- cbind(rownames(curr_coef),curr_coef)
        importance_matrix_LR$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(importance_matrix_LR) == "(Intercept)")
        importance_matrix_LR <- importance_matrix_LR[-inter_index,]
        colnames(importance_matrix_LR) <- c("Feature","Coeff")
        
        #model with top 25 features 
        if (nrow(importance_matrix_LR) >= 25){
          top_features <- as.character(unlist(importance_matrix_LR[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_LR[1:nrow(importance_matrix_LR),"Feature"]))
        }
        new_sampled_train_data<- as.data.frame(sampled_train_data[,c(top_features,outcome_colname)])
        colnames(new_sampled_train_data) <- c(top_features,outcome_colname)
        
        model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = new_sampled_train_data, family = binomial)
        curr_model <- model_logreg
        curr_coef <- as.data.frame(curr_model$coefficients)
        curr_importance_matrix <- cbind(rownames(curr_coef),curr_coef)
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
        #remove intercept
        inter_index <- which(rownames(curr_importance_matrix) == "(Intercept)")
        curr_importance_matrix <- curr_importance_matrix[-inter_index,]
        colnames(curr_importance_matrix) <- c("Feature","Coeff")
        rownames(curr_importance_matrix) <- NULL
        
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
        
        #model with top 25 features 
        if (nrow(importance_matrix_xgb) >= 25){
          top_features <- as.character(unlist(importance_matrix_xgb[1:25,"Feature"]))
        }else{
          top_features <- as.character(unlist(importance_matrix_xgb[1:nrow(importance_matrix_xgb),"Feature"]))
        }
        new_train_part <- as.data.frame(train_data_part[,top_features])
        colnames(new_train_part) <- top_features
        model_top_xgb_res <- train_xgboost(new_train_part,sampled_train_data[,outcome_index],xgb_params,num_rounds,upsample_flag)
        model_top_xgb <- model_top_xgb_res[[1]] #model with top features
        importance_matrix_xgb_top <- model_top_xgb_res[[2]]
        curr_model <- model_top_xgb
        curr_importance_matrix <- importance_matrix_xgb_top
        curr_importance_matrix$Sample_Index <- paste0("TestFold",i, "_Sample",s)
      }
      
      sampling_importance_matrix_list[[s]] <- curr_importance_matrix
      
      #prediction probabiliy of test data and validation data
      if (model_name =="SVM" | model_name =="RF"){
        pred_res <- predict(curr_model, newdata = test_data_part,type = "prob")  
        pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
        
        pred_res <- predict(curr_model, newdata = val_data_part,type = "prob")  
        pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
        
      }else if ( model_name =="SVM_TOP" | model_name =="RF_TOP"){
        new_test_data_part <- as.data.frame(test_data_part[,top_features])
        colnames(new_test_data_part) <- top_features
        pred_res <- predict(curr_model, newdata = new_test_data_part,type = "prob")  
        pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
        
        new_val_data_part <- as.data.frame(val_data_part[,top_features])
        colnames(new_val_data_part) <- top_features
        pred_res <- predict(curr_model, newdata = new_val_data_part,type = "prob")  
        pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
        
      }else if (model_name == "XGB"){
        #convert to xgboost format 
        pred_prob <- predict_xgboost(curr_model,test_data_part,curr_test_data[,outcome_colname],"prob")
        pred_prob_validation <- predict_xgboost(curr_model,val_data_part,validation_df[,outcome_colname],"prob")
        
        
      }else if (model_name == "XGB_TOP"){
        #convert to xgboost format 
        new_test_data_part <- as.data.frame(test_data_part[,top_features])
        colnames(new_test_data_part) <- top_features
        pred_prob <- predict_xgboost(curr_model,new_test_data_part,curr_test_data[,outcome_colname],"prob")
        new_val_data_part <- as.data.frame(val_data_part[,top_features])
        colnames(new_val_data_part) <- colnames(val_data_part[,top_features])
        pred_prob_validation <- predict_xgboost(curr_model,new_val_data_part,validation_df[,outcome_colname],"prob")
        
      }else if (model_name == "LogReg_TOP"){
        new_test_data <- as.data.frame(curr_test_data[,c(top_features,outcome_colname)])
        colnames(new_test_data) <- c(top_features,outcome_colname)
        pred_res <- predict(curr_model, new_test_data, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob <- as.numeric(pred_res)
        
        new_validation_df <- as.data.frame(validation_df[,c(top_features,outcome_colname)])
        colnames(new_validation_df) <- c(top_features,outcome_colname)
        pred_res <- predict(curr_model, new_validation_df, type='response')  #do not need exclude col name from test here, it will not use it for prediciton
        pred_prob_validation <- as.numeric(pred_res)
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




##Performance functions
#compute calibration slope and Intercept
compute_calibration_func <-function(perf_table){
  #perf_table <- curr_table
  
  #compute calibration Intercept and slope and plot
  pred_p <-   perf_table[,"pred_prob"]
  acutal <- as.numeric(as.vector(perf_table[,"Label"]))
  res = val.prob(pred_p,acutal, pl=FALSE)
  calib_res <- res[c("Intercept","Slope")]
  
  #Note: This is what val.prb actually doing
  #check <- glm(acutal ~ log(pred_p/(1-pred_p)),family="binomial")
  #check$coefficients
  
  #another way mentioned in Taylor's email:
  #Slope: Mean Model Output for All With Positive Outcome / Mean Model Output for All With Negative Outcome>
  #Intercept: Mean model output for all patients with a negative result for the outcome.
  all_pos_output <- perf_table[which(perf_table[,"Label"]==1),"pred_prob"]
  all_neg_output <- perf_table[which(perf_table[,"Label"]==0),"pred_prob"]
  possible_slope <- mean(all_pos_output)/mean(all_neg_output)
  possible_Intercept <- mean(all_neg_output)
  
  return(list(calib_res,possible_Intercept,possible_slope))
}

#Compute AUC, ACC, Precision, Sensitivity and...
compute_performance_func <- function(prediction_table){
  #prediction_table <- curr_v_tab
  prediction_table[,"pred_class"] <- as.factor(prediction_table[,"pred_class"])
  prediction_table[,"Label"] <- as.factor(prediction_table[,"Label"])
  
  pred_prob <- prediction_table[,"pred_prob"]
  pred_label <- prediction_table[,"pred_class"]
  actual_label <- prediction_table[,"Label"]
  
  #ROC AUC
  AUC_res <- roc(actual_label, pred_prob,direction = "<",ci =T, auc= T, quiet=TRUE) # control:0, case:1
  AUC <- as.numeric(AUC_res$auc)
  
  #By default, this function uses 2000 bootstraps to calculate a 95% confidence interval.
  AUC_95CI <- c(as.numeric(ci.auc(actual_label,pred_prob,direction = "<", quiet=TRUE))[1],as.numeric(ci.auc(actual_label,pred_prob,direction = "<"))[3])
  
  cm <- confusionMatrix(pred_label, actual_label, positive = "1", dnn = c("Prediction", "Reference"), mode="everything")
  ACC <- cm$overall[1]
  #NOte: Use Exact binomial testto compute CI for accuracy, first value is #number of sucess, #number of failure
  ##Note: Following uses binom.test(c(4883,1283),conf.level = 0.95) 
  ACC_95CI <- cm$overall[c("AccuracyLower","AccuracyUpper")] #
  Precision <- cm$byClass["Precision"]
  Sensitivity <- cm$byClass["Sensitivity"]
  Specificity  <- cm$byClass["Specificity"]
  PPV <- cm$byClass["Pos Pred Value"]
  NPV <- cm$byClass["Neg Pred Value"]
  F1_Score <- cm$byClass["F1"]
  perf_vec <- c(AUC,AUC_95CI,ACC,ACC_95CI,Precision,Sensitivity,Specificity,PPV,NPV,F1_Score)
  return(perf_vec)
}

#Compute report mean and CI for all folds
perf_Mean_CI_Folds_func <-function(Fold_perf_table){
  #Fold_perf_table <- EachFold_perf_table
  
  mean_CI_perf <- as.data.frame(matrix(NA,nrow = ncol(Fold_perf_table),ncol = 1))
  colnames(mean_CI_perf) <- "Mean_(95CI)"
  rownames(mean_CI_perf) <-  colnames(Fold_perf_table)
  for (j in 1:ncol(Fold_perf_table)){
    curr_CI <- CI(Fold_perf_table[,j], ci=0.95)
    curr_CI <- as.numeric(round(curr_CI,2))
    mean_CI_perf[j,1] <- paste0(curr_CI[2], "(",curr_CI[3],"-",curr_CI[1],")")
  }
  return(mean_CI_perf)
}

Test_AUC_diff_func <-function(perf_dir,baseline_model_file,comprison_model_file1){
  baseline_df <- read.csv(paste0(perf_dir,"/",baseline_model_file),stringsAsFactors = F)
  comp_df <- read.csv(paste0(perf_dir,"/",comprison_model_file1),stringsAsFactors = F)
  
  #Combine comparison models
  model_comp_df <- cbind.data.frame(baseline_df[,"Label"],
                                    baseline_df[,"pred_prob"],
                                    comp_df[,"pred_prob"])
  colnames(model_comp_df) <- c("Label","pred_prob_bl","pred_prob1")
  #AUC difference
  roc_bl <- roc(model_comp_df$Label, model_comp_df$pred_prob_bl)
  roc_1 <- roc(model_comp_df$Label, model_comp_df$pred_prob1)
  test_res <- roc.test(roc_bl,roc_1,method = "delong")
  pval <- test_res$p.value
  return(pval)
}

compare_AUC_func <- function(cohort_name,curr_perf,baseline_name,tocompare_modelname,method_name,perf_dir){
  # cohort_name <- "UKY"
  # baseline_name <- "max_kdigo"
  # tocompare_modelname <- curr_feature_name
  # method_name <- current_method
  
  #1.Compute exact diff  
  auc_idx <- which(grepl("AUC",rownames(curr_perf)) == T)  
  
  cohort_indxes <-  which(grepl(cohort_name,colnames(curr_perf)) == T)  
  cohort_perf<- curr_perf[,cohort_indxes]
  baseline_idx <- which(grepl(baseline_name,colnames(cohort_perf)) == T)  
  comparemodel_idx <- which(grepl(tocompare_modelname,colnames(cohort_perf)) == T)  
  baseline_AUC <- as.numeric(unlist(strsplit(cohort_perf[auc_idx,baseline_idx],"\\("))[1])
  compModel_AUC <- as.numeric(unlist(strsplit(cohort_perf[auc_idx,comparemodel_idx],"\\("))[1])
  exact_diffs <- compModel_AUC-baseline_AUC
  
  #2. diff test
  all_perf_files <- list.files(perf_dir)
  cohort_files <- all_perf_files[which(grepl(cohort_name,all_perf_files)==T)]
  methodcohort_files <- cohort_files[which(grepl(method_name,cohort_files)==T)]
  
  baseline_model_file  <- methodcohort_files[which(grepl(baseline_name,methodcohort_files)==T)]
  comprison_model_file <- methodcohort_files[which(grepl(tocompare_modelname,methodcohort_files)==T)]
  
  pvalue <- Test_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file)
  
  return(list(exact_diffs,pvalue))
}
