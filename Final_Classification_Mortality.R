library(pROC)
library(caret)

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


cv_func <- function(analysis_df,outcome_colname,model_name,validation_df,upsample_flag,N_sampling){
  # analysis_df <- feature_df
  # outcome_colname <- "died_inp"
  # model_name <- "SVM"
  # validation_df <- UTSW_feature_df
  # upsample_flag <- 0
  # N_sampling <- 5
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
  for (i in 1:10){ #10 Fold
    curr_test_data <- analysis_df[which(Idxes_fold_df[,"Fold"] == i),]
    curr_train_data <-  analysis_df[which(Idxes_fold_df[,"Fold"] != i),]
    
    #sampling for traning data
    sampling_pred_table_list <- list(NA)
    sampling_pred_table_Validation_list <- list(NA)
    for (s in 1:N_sampling){
          print(paste0("Fold",i,": Sampling",s))
          seed_num <- s*i
          sampled_train_data <- Model_sampling_func(upsample_flag,curr_train_data,outcome_colname,seed_num)
      
          if (model_name == "SVM"){
            model_svm  <- train(sampled_train_data[,-outcome_index], sampled_train_data[,outcome_index],method='svmPoly' , 
                                trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
            curr_model <- model_svm
          }else if (model_name == "RF"){
            model_rf <- train(sampled_train_data[,-outcome_index], sampled_train_data[,outcome_index], method='rf',
                              trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
            curr_model <- model_rf
          }else if (model_name == "LogReg"){
            model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = sampled_train_data, family = binomial)
            curr_model <- model_logreg
          }
          
          #prediction probabiliy of test data and validation data
          if (model_name =="SVM" | model_name =="RF" ){
              pred_res <- predict(curr_model, newdata = curr_test_data[,-outcome_index],type = "prob")  
              pred_prob <- pred_res[,"Y"] #use the prob for Y or 1
              
              pred_res <- predict(curr_model, newdata = validation_df[,-outcome_index_utsw],type = "prob")  
              pred_prob_validation <- pred_res[,"Y"] #use the prob for Y or 1
              
          }else{
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
  }
  
  final_pred <- do.call(rbind,All_sampling_results_perFold)
  final_pred_validation <- do.call(rbind,All_sampling_results_perFold_validation)
  
  return(list(final_pred,final_pred_validation))
}


#User input
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0210/"

UK_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
feature_file <- "clinical_model_mortality_norm.csv" #"clinical_model_mortality_wTrajectory_norm.csv"
outcome_file <- "Old_outcomes.csv"
outcome_colname <- "died_inp"
outfile_pname <- gsub("_mortality|_norm.csv","",feature_file)


UTSW_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/utsw/"
UTSW_feature_file <- "clinical_model_mortality_norm.csv"
UTSW_outcome_file <- "old_outcomes.csv"
UTSW_outcome_colname <- "Died"

####################################################################################################
##### UK data and outcome
####################################################################################################
#Load feature data
feature_df <- read.csv(paste0(UK_dir,feature_file),stringsAsFactors = F)
#Load Outcome data
outcome_df <- read.csv(paste0(UK_dir,outcome_file),stringsAsFactors = F)
updated_outcome_df <- outcome_df[which(outcome_df[,"id"] %in% feature_df[,"STUDY_PATIENT_ID"]),]
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

# SVM 
pred_table_SVM <- cv_func(analysis_df,outcome_colname,"SVM",validation_df,upsample_flag,10)
UKY_perd_table <- pred_table_SVM[[1]]
UTSW_perd_table <- pred_table_SVM[[2]]
write.csv(UKY_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UKY.csv"))
write.csv(UTSW_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UTSW.csv"))

# RF
pred_table_RF <- cv_func(analysis_df,outcome_colname,"RF",validation_df,upsample_flag,10)
UKY_perd_table_RF <- pred_table_RF[[1]]
UTSW_perd_table_RF <- pred_table_RF[[2]]
write.csv(UKY_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UKY.csv"))
write.csv(UTSW_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UTSW.csv"))

# Logreg
pred_table_Logreg <- cv_func(analysis_df,outcome_colname,"LogReg",validation_df,upsample_flag,10)
UKY_perd_table_Logreg <- pred_table_Logreg[[1]]
UTSW_perd_table_Logreg <- pred_table_Logreg[[2]]
write.csv(UKY_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UKY.csv"))
write.csv(UTSW_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UTSW.csv"))


