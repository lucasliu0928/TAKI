library(pROC)
library(caret)

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

cv_func <- function(analysis_df,outcome_colname,model_name,validation_df){
  #get folds info table 
  Idxes_fold_df <- create_fold_func(analysis_df)
  
  
  #code factor as Y for 1, and N for 0, for caret package, we cannot use 1 or 0 for outcome
  analysis_df[,outcome_colname][which(analysis_df[,outcome_colname] == 1)] <- "Y"
  analysis_df[,outcome_colname][which(analysis_df[,outcome_colname] == 0)] <- "N"
  analysis_df[,outcome_colname] <-as.factor(analysis_df[,outcome_colname])
  outcome_index <- which(colnames(analysis_df) == outcome_colname)
  
  #for utsw validation df
  validation_df[,outcome_colname][which(validation_df[,outcome_colname] == 1)] <- "Y"
  validation_df[,outcome_colname][which(validation_df[,outcome_colname] == 0)] <- "N"
  validation_df[,outcome_colname] <-as.factor(validation_df[,outcome_colname])
  outcome_index_utsw <- which(colnames(analysis_df) == outcome_colname)

  pred_table_list <- list(NA)
  pred_table_Validation_list <- list(NA)
  for (i in 1:10){
    curr_test_data <- analysis_df[which(Idxes_fold_df[,"Fold"] == i),]
    curr_train_data <-  analysis_df[which(Idxes_fold_df[,"Fold"] != i),]
    
    
    if (model_name == "SVM"){
      model_svm  <- train(curr_train_data[,-outcome_index], curr_train_data[,outcome_index],method='svmRadial' , 
                          trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
      curr_model <- model_svm
    }else if (model_name == "RF"){
      model_rf <- train(curr_train_data[,-outcome_index], curr_train_data[,outcome_index], method='rf',
                        trControl = trainControl("none", classProbs = TRUE), verbose=F) # Random Forest
      curr_model <- model_rf
    }else if (model_name == "LogReg"){
      model_logreg <- glm(as.formula(paste0(eval(outcome_colname) ,"~.")), data = curr_train_data, family = binomial)
      curr_model <- model_logreg
    }
    
    #prediction of test data and validation data
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
    
    #get Pred class cutoff = 0.5
    pred_class <- pred_prob
    pred_class[which(pred_prob >=0.5)] <- 1
    pred_class[which(pred_prob < 0.5)] <- 0
    pred_class <- as.factor(pred_class)
    
    #Code outcome of test back to 1 and 0
    curr_test_data[,outcome_colname] <- as.character(curr_test_data[,outcome_colname])
    curr_test_data[,outcome_colname][which(curr_test_data[,outcome_colname] == "Y")] <- 1
    curr_test_data[,outcome_colname][which(curr_test_data[,outcome_colname] == "N")] <- 0
    
    #combine pred prob, pred class, acutal label, and fold index
    pred_table <- cbind.data.frame(pred_prob,pred_class,as.numeric(curr_test_data[,outcome_colname]),paste0("Fold",i))
    colnames(pred_table) <- c("pred_prob","pred_class","Label","TestFold")
    pred_table_list[[i]] <- pred_table
    
    
    ####################################################################################################################
    ### Validation data
    ####################################################################################################################
    #validation get Pred class cutoff = 0.5 
    pred_class_validation <- pred_prob_validation
    pred_class_validation[which(pred_prob_validation >=0.5)] <- 1
    pred_class_validation[which(pred_prob_validation < 0.5)] <- 0
    pred_class_validation <- as.factor(pred_class_validation)
    
    #Code outcome of test back to 1 and 0
    validation_df[,outcome_colname] <- as.character(validation_df[,outcome_colname])
    validation_df[,outcome_colname][which(validation_df[,outcome_colname] == "Y")] <- 1
    validation_df[,outcome_colname][which(validation_df[,outcome_colname] == "N")] <- 0
    
    #combine pred prob, pred class, acutal label, and fold index
    pred_table_validation <- cbind.data.frame(pred_prob_validation,pred_class_validation,as.numeric(validation_df[,outcome_colname]),paste0("Fold",i))
    colnames(pred_table_validation) <- c("pred_prob","pred_class","Label","TestFold")
    pred_table_Validation_list[[i]] <- pred_table_validation
    
    
    
  }
  
  final_pred <- do.call(rbind,pred_table_list)
  final_pred_validation <- do.call(rbind,pred_table_Validation_list)
  
  return(list(final_pred,final_pred_validation))
}


#User input
UK_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
feature_file <- "clinical_model_mortality_norm.csv" #"clinical_model_mortality_wTrajectory_norm.csv"
outcome_file <- "Old_outcomes.csv"
outcome_colname <- "died_inp"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/TAKI_Project/Intermediate_Results/mortality/"
outfile_pname <- gsub("_mortality_|_norm.csv","",feature_file)


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
# SVM 
pred_table_SVM <- cv_func(analysis_df,outcome_colname,"SVM",validation_df)
UKY_perd_table <- pred_table_SVM[[1]]
UTSW_perd_table <- pred_table_SVM[[2]]
write.csv(UKY_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UKY.csv"))
write.csv(UTSW_perd_table,paste0(outdir,outfile_pname,"_pred_table_SVM_UTSW.csv"))

# RF
pred_table_RF <- cv_func(analysis_df,outcome_colname,"RF",validation_df)
UKY_perd_table_RF <- pred_table_RF[[1]]
UTSW_perd_table_RF <- pred_table_RF[[2]]
write.csv(UKY_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UKY.csv"))
write.csv(UTSW_perd_table_RF,paste0(outdir,outfile_pname,"_pred_table_RF_UTSW.csv"))

# Logreg
pred_table_Logreg <- cv_func(analysis_df,outcome_colname,"LogReg",validation_df)
UKY_perd_table_Logreg <- pred_table_Logreg[[1]]
UTSW_perd_table_Logreg <- pred_table_Logreg[[2]]
write.csv(UKY_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UKY.csv"))
write.csv(UTSW_perd_table_Logreg,paste0(outdir,outfile_pname,"_pred_table_Logreg_UTSW.csv"))


