library(rms)
library(PredictABEL)
library(pROC) #can also use this one for delong's methods
library(Rmisc)


#compute calibration slope and Intercept
compute_calibration_func <-function(perf_table){
  #perf_table <- curr_table
  
  #compute calibration Intercept and slope and plot
  pred_p <-   perf_table[,"pred_prob"]
  acutal <- as.numeric(as.vector(perf_table[,"Label"]))
  res = val.prob(pred_p,acutal,m=100, cex=.5)
  calib_res <- res[c("Intercept","Slope")]
  
  #Note: This is what val.prb actually doing
  #glm(acutal ~ log(pred_p/(1-pred_p)),family="binomial")
  
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

compute_AUC_diff_func <-function(perf_dir,baseline_model_file,comprison_model_file1){
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

## compute reclassification measures
compute_IDI_NRI_func <-function(perf_dir,baseline_model_file,comprison_model_file1){
  baseline_df <- read.csv(paste0(perf_dir,"/",baseline_model_file),stringsAsFactors = F)
  comp_df <- read.csv(paste0(perf_dir,"/",comprison_model_file1),stringsAsFactors = F)
  
  #Combine comparison models
  model_comp_df <- cbind.data.frame(baseline_df[,"Label"],
                                    baseline_df[,"pred_prob"],
                                    comp_df[,"pred_prob"])
  colnames(model_comp_df) <- c("Label","pred_prob_bl","pred_prob1")
  
  predRisk1 <- model_comp_df$pred_prob_bl
  predRisk2 <- model_comp_df$pred_prob1
  cutoff <- c(0,.5,1)
  #NRI = P(up|event)−P(down|event) + P(down|nonevent)−P(up|nonevent)
  res <- reclassification(data=model_comp_df, cOutcome=1, predrisk1=predRisk1, predrisk2=predRisk2, cutoff)
  
  
  return(res)
}


#read perforamnce table
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/TAKI_Project/Intermediate_Results/mortality"
#perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/TAKI_Project/Intermediate_Results/make"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/TAKI_Project/Intermediate_Results/mortality/"

all_perf_files <- list.files(perf_dir,full.names = T)
#when outcome == make
#remove_RF_1feature_table <- which(grepl("max_kdigo_d03_norm.csv_pred_table_RF_UKY.csv|max_kdigo_d03_norm.csv_pred_table_RF_UTSW.csv",all_perf_files))
#all_perf_files <- all_perf_files[-remove_RF_1feature_table]

perf_table_list <- lapply(all_perf_files, read.csv, stringsAsFactors = F)



File_perf <- list(NA)
for (i in 1:length(perf_table_list)){ #for each file
  curr_table <- perf_table_list[[i]]
  curr_file <- gsub(perf_dir,"",all_perf_files[[i]])
  
  EachFold_perf_table <- as.data.frame(matrix(NA, nrow =10 ,ncol = 12))
  rownames(EachFold_perf_table) <- paste0("Fold",seq(1,10))
  colnames(EachFold_perf_table) <-c("AUC","Accuracy","Precision","Sensitivity","Specificity","PPV","NPV",
                                    "Calibration_Intercept","Calibration_Slope",
                                    "Taylor_Calibration_Intercept","Taylor_Calibration_Slope","F1")
  for (v in 1:10){ #for each cross validation 
    curr_v_tab <- curr_table[which(curr_table[,"TestFold"] == paste0("Fold",v)),]
    
    #calibration
    calib_res <- compute_calibration_func(curr_v_tab)
    my_calib_res <- calib_res[[1]]
    taylors_res <- c(calib_res[[2]],calib_res[[3]])
    EachFold_perf_table[v,"Calibration_Intercept"] <- my_calib_res[1]
    EachFold_perf_table[v,"Calibration_Slope"] <- my_calib_res[2]
    EachFold_perf_table[v,"Taylor_Calibration_Intercept"] <- taylors_res[1]
    EachFold_perf_table[v,"Taylor_Calibration_Slope"] <- taylors_res[2]
    
    #Other perforamnce:
    other_res <- compute_performance_func(curr_v_tab)
    EachFold_perf_table[v,"AUC"] <- as.numeric(other_res[1])
    EachFold_perf_table[v,"Accuracy"] <- other_res["Accuracy"]
    EachFold_perf_table[v,"Precision"] <- other_res["Precision"]
    EachFold_perf_table[v,"Sensitivity"] <- other_res["Sensitivity"]
    EachFold_perf_table[v,"Specificity"] <- other_res["Specificity"]
    EachFold_perf_table[v,"PPV"] <- other_res["Pos Pred Value"]
    EachFold_perf_table[v,"NPV"] <- other_res["Neg Pred Value"]
    EachFold_perf_table[v,"F1"] <- other_res["F1"]
  }
  
  
  
  curr_File_perf <- perf_Mean_CI_Folds_func(EachFold_perf_table)
  colnames(curr_File_perf) <- gsub("/|.csv|_pred_table","",curr_file)
  File_perf[[i]] <- curr_File_perf
}

Final_perf <- do.call(cbind,File_perf)
write.csv(Final_perf, paste0(outdir , "Final_ALLperf.csv"))


####################################################
#compare models with IDI, NRI and AUC difference
####################################################
#1. For mortality
baseline_model_file  <- "sofa_apache_pred_table_Logreg_UKY.csv"
comprison_model_file1 <- "clinical_modelnorm.csv_pred_table_Logreg_UKY.csv" 
comprison_model_file2 <- "clinical_modelwTrajectory_pred_table_Logreg_UKY.csv" 

pvalue1 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file1)
pvalue2 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file2)
pvalue1
pvalue2
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)


#UTSW
baseline_model_file  <- "sofa_apache_pred_table_Logreg_UTSW.csv"
comprison_model_file1 <- "clinical_modelnorm.csv_pred_table_Logreg_UTSW.csv" 
comprison_model_file2 <- "clinical_modelwTrajectory_pred_table_Logreg_UTSW.csv" 

pvalue1 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file1)
pvalue2 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file2)
pvalue1
pvalue2
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)

#2. For MAKE
baseline_model_file  <- "max_kdigo_d03_norm.csv_pred_table_Logreg_UKY.csv"
comprison_model_file1 <- "clinical_model_pred_table_Logreg_UKY.csv" 
comprison_model_file2 <- "clinical_model_make_wTrajectory_norm.csv_pred_table_Logreg_UKY.csv" 

pvalue1 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file1)
pvalue2 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file2)
pvalue1
pvalue2
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)


#UTSW
baseline_model_file  <- "max_kdigo_d03_norm.csv_pred_table_Logreg_UTSW.csv"
comprison_model_file1 <- "clinical_model_pred_table_Logreg_UTSW.csv" 
comprison_model_file2 <- "clinical_model_make_wTrajectory_norm.csv_pred_table_Logreg_UTSW.csv" 

pvalue1 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file1)
pvalue2 <- compute_AUC_diff_func(perf_dir,baseline_model_file,comprison_model_file2)
pvalue1
pvalue2
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)
