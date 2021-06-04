library(rms)
library(PredictABEL)
library(pROC) #can also use this one for delong's methods
library(Rmisc)
library(caret)
source("TAKI_Ultility.R")

compute_ROCAUC_bootstrap_CI <- function(pred_table){
  roc <- roc(pred_table[,"pred_class"], pred_table[,"Label"])
  CI_AUC <- ci.auc(roc, conf.level=0.95, method=c("bootstrap"), boot.n = 2000, boot.stratified = TRUE, reuse.auc=TRUE,
                   progress = getOption("pROCProgress")$name, parallel=FALSE)
  roc_auc_andCI <- CI_AUC
  res <- t(data.frame(roc_auc_andCI))
  colnames(res) <- c("CI_P2_5","AUC","CI_P97_5")
  rownames(res) <- NULL
  return(res)
}    

################################################################################################
##########   Mortality final performance
################################################################################################

#read perforamnce table
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/mortality/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/motality_final_perf2/"

#read all perf files except the final all perf file
all_perf_files <- list.files(perf_dir,full.names = T)
perf_table_list <- lapply(all_perf_files, read.csv, stringsAsFactors = F)
########
File_perf <- list(NA)
for (i in 1:length(perf_table_list)){ #for each file
  curr_table <- perf_table_list[[i]]
  curr_file <- gsub(perf_dir,"",all_perf_files[[i]])
  
  EachFold_perf_table <- as.data.frame(matrix(NA, nrow =10 ,ncol = 12))
  rownames(EachFold_perf_table) <- paste0("Fold",seq(1,10))
  colnames(EachFold_perf_table) <-c("AUC","Accuracy","Precision","Sensitivity","Specificity","PPV","NPV",
                                    "Calibration_Intercept","Calibration_Slope",
                                    "Taylor_Calibration_Intercept","Taylor_Calibration_Slope","F1")
  for (s in 1:10){ #for each fold 
    curr_v_tab <- curr_table[which(curr_table[,"TestFold"] == paste0("Fold",s)),]
    
    #calibration
    calib_res <- compute_calibration_func(curr_v_tab)
    my_calib_res <- calib_res[[1]]
    taylors_res <- c(calib_res[[2]],calib_res[[3]])
    EachFold_perf_table[s,"Calibration_Intercept"] <- my_calib_res[1]
    EachFold_perf_table[s,"Calibration_Slope"] <- my_calib_res[2]
    EachFold_perf_table[s,"Taylor_Calibration_Intercept"] <- taylors_res[1]
    EachFold_perf_table[s,"Taylor_Calibration_Slope"] <- taylors_res[2]
    
    #Other perforamnce:
    other_res <- compute_performance_func(curr_v_tab)
    EachFold_perf_table[s,"AUC"] <- as.numeric(other_res[1])
    EachFold_perf_table[s,"Accuracy"] <- other_res["Accuracy"]
    EachFold_perf_table[s,"Precision"] <- other_res["Precision"]
    EachFold_perf_table[s,"Sensitivity"] <- other_res["Sensitivity"]
    EachFold_perf_table[s,"Specificity"] <- other_res["Specificity"]
    EachFold_perf_table[s,"PPV"] <- other_res["Pos Pred Value"]
    EachFold_perf_table[s,"NPV"] <- other_res["Neg Pred Value"]
    EachFold_perf_table[s,"F1"] <- other_res["F1"]
  }
  
  
  
  curr_File_perf <- perf_Mean_CI_Folds_func(EachFold_perf_table)
  colnames(curr_File_perf) <- gsub("/|.csv|_pred_table","",curr_file)
  File_perf[[i]] <- curr_File_perf
}

Final_perf <- do.call(cbind,File_perf)
#write.csv(Final_perf, paste0(outdir , "Final_ALLperf.csv"))

#################################################################################################
#1. Seperate output the performance by ML methods
#2. For each method, each cohort, compare with baseline AUC diff
#################################################################################################
method_name_list <- c("Logreg","RF","SVM","Xgb") # "Logreg","RF","SVM","Xgb"
for (i in 1:length(method_name_list)){
  current_method <- method_name_list[i]
  current_perf_indxes <- which(grepl(current_method,colnames(Final_perf)) == T)
  curr_perf <- Final_perf[,current_perf_indxes]
  
  exp_names <- colnames(curr_perf)
  
  AUC_diff_df <- as.data.frame(matrix(NA, nrow = 4 ,ncol = length(exp_names)))
  rownames(AUC_diff_df) <- c("AUC_DIFF_withAPACHE","P_value_withAPACHE","AUC_DIFF_withSOFA","P_value_withSOFA")
  colnames(AUC_diff_df) <- exp_names
  
  for (j in 1:length(exp_names)){
    curr_exp_names <- exp_names[j]
    strings_names <- unlist(strsplit(curr_exp_names,split = "_"))
    curr_cohort <- strings_names[length(strings_names)]
    curr_feature_name <- paste0(strings_names[2:3],collapse = "_")
    
    if (curr_feature_name != "APACHE_SUM" & curr_feature_name!="SOFA_SUM"){
      diffres <- compare_AUC_func(curr_cohort,curr_perf,"APACHE_SUM",curr_feature_name,current_method,perf_dir)
      exact_diff <- round(diffres[[1]],3)
      p_value <-   round(diffres[[2]],3)
      
      diffres <- compare_AUC_func(curr_cohort,curr_perf,"SOFA_SUM",curr_feature_name,current_method,perf_dir)
      exact_diff2 <- round(diffres[[1]],3)
      p_value2 <-   round(diffres[[2]],3)
      AUC_diff_df[,j] <- c(exact_diff,p_value,exact_diff2,p_value2)
    }
  
  }

  sep_final_perf <- rbind(curr_perf,AUC_diff_df)

  reorder_names <- c ("AUC" ,"AUC_DIFF_withSOFA",           
                      "P_value_withSOFA","AUC_DIFF_withAPACHE","P_value_withAPACHE",
                      "Accuracy" ,"Precision",                 
                      "Sensitivity" , "Specificity","F1",
                      "PPV", "NPV","Calibration_Intercept","Calibration_Slope",           
                      "Taylor_Calibration_Intercept","Taylor_Calibration_Slope")
  sep_final_perf <- sep_final_perf[match(reorder_names,rownames(sep_final_perf)),]
  write.csv(sep_final_perf, paste0(outdir , current_method,"_Final_perf.csv"))
}


################################################################################################
##########   MAKE final performance
################################################################################################
                             


#read perforamnce table
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0523/make/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0523/make_final_perf2/"

#read all perf files except the final all perf file
all_perf_files <- list.files(perf_dir,full.names = T)
perf_table_list <- lapply(all_perf_files, read.csv, stringsAsFactors = F)
########
File_perf <- list(NA)
for (i in 1:length(perf_table_list)){ #for each file
  curr_table <- perf_table_list[[i]]
  curr_file <- gsub(perf_dir,"",all_perf_files[[i]])

  EachFold_perf_table <- as.data.frame(matrix(NA, nrow =10 ,ncol = 12))
  rownames(EachFold_perf_table) <- paste0("Fold",seq(1,10))
  colnames(EachFold_perf_table) <-c("AUC","Accuracy","Precision","Sensitivity","Specificity","PPV","NPV",
                                    "Calibration_Intercept","Calibration_Slope",
                                    "Taylor_Calibration_Intercept","Taylor_Calibration_Slope","F1")

  AUC_perf_tb <- as.data.frame(matrix(NA, nrow =10 ,ncol = 3))
  rownames(AUC_perf_tb) <- paste0("DownSample",seq(1,10))
  colnames(AUC_perf_tb) <- c("")
  for (s in 1:10){ #for each down sampling
    #if uky
    curr_v_tab <- curr_table[which(curr_table[,"TrainingSample_Index"] == paste0("S",1)),]
    curr_AUC <- compute_ROCAUC_bootstrap_CI(curr_v_tab)
    

    
    #calibration
    calib_res <- compute_calibration_func(curr_v_tab)
    my_calib_res <- calib_res[[1]]
    taylors_res <- c(calib_res[[2]],calib_res[[3]])
    EachFold_perf_table[s,"Calibration_Intercept"] <- my_calib_res[1]
    EachFold_perf_table[s,"Calibration_Slope"] <- my_calib_res[2]
    EachFold_perf_table[s,"Taylor_Calibration_Intercept"] <- taylors_res[1]
    EachFold_perf_table[s,"Taylor_Calibration_Slope"] <- taylors_res[2]
    
    #Other perforamnce:
    other_res <- compute_performance_func(curr_v_tab)
    EachFold_perf_table[s,"AUC"] <- as.numeric(other_res[1])
    EachFold_perf_table[s,"Accuracy"] <- other_res["Accuracy"]
    EachFold_perf_table[s,"Precision"] <- other_res["Precision"]
    EachFold_perf_table[s,"Sensitivity"] <- other_res["Sensitivity"]
    EachFold_perf_table[s,"Specificity"] <- other_res["Specificity"]
    EachFold_perf_table[s,"PPV"] <- other_res["Pos Pred Value"]
    EachFold_perf_table[s,"NPV"] <- other_res["Neg Pred Value"]
    EachFold_perf_table[s,"F1"] <- other_res["F1"]
    
  }
  
  for (s in 1:10){ #for each fold 
    #if utsw
    curr_v_tab <- curr_table[which(curr_table[,"TestFold"] == paste0("Fold",s) & 
                                   curr_table[,"TrainingSample_Index"] == paste0("S",1)),]
    #calibration
    calib_res <- compute_calibration_func(curr_v_tab)
    my_calib_res <- calib_res[[1]]
    taylors_res <- c(calib_res[[2]],calib_res[[3]])
    EachFold_perf_table[s,"Calibration_Intercept"] <- my_calib_res[1]
    EachFold_perf_table[s,"Calibration_Slope"] <- my_calib_res[2]
    EachFold_perf_table[s,"Taylor_Calibration_Intercept"] <- taylors_res[1]
    EachFold_perf_table[s,"Taylor_Calibration_Slope"] <- taylors_res[2]
    
    #Other perforamnce:
    other_res <- compute_performance_func(curr_v_tab)
    EachFold_perf_table[s,"AUC"] <- as.numeric(other_res[1])
    EachFold_perf_table[s,"Accuracy"] <- other_res["Accuracy"]
    EachFold_perf_table[s,"Precision"] <- other_res["Precision"]
    EachFold_perf_table[s,"Sensitivity"] <- other_res["Sensitivity"]
    EachFold_perf_table[s,"Specificity"] <- other_res["Specificity"]
    EachFold_perf_table[s,"PPV"] <- other_res["Pos Pred Value"]
    EachFold_perf_table[s,"NPV"] <- other_res["Neg Pred Value"]
    EachFold_perf_table[s,"F1"] <- other_res["F1"]
  }
  
  
  
  curr_File_perf <- perf_Mean_CI_Folds_func(EachFold_perf_table)
  colnames(curr_File_perf) <- gsub("/|.csv|_pred_table","",curr_file)
  File_perf[[i]] <- curr_File_perf
}

Final_perf <- do.call(cbind,File_perf)

#################################################################################################
#1. Seperate output the performance by ML methods
#2. For each method, each cohort, compare with baseline AUC diff
#################################################################################################
method_name_list <- c("Logreg","RF","SVM","Xgb") # "Logreg","RF","SVM","Xgb"
for (i in 1:length(method_name_list)){
  current_method <- method_name_list[i]
  current_perf_indxes <- which(grepl(current_method,colnames(Final_perf)) == T)
  curr_perf <- Final_perf[,current_perf_indxes]
  
  exp_names <- colnames(curr_perf)
  
  AUC_diff_df <- as.data.frame(matrix(NA, nrow = 2 ,ncol = length(exp_names)))
  rownames(AUC_diff_df) <- c("AUC_DIFF_withKdigo","P_value_withKdigo")
  colnames(AUC_diff_df) <- exp_names
  
  for (j in 1:length(exp_names)){
    curr_exp_names <- exp_names[j]
    strings_names <- unlist(strsplit(curr_exp_names,split = "_"))
    curr_cohort <- strings_names[length(strings_names)]
    if(grepl("selected_features_norm_option1",curr_exp_names) == T){
      curr_feature_name <- "selected_features_norm_option1"
    }else if (grepl("selected_features_norm_option2",curr_exp_names) == T){
      curr_feature_name <- "selected_features_norm_option2"
    }else if (grepl("selected_features_norm_option3",curr_exp_names) == T){
      curr_feature_name <- "selected_features_norm_option3"
      
    }else{
      curr_feature_name <- paste0(strings_names[2:3],collapse = "_")
      
    }
    if (curr_feature_name != "max_kdigo"){
      diffres <- compare_AUC_func(curr_cohort,curr_perf,"max_kdigo",curr_feature_name,current_method,perf_dir)
      exact_diff <- round(diffres[[1]],3)
      p_value <-   round(diffres[[2]],3)
    
      AUC_diff_df[,j] <- c(exact_diff,p_value)
    }
    
  }
  
  sep_final_perf <- rbind(curr_perf,AUC_diff_df)

  
  reorder_names <- c ("AUC" ,
                      "AUC_DIFF_withKdigo","P_value_withKdigo",
                      "Accuracy" ,"Precision",                 
                      "Sensitivity" , "Specificity","F1",
                      "PPV", "NPV","Calibration_Intercept","Calibration_Slope",           
                      "Taylor_Calibration_Intercept","Taylor_Calibration_Slope")
  sep_final_perf <- sep_final_perf[match(reorder_names,rownames(sep_final_perf)),]
  write.csv(sep_final_perf, paste0(outdir , current_method,"_Final_perf.csv"))
}
