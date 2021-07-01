source("TAKI_Ultility.R")
get_allmethods_performance <- function(folder_name,file_names,feature_set_name){
  file_dir <- paste0(folder_name,feature_set_name,"/",file_names)
  perfs_list <- list(NA)
  for (i in 1:length(file_dir)){
    curr_file <- file_dir[i]
    curr_method_name <- gsub(paste0(folder_name,feature_set_name,"/Performance_AVG_CI_|.csv"),"",curr_file)
    curr_perf <- read.csv(curr_file ,stringsAsFactors = F)
    colnames(curr_perf) <- c("Metrics",paste0(feature_set_name,"_",curr_method_name,"_Mean_95CI"))
    perfs_list[[i]] <- curr_perf
  }
  
  perfs <- do.call(cbind,perfs_list)
  perfs <- perfs[,-c(3,5,7)] #remove duplicated "metric" col
  return(perfs)
}


####################################################################################### 
##### Cross Validation Mortality performance
#######################################################################################
#Perf dir
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
folder_name <- paste0(perf_dir,"CV_performance/mortality/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
SOFA_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SOFA")
APACHE_perfs <- get_allmethods_performance(folder_name,perf_file_names,"APACHE")
AllClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"AllClinicalFeature")

all_perfs <- cbind(SOFA_perfs,APACHE_perfs,AllClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6,11)]

#2.For each featuresets and each method, compare with baseline AUC diff
AUC_diff <- as.data.frame(matrix(NA, nrow = 2, ncol = ncol(all_perfs)))
colnames(AUC_diff) <- colnames(all_perfs)
AUC_diff$Metrics[1] <- "AUC_Diff"
AUC_diff$Metrics[2] <- "AUC_Diff_Pvalue"

baseline_sets <- "SOFA"
for (i in 2:ncol(AUC_diff)){ #for each feature set
  curr_col <- colnames(AUC_diff)[i]
  
  curr_comp_feature <- unlist(strsplit(curr_col,split = "_"))[1]
  curr_method <- unlist(strsplit(curr_col,split = "_"))[2]
  if (curr_comp_feature != baseline_sets){
        #baseline AUC
        baseline_auc_colidxes <- which(grepl(paste0(baseline_sets,"_",curr_method),colnames(all_perfs))== T)
        baseline_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),baseline_auc_colidxes]
        baseline_auc <- as.numeric(unlist(strsplit(baseline_auc,split = "(",fixed = T))[[1]])
        
        tocompare_auc_colidxes <- which(grepl(paste0(curr_comp_feature,"_",curr_method),colnames(all_perfs))== T)
        tocompare_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),tocompare_auc_colidxes]
        tocompare_auc <- as.numeric(unlist(strsplit(tocompare_auc,split = "(",fixed = T))[[1]])
        
        AUC_diff[1,i] <- round(tocompare_auc - baseline_auc,2)
          
        baseline_pred_file <- paste0(baseline_sets,"/Prediction_",curr_method,".csv")
        tocompare_pred_file <- paste0(curr_comp_feature,"/Prediction_",curr_method,".csv")
        p_value <- Test_AUC_diff_func(folder_name,baseline_pred_file,tocompare_pred_file)
        if (p_value < 0.001){
          p_value <- "< 0.001"
        }
        AUC_diff[2,i] <- p_value
  }else{
    AUC_diff[1,i] <- "-"
    AUC_diff[2,i] <- "-"
  }
}

Final_all_perfs <- rbind(all_perfs,AUC_diff)

reorder_names <- c("AUC" ,"AUC_Diff", "AUC_Diff_Pvalue", "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
Final_all_perfs <- Final_all_perfs[match(reorder_names,Final_all_perfs$Metrics),]
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"))



####################################################################################### 
##### Cross Validation Mortality MAKE drop 30
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
folder_name <- paste0(perf_dir,"CV_performance/make120_drop30/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
AllClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"AllClinicalFeature")

all_perfs <- cbind(KDIGO_perfs,AllClinicalFeature_perfs)
all_perfs <- all_perfs[-6]

#2.For each featuresets and each method, compare with baseline AUC diff
AUC_diff <- as.data.frame(matrix(NA, nrow = 2, ncol = ncol((all_perfs))))
colnames(AUC_diff) <- colnames(all_perfs)
AUC_diff$Metrics[1] <- "AUC_Diff"
AUC_diff$Metrics[2] <- "AUC_Diff_Pvalue"

baseline_sets <- "KDIGO"
for (i in 2:ncol(AUC_diff)){ #for each feature set
  curr_col <- colnames(AUC_diff)[i]
  
  curr_comp_feature <- unlist(strsplit(curr_col,split = "_"))[1]
  curr_method <- unlist(strsplit(curr_col,split = "_"))[2]
  if (curr_comp_feature != baseline_sets){
    #baseline AUC
    baseline_auc_colidxes <- which(grepl(paste0(baseline_sets,"_",curr_method),colnames(all_perfs))== T)
    baseline_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),baseline_auc_colidxes]
    baseline_auc <- as.numeric(unlist(strsplit(baseline_auc,split = "(",fixed = T))[[1]])
    
    tocompare_auc_colidxes <- which(grepl(paste0(curr_comp_feature,"_",curr_method),colnames(all_perfs))== T)
    tocompare_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),tocompare_auc_colidxes]
    tocompare_auc <- as.numeric(unlist(strsplit(tocompare_auc,split = "(",fixed = T))[[1]])
    
    AUC_diff[1,i] <- round(tocompare_auc - baseline_auc,2)
    
    baseline_pred_file <- paste0(baseline_sets,"/Prediction_",curr_method,".csv")
    tocompare_pred_file <- paste0(curr_comp_feature,"/Prediction_",curr_method,".csv")
    p_value <- Test_AUC_diff_func(folder_name,baseline_pred_file,tocompare_pred_file)
    if (p_value < 0.001){
      p_value <- "< 0.001"
    }
    AUC_diff[2,i] <- p_value
  }else{
    AUC_diff[1,i] <- "-"
    AUC_diff[2,i] <- "-"
  }
}

Final_all_perfs <- rbind(all_perfs,AUC_diff)

reorder_names <- c("AUC" ,"AUC_Diff", "AUC_Diff_Pvalue", "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
Final_all_perfs <- Final_all_perfs[match(reorder_names,Final_all_perfs$Metrics),]
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"))


####################################################################################### 
##### Cross Validation Mortality MAKE drop 50
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
folder_name <- paste0(perf_dir,"CV_performance/make120_drop50/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
AllClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"AllClinicalFeature")

all_perfs <- cbind(KDIGO_perfs,AllClinicalFeature_perfs)
all_perfs <- all_perfs[-6]

#2.For each featuresets and each method, compare with baseline AUC diff
AUC_diff <- as.data.frame(matrix(NA, nrow = 2, ncol = ncol((all_perfs))))
colnames(AUC_diff) <- colnames(all_perfs)
AUC_diff$Metrics[1] <- "AUC_Diff"
AUC_diff$Metrics[2] <- "AUC_Diff_Pvalue"

baseline_sets <- "KDIGO"
for (i in 2:ncol(AUC_diff)){ #for each feature set
  curr_col <- colnames(AUC_diff)[i]
  
  curr_comp_feature <- unlist(strsplit(curr_col,split = "_"))[1]
  curr_method <- unlist(strsplit(curr_col,split = "_"))[2]
  if (curr_comp_feature != baseline_sets){
    #baseline AUC
    baseline_auc_colidxes <- which(grepl(paste0(baseline_sets,"_",curr_method),colnames(all_perfs))== T)
    baseline_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),baseline_auc_colidxes]
    baseline_auc <- as.numeric(unlist(strsplit(baseline_auc,split = "(",fixed = T))[[1]])
    
    tocompare_auc_colidxes <- which(grepl(paste0(curr_comp_feature,"_",curr_method),colnames(all_perfs))== T)
    tocompare_auc <-  all_perfs[which(all_perfs$Metrics == "AUC"),tocompare_auc_colidxes]
    tocompare_auc <- as.numeric(unlist(strsplit(tocompare_auc,split = "(",fixed = T))[[1]])
    
    AUC_diff[1,i] <- round(tocompare_auc - baseline_auc,2)
    
    baseline_pred_file <- paste0(baseline_sets,"/Prediction_",curr_method,".csv")
    tocompare_pred_file <- paste0(curr_comp_feature,"/Prediction_",curr_method,".csv")
    p_value <- Test_AUC_diff_func(folder_name,baseline_pred_file,tocompare_pred_file)
    if (p_value < 0.001){
      p_value <- "< 0.001"
    }
    AUC_diff[2,i] <- p_value
  }else{
    AUC_diff[1,i] <- "-"
    AUC_diff[2,i] <- "-"
  }
}

Final_all_perfs <- rbind(all_perfs,AUC_diff)

reorder_names <- c("AUC" ,"AUC_Diff", "AUC_Diff_Pvalue", "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
Final_all_perfs <- Final_all_perfs[match(reorder_names,Final_all_perfs$Metrics),]
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"))
