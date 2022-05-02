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

#this function compute the AUC difference between 
#baseline feature + LogReg and all other feature with other methods
compute_AUC_diff_func1 <- function(all_perfs_df, bl_feature, bl_method,pred_file_folder){
  #Input: 
  #All_perfs_df: all performance dataframe
  #bl_feature: baseline feature name
  #bl_method: baseline method name
  #pred_file_folder: prediction file folder
  # all_perfs_df <- all_perfs
  # bl_feature <- "SOFA"
  # bl_method  <- "LogReg"
  
  #Get Baseline AUC
  bl_auc_colidxes <- which(grepl(paste0(bl_feature,"_",bl_method),colnames(all_perfs_df))== T)
  bl_auc_rowidxes <- which(all_perfs_df$Metrics == "AUC")
  bl_auc    <-  all_perfs_df[bl_auc_rowidxes,bl_auc_colidxes]
  bl_auc    <- as.numeric(unlist(strsplit(bl_auc,split = "(",fixed = T))[1])
  
  #Comput AUC diff for each other feature set and methods
  AUC_diff <- as.data.frame(matrix(NA, nrow = 2, ncol = ncol(all_perfs_df)))
  colnames(AUC_diff) <- colnames(all_perfs_df)
  AUC_diff$Metrics[1] <-  paste0("AUC_Diff_",bl_feature,"_", bl_method)
  AUC_diff$Metrics[2] <-  paste0("AUC_Diff_Pvalue_",bl_feature,"_", bl_method)
  
  for (i in 2:ncol(AUC_diff)){ #for each feature set
    #Get current column name and feature and method
    curr_col <- colnames(AUC_diff)[i]
    curr_feature <- unlist(strsplit(curr_col,split = "_"))[1]
    curr_method  <- unlist(strsplit(curr_col,split = "_"))[2]
    
    if (curr_feature != bl_feature){
      cur_auc_colidxes <- which(grepl(paste0(curr_feature,"_",curr_method),colnames(all_perfs_df))== T)
      cur_auc_rowidxes <- which(all_perfs_df$Metrics == "AUC")
      cur_auc          <-  all_perfs_df[cur_auc_rowidxes,cur_auc_colidxes]
      cur_auc          <- as.numeric(unlist(strsplit(cur_auc,split = "(",fixed = T))[[1]])
      
      #Compute AUC diff
      AUC_diff[1,i] <- round(cur_auc - bl_auc,2)
      
      #Compute P value 
      bl_pred_file   <- paste0(bl_feature,"/Prediction_",bl_method,".csv")
      curr_pred_file <- paste0(curr_feature,"/Prediction_",curr_method,".csv")
      p_value <- Test_AUC_diff_func(pred_file_folder,bl_pred_file,curr_pred_file)
      if (p_value < 0.001){
        p_value <- "< 0.001"
      }
      AUC_diff[2,i] <- p_value
    }else{
      AUC_diff[1,i] <- "-"
      AUC_diff[2,i] <- "-"
    }
  }
  
  return(AUC_diff)
}

#this function compute the AUC difference between 
#baseline feature and all other feature with corresponding methods
compute_AUC_diff_func2 <- function(all_perfs_df, bl_feature, pred_file_folder){
  #Input: 
  #All_perfs_df: all performance dataframe
  #bl_feature: baseline feature name
  #pred_file_folder: prediction file folder

  #Comput AUC diff for each other feature set and methods
  AUC_diff <- as.data.frame(matrix(NA, nrow = 2, ncol = ncol(all_perfs_df)))
  colnames(AUC_diff) <- colnames(all_perfs_df)
  AUC_diff$Metrics[1] <-  paste0("AUC_Diff_",bl_feature,"_CorrespondMethod")
  AUC_diff$Metrics[2] <-  paste0("AUC_Diff_Pvalue_",bl_feature,"_CorrespondMethod")
  
  for (i in 2:ncol(AUC_diff)){ #for each feature set
    #Get current column name and feature and method
    curr_col <- colnames(AUC_diff)[i]
    curr_feature <- unlist(strsplit(curr_col,split = "_"))[1]
    curr_method  <- unlist(strsplit(curr_col,split = "_"))[2]
    
    #Get Baseline AUC
    bl_auc_colidxes <- which(grepl(paste0(bl_feature,"_",curr_method),colnames(all_perfs_df))== T)
    bl_auc_rowidxes <- which(all_perfs_df$Metrics == "AUC")
    bl_auc    <-  all_perfs_df[bl_auc_rowidxes,bl_auc_colidxes]
    bl_auc    <- as.numeric(unlist(strsplit(bl_auc,split = "(",fixed = T))[1])
    
    if (curr_feature != bl_feature){
      cur_auc_colidxes <- which(grepl(paste0(curr_feature,"_",curr_method),colnames(all_perfs_df))== T)
      cur_auc_rowidxes <- which(all_perfs_df$Metrics == "AUC")
      cur_auc          <-  all_perfs_df[cur_auc_rowidxes,cur_auc_colidxes]
      cur_auc          <- as.numeric(unlist(strsplit(cur_auc,split = "(",fixed = T))[[1]])
      
      #Compute AUC diff
      AUC_diff[1,i] <- round(cur_auc - bl_auc,2)
      
      #Compute P value 
      bl_pred_file   <- paste0(bl_feature,"/Prediction_",curr_method,".csv")
      curr_pred_file <- paste0(curr_feature,"/Prediction_",curr_method,".csv")
      p_value <- Test_AUC_diff_func(pred_file_folder,bl_pred_file,curr_pred_file)
      if (p_value < 0.001){
        p_value <- "< 0.001"
      }
      AUC_diff[2,i] <- p_value
    }else{
      AUC_diff[1,i] <- "-"
      AUC_diff[2,i] <- "-"
    }
  }
  
  return(AUC_diff)
}


####################################################################################### 
##### 1. Cross Validation Mortality performance
#######################################################################################
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"CV_performance/mortality/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
SOFA_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SOFA")
APACHE_perfs <- get_allmethods_performance(folder_name,perf_file_names,"APACHE")
SelectedClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature15Vars")
AllClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"AllClinicalFeature")

all_perfs <- cbind(SOFA_perfs,APACHE_perfs,SelectedClinicalFeature_perfs,AllClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6,11,16)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]

#'@Modified 050122
#2.1 For each featuresets and each method, compare with baseline (SOFA + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"SOFA","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (SOFA + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"SOFA",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two


#3.1.For each featuresets and each method, compare with APACHE + Logreg
AUC_diff_LR   <- compute_AUC_diff_func1(all_perfs,"APACHE","LogReg",folder_name)
#3.2. For each featuresets, compare with baseline (APACHE + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"APACHE",folder_name)
AUC_diff2 <- rbind(AUC_diff_LR, AUC_diff_Corr)  #Combine the two

Final_all_perfs <- rbind(all_perfs,AUC_diff,AUC_diff2)
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)


####################################################################################### 
##### 2.Cross Validation  MAKE drop 50
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"CV_performance/make120_drop50/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
SelectedClinicalFeature_perfs  <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature14Vars")
SelectedClinicalFeature_perfs2 <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature15Vars")
AllClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"AllClinicalFeature")

all_perfs <- cbind(KDIGO_perfs,SelectedClinicalFeature_perfs,SelectedClinicalFeature_perfs2,AllClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6,11,16)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]


#2.1 For each featuresets and each method, compare with baseline (KDIGO + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"KDIGO","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (KDIGO + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"KDIGO",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two

Final_all_perfs <- rbind(all_perfs,AUC_diff)
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)



####################################################################################### 
##### 3. External Validation Mortality performance
#######################################################################################
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"ExternalV_performance/mortality/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")

#1. Performances using different feature
SOFA_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SOFA")
APACHE_perfs <- get_allmethods_performance(folder_name,perf_file_names,"APACHE")
SelectedClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature15Vars")

all_perfs <- cbind(SOFA_perfs,APACHE_perfs,SelectedClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6,11)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]

#'#2.1 For each featuresets and each method, compare with baseline (SOFA + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"SOFA","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (SOFA + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"SOFA",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two

#3.1.For each featuresets and each method, compare with APACHE + Logreg
AUC_diff_LR   <- compute_AUC_diff_func1(all_perfs,"APACHE","LogReg",folder_name)
#3.2. For each featuresets, compare with baseline (APACHE + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"APACHE",folder_name)
AUC_diff2 <- rbind(AUC_diff_LR, AUC_diff_Corr)  #Combine the two

Final_all_perfs <- rbind(all_perfs,AUC_diff,AUC_diff2)
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)


####################################################################################### 
##### 4.External Validation  MAKE drop 50
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"ExternalV_performance/make120_drop50/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
SelectedClinicalFeature_perfs <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature14Vars")

all_perfs <- cbind(KDIGO_perfs,SelectedClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]

#2.1 For each featuresets and each method, compare with baseline (KDIGO + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"KDIGO","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (KDIGO + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"KDIGO",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two
Final_all_perfs <- rbind(all_perfs,AUC_diff)

write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)


#'@TODO:
#'#'@check
identical(AUC_diff,AUC_diff_Corr)
####################################################################################### 
#'@ADDITIONAL_PERFORMANCE
##### 5.Cross Validation  MAKE drop 50 for survivors
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"CV_performance/Surviors_make120_drop50/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
SelectedClinicalFeature_perfs  <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature14Vars")

all_perfs <- cbind(KDIGO_perfs,SelectedClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]

#2.1 For each featuresets and each method, compare with baseline (KDIGO + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"KDIGO","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (KDIGO + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"KDIGO",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two
Final_all_perfs <- rbind(all_perfs,AUC_diff)
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)


####################################################################################### 
#'@ADDITIONAL_PERFORMANCE
##### external Validation  MAKE drop 50 for survivors
#######################################################################################
rm() #clear all vairbales
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
folder_name <- paste0(perf_dir,"ExternalV_performance/Surviors_make120_drop50/")
method_names <- c("LogReg","RF","SVM","XGB")
perf_file_names <- paste0("Performance_AVG_CI_",method_names,".csv")
prediction_file_names <- paste0("Prediction_",method_names,".csv")


#1. Performances using different feature
KDIGO_perfs <- get_allmethods_performance(folder_name,perf_file_names,"KDIGO")
SelectedClinicalFeature_perfs  <- get_allmethods_performance(folder_name,perf_file_names,"SelectedClinicalFeature14Vars")

all_perfs <- cbind(KDIGO_perfs,SelectedClinicalFeature_perfs)
all_perfs <- all_perfs[-c(6)]
#reorder rows
reorder_names <- c("AUC" , "Accuracy" ,"Precision" ,"Sensitivity","Specificity",
                   "F1",  "PPV" ,"NPV" ,"Calibration_Intercept","Calibration_Slope" ,"Taylor_Calibration_Intercept",
                   "Taylor_Calibration_Slope")
all_perfs <- all_perfs[match(reorder_names,all_perfs$Metrics),]


#2.1 For each featuresets and each method, compare with baseline (KDIGO + Logreg) AUC diff
AUC_diff_LR <- compute_AUC_diff_func1(all_perfs,"KDIGO","LogReg",folder_name)
#2.2. For each featuresets, compare with baseline (KDIGO + corresponding ML method) AUC diff
AUC_diff_Corr <- compute_AUC_diff_func2(all_perfs,"KDIGO",folder_name)
AUC_diff <- rbind(AUC_diff_LR, AUC_diff_Corr) #Combine the two
Final_all_perfs <- rbind(all_perfs,AUC_diff)
write.csv(Final_all_perfs, paste0(folder_name,"Performance_AVG_CI_Altogether.csv"),row.names = F)

