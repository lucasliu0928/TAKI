library(stringr)
library(openxlsx)

norm_0to100_func <- function(xgb_importance_matrix){
  #normlize for each testfold with each sample
  unique_foldsamples <- unique(xgb_importance_matrix$Sample_Index)
  
  for (i in 1:length(unique_foldsamples)){
    curr_foldsp <- unique_foldsamples[i]
    curr_foldsp_idxes <- which(xgb_importance_matrix[,"Sample_Index"] == curr_foldsp ) 
    col_value <- xgb_importance_matrix[curr_foldsp_idxes,"Gain"]
    minv <- min(col_value,na.rm = T)
    maxv <- max(col_value,na.rm = T)
    normed_col_value <- (col_value - minv) / (maxv - minv) * 100
    xgb_importance_matrix[curr_foldsp_idxes,"Gain"] <- normed_col_value
  }
  
  updated_xgb_importance_matrix <- xgb_importance_matrix
  return(updated_xgb_importance_matrix)
}


####Mortality
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/"
importance_dir <- paste0(data_dir,"Prediction_results0216/mortality_importance")
all_importance_files <- list.files(importance_dir,full.names = T)
#do not compute XGBTOP, cuz it only has top features, and it is the same as XGB
all_importance_files <- all_importance_files[-which(grepl("XGBTOP",all_importance_files))]


for (i in 1:length(all_importance_files)){
  curr_file <- all_importance_files[i]
  curr_import_matrix <- read.csv(curr_file,stringsAsFactors = F,row.names = 1)
  model_name <- str_match(curr_file, "died_inp_\\s*(.*?)\\s*_importance_")[,2]
  prediction_task <- "mortality"
  
  #NOTE: for XGB importance, some of feature were droped when compute feature importance, we just manully add 0 to then
  if (grepl("wTrajectory",model_name) == T){
    data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
    model_data <-read.csv(paste0(data_dir, "clinical_model_mortality_wTrajectory_norm.csv"),stringsAsFactors = F)
    unique_features <- colnames(model_data)[-which(colnames(model_data) == "STUDY_PATIENT_ID")]
  }else{
    unique_features <- unique(curr_import_matrix[,"Feature"])
    length(unique_features)
  }
  
  score_df <- as.data.frame(matrix(NA, nrow = length(unique_features),ncol = 5))
  colnames(score_df) <- c("Feature","Importance","Method","Prediction","Model")
  score_df$Method <- str_match(curr_file, "matrix_\\s*(.*?)\\s*.csv")[,2]
  score_df$Prediction <- prediction_task
  score_df$Model <- model_name
  
  for (f in 1:length(unique_features)){ #for each feature, avg all scores over all folds and sample
    curr_feature <- unique_features[f]
    
    if (grepl("xgb",curr_file,ignore.case = T) == T) {
      score_col <-"Gain"
      curr_import_matrix <- norm_0to100_func(curr_import_matrix)
    }else{
      score_col <-"Importance_Scaled0_100"
      
    }
    
    if (curr_feature %in% curr_import_matrix[,"Feature"]){
      curr_score <- curr_import_matrix[which(curr_import_matrix[,"Feature"] == curr_feature),score_col]
      avg_score <- mean(curr_score,na.rm = T)
    }else{#  #NOTE: for XGB importance, some of feature were droped when compute feature importance, we just manully add 0 to then
      avg_score <- 0 
    }
    score_df$Feature[f] <- curr_feature
    score_df$Importance[f] <- avg_score
  }
  score_df$Importance <- round(score_df$Importance,2)
  sorted_score_df <- score_df[order(score_df$Importance,decreasing = T),]
  outname <- paste0(importance_dir, "/",'AVG_Importance_',unique(sorted_score_df$Prediction),"_",unique(sorted_score_df$Model),"_",unique(sorted_score_df$Method),".xlsx")
  write.xlsx(sorted_score_df,outname)
}


####MAKE
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/"
importance_dir <- paste0(data_dir,"Prediction_results0216/make_importance")
all_importance_files <- list.files(importance_dir,full.names = T)
#do not compute XGBTOP, cuz it only has top features, and it is the same as XGB
all_importance_files <- all_importance_files[-which(grepl("XGBTOP",all_importance_files))]


for (i in 1:length(all_importance_files)){
  curr_file <- all_importance_files[i]
  curr_import_matrix <- read.csv(curr_file,stringsAsFactors = F,row.names = 1)
  model_name <- str_match(curr_file, "/MAKE_\\s*(.*?)\\s*_importance_")[,2]
  prediction_task <- "MAKE"
  
  #NOTE: for XGB importance, some of feature were droped when compute feature importance, we just manully add 0 to then
  if (grepl("wTrajectory",model_name) == T){
    data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
    model_data <-read.csv(paste0(data_dir, "clinical_model_make_wTrajectory_norm.csv"),stringsAsFactors = F)
    unique_features <- colnames(model_data)[-which(colnames(model_data) == "STUDY_PATIENT_ID")]
  }else{
    unique_features <- unique(curr_import_matrix[,"Feature"])
    length(unique_features)
  }
  
  score_df <- as.data.frame(matrix(NA, nrow = length(unique_features),ncol = 5))
  colnames(score_df) <- c("Feature","Importance","Method","Prediction","Model")
  score_df$Method <- str_match(curr_file, "matrix_\\s*(.*?)\\s*.csv")[,2]
  score_df$Prediction <- prediction_task
  score_df$Model <- model_name
  
  for (f in 1:length(unique_features)){ #for each feature, avg all scores over all folds and sample
    curr_feature <- unique_features[f]
    
    if (grepl("xgb",curr_file,ignore.case = T) == T) {
      score_col <-"Gain"
      curr_import_matrix <- norm_0to100_func(curr_import_matrix)
    }else{
      score_col <-"Importance_Scaled0_100"
      
    }
    
    if (curr_feature %in% curr_import_matrix[,"Feature"]){
      curr_score <- curr_import_matrix[which(curr_import_matrix[,"Feature"] == curr_feature),score_col]
      avg_score <- mean(curr_score,na.rm = T)
    }else{#  #NOTE: for XGB importance, some of feature were droped when compute feature importance, we just manully add 0 to then
      avg_score <- 0 
    }
    score_df$Feature[f] <- curr_feature
    score_df$Importance[f] <- avg_score
  }
  score_df$Importance <- round(score_df$Importance,2)
  sorted_score_df <- score_df[order(score_df$Importance,decreasing = T),]
  outname <- paste0(importance_dir, "/",'AVG_Importance_',unique(sorted_score_df$Prediction),"_",unique(sorted_score_df$Model),"_",unique(sorted_score_df$Method),".xlsx")
  write.xlsx(sorted_score_df,outname)
}
