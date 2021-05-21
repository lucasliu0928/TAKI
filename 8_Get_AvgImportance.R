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


plot_func <- function(importance_matrix){
  plot <-ggplot(importance_matrix,
                ggplot2::aes(x = factor(Feature, levels = rev(Feature)), y = Importance, width = 0.5),
                environment = environment()) +
    geom_bar(aes(fill = "Darkred"), stat = "identity", position = "identity") +
    coord_flip() +
    xlab("Features") +
    ggtitle("Feature importance") +
    theme(plot.title = element_text(lineheight = .9, face = "bold"),
          panel.grid.major.y = element_blank()) +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "none")
  return(plot)
}


####Mortality
importance_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/"
importance_dir1 <- paste0(importance_dir,"Prediction_results0512/mortality_importance")
all_importance_files <- list.files(importance_dir1,full.names = T)


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
    }else if (grepl("Logreg",curr_file,ignore.case = T) == T){
      score_col <-"Beta_Coef"
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
  sorted_score_df <- score_df[order(abs(score_df$Importance),decreasing = T),]
  outname <- paste0(importance_dir1, "/",'AVG_Importance_',unique(sorted_score_df$Prediction),"_",unique(sorted_score_df$Model),"_",unique(sorted_score_df$Method),".xlsx")
  write.xlsx(sorted_score_df,outname)
}


####MAKE
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/"
importance_dir <- paste0(data_dir,"Prediction_results0512/make_importance")
all_importance_files <- list.files(importance_dir,full.names = T)
#do not compute XGBTOP, cuz it only has top features, and it is the same as XGB
#all_importance_files <- all_importance_files[-which(grepl("XGBTOP",all_importance_files))]


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
    }else if (grepl("Logreg",curr_file,ignore.case = T) == T){
      score_col <-"Beta_Coef"
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
  sorted_score_df <- score_df[order(abs(score_df$Importance),decreasing = T),]
  outname <- paste0(importance_dir, "/",'AVG_Importance_',unique(sorted_score_df$Prediction),"_",unique(sorted_score_df$Model),"_",unique(sorted_score_df$Method),".xlsx")
  write.xlsx(sorted_score_df,outname)
}



#Plot importance matrix
library(ggplot2)
importance_matrix_mortality <- read.xlsx(paste0(importance_dir1,"/AVG_Importance_mortality_Mortality_selected_features_SVM.xlsx"),sheet = 1)
plot_func(importance_matrix_mortality)

importance_matrix_MAKE_PH <- read.xlsx(paste0(importance_dir,"/AVG_Importance_MAKE_MAKE_selected_features_norm_option1_withPH.csv.csv_SVM.xlsx"),sheet = 1)
plot_func(importance_matrix_MAKE_PH)

importance_matrix_MAKE_MechHemoSup <- read.xlsx(paste0(importance_dir,"/AVG_Importance_MAKE_MAKE_selected_features_norm_option2_withMechHemoSup.csv_SVM.xlsx"),sheet = 1)
plot_func(importance_matrix_MAKE_MechHemoSup)

importance_matrix_MAKE_PH_And_MechHemoSup <- read.xlsx(paste0(importance_dir,"/AVG_Importance_MAKE_MAKE_selected_features_norm_option3_withPH_And_MechHemoSup.csv_SVM.xlsx"),sheet = 1)
plot_func(importance_matrix_MAKE_PH_And_MechHemoSup)
