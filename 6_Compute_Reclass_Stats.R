get_avg_pred_func <- function(analysis_df){
  unique_Ids <- unique(analysis_df[,"ID"])
  avg_pred_tb <- as.data.frame(matrix(NA, nrow = length(unique_Ids),ncol = 4))
  colnames(avg_pred_tb) <- c("ID" ,"pred_prob" , "pred_class","Label")
  for (i in 1:length(unique_Ids)){
    curr_id <- unique_Ids[i]
    curr_df <- analysis_df[which(analysis_df[,"ID"] == curr_id),]
    curr_avg_pred_prob <- mean(curr_df[,"pred_prob"])
    if (curr_avg_pred_prob >= 0.5){
      curr_avg_pred_class <- 1
    }else{
      curr_avg_pred_class <- 0
    }
    avg_pred_tb[i,"ID"] <- curr_id
    avg_pred_tb[i,"pred_prob"] <- curr_avg_pred_prob
    avg_pred_tb[i,"pred_class"] <- curr_avg_pred_class
    avg_pred_tb[i,"Label"] <- unique(curr_df[,"Label"])
  }
  return(avg_pred_tb)
}


## compute reclassification measures
compute_IDI_NRI_func <-function(perf_dir,b_model_file,comprison_model_file){
  baseline_df <- read.csv(paste0(perf_dir,"/",b_model_file),stringsAsFactors = F)
  comp_df <- read.csv(paste0(perf_dir,"/",comprison_model_file),stringsAsFactors = F)
  
  #get averge pred prob for each pt from  sampling
  avg_baseline_df <- get_avg_pred_func(baseline_df)
  avg_comp_df <- get_avg_pred_func(comp_df)
  
  #match Id order
  avg_baseline_df <- avg_baseline_df[match(avg_baseline_df$ID,avg_comp_df$ID),]

  #Combine comparison models
  model_comp_df <- cbind.data.frame(avg_baseline_df[,"Label"],
                                    avg_baseline_df[,"pred_prob"],
                                    avg_comp_df[,"pred_prob"])
  colnames(model_comp_df) <- c("Label","pred_prob_bl","pred_prob1")
  
  predRisk1 <- model_comp_df$pred_prob_bl
  predRisk2 <- model_comp_df$pred_prob1
  cutoff <- c(0,.10,.30,1)
  
  #NRI = P(up|event)−P(down|event) + P(down|nonevent)−P(up|nonevent)
  res <- reclassification(data=model_comp_df, cOutcome=1, predrisk1=predRisk1, predrisk2=predRisk2, cutoff)
  
  return(res)
}


####################################################
#compare models with IDI, NRI 
####################################################
#1. For mortality
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/mortality/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/motality_final_perf2/"

baseline_model_file  <- "Mortality_SOFA_SUM_pred_table_SVM_UKY.csv"
baseline_model_file2  <- "Mortality_APACHE_SUM_pred_table_SVM_UKY.csv"
comprison_model_file1 <- "Mortality_selected_features_pred_table_SVM_UKY.csv" 

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file2,comprison_model_file1)

#'@Note:
#Events, No (%) : # of recalssifed events / (total # of original events)  (4+170) / (1618)
#nonEvents, No (%) : # of recalssifed non-events / (total # of original nonevents) (208+2377)/5233

#UTSW
baseline_model_file  <- "Mortality_SOFA_SUM_pred_table_SVM_UTSW.csv"
baseline_model_file2 <- "Mortality_APACHE_SUM_pred_table_SVM_UTSW.csv" 
comprison_model_file1 <- "Mortality_selected_features_pred_table_SVM_UTSW.csv" 

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file2,comprison_model_file1)

#2. For MAKE
perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/make/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/make_final_perf2/"

baseline_model_file  <- "MAKE_max_kdigo_d03_pred_table_SVM_UKY.csv"
comprison_model_file1 <- "MAKE_selected_features_norm_option1_withPH.csv.csv_pred_table_SVM_UKY.csv" 
comprison_model_file2 <- "MAKE_selected_features_norm_option2_withMechHemoSup.csv_pred_table_SVM_UKY.csv" 
comprison_model_file3 <- "MAKE_selected_features_norm_option3_withPH_And_MechHemoSup.csv_pred_table_SVM_UKY.csv" 

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file3)


#UTSW
baseline_model_file  <- "MAKE_max_kdigo_d03_pred_table_SVM_UTSW.csv"
comprison_model_file1 <- "MAKE_selected_features_norm_option1_withPH.csv.csv_pred_table_SVM_UTSW.csv" 
comprison_model_file2 <- "MAKE_selected_features_norm_option2_withMechHemoSup.csv_pred_table_SVM_UTSW.csv" 
comprison_model_file3 <- "MAKE_selected_features_norm_option3_withPH_And_MechHemoSup.csv_pred_table_SVM_UTSW.csv" 

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2)
reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file3)
