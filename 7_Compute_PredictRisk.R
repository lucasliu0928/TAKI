get_avg_riskandlabel_func <- function(pred_df){
  unique_Ids <- unique(pred_df[,"ID"])
  avg_risk_df <- as.data.frame(matrix(NA, nrow = length(unique_Ids), ncol = 3))
  colnames(avg_risk_df) <- c("ID", "AVG_Risk","Label")
  for (i in 1:length(unique_Ids)){
    curr_id <- unique_Ids[i]
    avg_risk_df[i,"ID"] <- curr_id
    curr_pred_df <- pred_df[which(pred_df[,"ID"] == curr_id),]
    avg_risk_df[i,"AVG_Risk"] <- mean(curr_pred_df[,"pred_prob"])
    avg_risk_df[i,"Label"] <- unique(curr_pred_df[,"Label"])
    
  }
  return(avg_risk_df)
}

count_n_category <- function(risk_df){
  #risk_df <- risk_mortality_UK
  risk_category <- c(0.1,0.5)
  Risk_Count_Table <- as.data.frame(matrix(NA, nrow = length(risk_category) + 1, ncol = 3))
  colnames(Risk_Count_Table) <- c("Category","N_andPerc_Predict_Risk","N_andPerc_Label1")
  
  for (i in 1: (length(risk_category)+ 1)){
    if (i == 1){
      cond <- risk_df[,"AVG_Risk"] < risk_category[i]
      
    }else if (i == length(risk_category) + 1){
      cond <- risk_df[,"AVG_Risk"] >= risk_category[i-1]
      
    }else{
      cond <- (risk_df[,"AVG_Risk"] >= risk_category[i-1]) & (risk_df[,"AVG_Risk"] < risk_category[i])
      
    }
    curr_pred_df <- risk_df[cond,]
    min_risk <- round(min(curr_pred_df[,"AVG_Risk"]),2)*100
    max_risk <- round(max(curr_pred_df[,"AVG_Risk"]),2)*100
    Risk_Count_Table[i,"Category"] <- paste0(min_risk,"% - ", max_risk, "%")
    nPredRisk_in_category <- nrow(curr_pred_df)
    perc_PredRisk <- round((nPredRisk_in_category/nrow(risk_df)*100),2) # / total n of pts
    Risk_Count_Table[i,"N_andPerc_Predict_Risk"] <- paste0(nPredRisk_in_category," (", perc_PredRisk, ")")
    
    nLabel1_in_category <- length(which(curr_pred_df[,"Label"] == 1))
    perc_label1 <- round(nLabel1_in_category/nPredRisk_in_category*100,2) # / number of predict risk in catego
    Risk_Count_Table[i,"N_andPerc_Label1"] <- paste0(nLabel1_in_category," (", perc_label1, ")")
    
  }
  return(Risk_Count_Table)
}
################################################################################################## 
############## Mortality ############## 
################################################################################################## 
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0512/"
outdir <- paste0(proj_dir,"Risk_Table/")
#for each Id, averge the predick risk corss all down-sampling times
#UK
mortality_dir <- paste0(proj_dir,"mortality/Mortality_selected_features_pred_table_SVM_UKY.csv" )
pred_df <- read.csv(mortality_dir,stringsAsFactors = F)
risk_mortality_UK <- get_avg_riskandlabel_func(pred_df)
risk_mortality_UK_count <- count_n_category(risk_mortality_UK)
write.csv(risk_mortality_UK_count,paste0(outdir,"risk_mortality_UK_count.csv"))

#UTSW
mortality_dir <- paste0(proj_dir,"mortality/Mortality_selected_features_pred_table_SVM_UTSW.csv" )
pred_df <- read.csv(mortality_dir,stringsAsFactors = F)
risk_mortality_UTSW <- get_avg_riskandlabel_func(pred_df)
risk_mortality_UTSW_count <- count_n_category(risk_mortality_UTSW) #count n in each percentage category
write.csv(risk_mortality_UTSW_count,paste0(outdir,"risk_mortality_UTSW_count.csv"))



################################################################################################## 
############## MAKE ############## 
################################################################################################## 
#1.With PH
#UK
mortality_dir <- paste0(proj_dir,"make/MAKE_selected_features_norm_option1_withPH.csv.csv_pred_table_SVM_UKY.csv" )
pred_df <- read.csv(mortality_dir,stringsAsFactors = F)
risk_make_UK <- get_avg_riskandlabel_func(pred_df)
risk_make_UK_count <- count_n_category(risk_make_UK)
write.csv(risk_make_UK_count,paste0(outdir,"risk_make_UK_count.csv"))

#UTSW
mortality_dir <- paste0(proj_dir,"make/MAKE_selected_features_norm_option1_withPH.csv.csv_pred_table_SVM_UTSW.csv" )
pred_df <- read.csv(mortality_dir,stringsAsFactors = F)
risk_make_UTSW <- get_avg_riskandlabel_func(pred_df)
risk_make_UTSW_count <- count_n_category(risk_make_UTSW)
write.csv(risk_make_UTSW_count,paste0(outdir,"risk_make_UTSW_count.csv"))
