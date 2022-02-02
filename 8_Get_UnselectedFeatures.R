source("TAKI_Ultility.R")

load_importance_df <- function(data_dir,filename){
  Important_df <- read.csv(paste0(data_dir,filename),stringsAsFactors = F)
  #Get absolute score
  Important_df$ABS_SCORE <- abs(Important_df[,"AVG_Importance"])
  #Order by abs socre
  Important_df <- Important_df[order(Important_df$ABS_SCORE,decreasing = T),]
  #round to 2 digits
  Important_df[,"AVG_Importance"] <- round(Important_df[,"AVG_Importance"],2)
  
  return(Important_df)
}

proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"

####################################################################################
# Mortality Important Features
####################################################################################
data_dir <- paste0(proj_dir, "CV_performance/mortality/AllClinicalFeature/")



Important_df_LR  <-  load_importance_df(data_dir,"Importance_AVG_LogReg.csv")
Important_df_RF  <- load_importance_df(data_dir,"Importance_AVG_RF.csv")
Important_df_SVM <- read.csv(paste0(data_dir,"Importance_AVG_SVM.csv"),stringsAsFactors = F)
Important_df_XGB <- read.csv(paste0(data_dir,"Importance_AVG_XGB.csv"),stringsAsFactors = F)

