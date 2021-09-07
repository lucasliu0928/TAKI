source("TAKI_Ultility.R")
###Mortatlity 
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/CV_performance/"
perf_dir <- paste0(proj_dir, "mortality/AllClinicalFeature/")
outdir   <- paste0(proj_dir,"mortality/")


method_names <- c("LogReg","RF","SVM","XGB")
imporatnce_list <- list()
for (i in 1:length(method_names)){
  curr_file <- paste0(perf_dir,"Importance_AVG_",method_names[i],".csv")
  curr_tb <- read.csv(curr_file,stringsAsFactors = F)
  curr_tb[,2] <- round(curr_tb[,2],2)
  curr_tb <- change_listoffeature_name_intable2(curr_tb)
  imporatnce_list[[i]] <- curr_tb
}
all_importance_df <- do.call(cbind,imporatnce_list)
top15_importance_df <- all_importance_df[1:15,]
#write.csv(top15_importance_df,paste0(outdir,"Top15FeatureRanking_4Methods.csv"),row.names = F)


###MAKE 
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/CV_performance/"
perf_dir <- paste0(proj_dir, "make120_drop50/AllClinicalFeature/")
outdir   <- paste0(proj_dir,"make120_drop50/")

method_names <- c("LogReg","RF","SVM","XGB")
imporatnce_list <- list()
for (i in 1:length(method_names)){
  curr_file <- paste0(perf_dir,"Importance_AVG_",method_names[i],".csv")
  curr_tb <- read.csv(curr_file,stringsAsFactors = F)
  curr_tb[,2] <- round(curr_tb[,2],2)
  curr_tb <- change_listoffeature_name_intable2(curr_tb)
  imporatnce_list[[i]] <- curr_tb
}
all_importance_df <- do.call(cbind,imporatnce_list)
top15_importance_df <- all_importance_df[1:15,]
#write.csv(top15_importance_df,paste0(outdir,"Top15FeatureRanking_4Methods.csv"),row.names = F)
