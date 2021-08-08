library(lubridate)
source("/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Code/TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/xilong_extracted/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #2233

#3.Load raw data
raw_weight_df <- read.xlsx(paste0(raw_dir,"Raw D0 D3 weight data for Lucas 08062021.xlsx"),sheet = 1)

##########################################################################################
#Load UK raw CLINICAL_OTHERS
#Features to extract :  1. inital weight
#                          If D0 availble, use D0, 
#                          If D0 not avaiable, use D1 -> D2 -> D3
#                       2.Exclude outliers <30 or >200)
##########################################################################################
#Get Features
initial_wt_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 2))
colnames(initial_wt_df) <- c("STUDY_PATIENT_ID","INITIAL_WEIGHT_KG")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  curr_weight_df <- raw_weight_df[which(raw_weight_df$PATIENT_NUM == curr_id),]
  
  #sort by day
  curr_weight_df <- curr_weight_df[order(curr_weight_df$DAY_NO),]
 
  if (nrow(curr_weight_df) > 0){
    init_weight <- curr_weight_df[1,"D_AVG_VAL"]
  }else{
    init_weight <- NA
  }
  
  initial_wt_df[i,"STUDY_PATIENT_ID"] <- curr_id
  initial_wt_df[i,"INITIAL_WEIGHT_KG"] <- init_weight
    
}


#3.Remove outlier
#INITIAL_WEIGHT_KG
initial_wt_df_OutlierExcluded <- remove_values_byValue(initial_wt_df,"INITIAL_WEIGHT_KG",30,"Less Than")
initial_wt_df_OutlierExcluded <- remove_values_byValue(initial_wt_df_OutlierExcluded,"INITIAL_WEIGHT_KG",200,"Greater Than")


write.csv(initial_wt_df_OutlierExcluded,paste0(outdir,"INITIAL_WEIGHT_NOTImputed.csv"),row.names = F)


