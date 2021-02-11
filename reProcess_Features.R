#Generate one score for SOFA and APACHE TAKE the sum of SOFA and APACHE
seperate_SOFA_APACHE_TOTAL_score_func <- function(analysis_df){
  #analysis_df <- UK_feature_df
  
  #This function sepearte sofa and apahce and compute one total socore for each
  SOFA_index <- which(grepl("SOFA",colnames(analysis_df)))
  APACHE_index <- which(grepl("APACHE",colnames(analysis_df)))
  ID_index <- which(colnames(analysis_df) == "STUDY_PATIENT_ID")
  
  SOFA_df <- analysis_df[,c(ID_index,SOFA_index)]
  APACHE_df <- analysis_df[,c(ID_index,APACHE_index)]

  SOFA_df$SOFA_SUM <- rowSums(SOFA_df[,-which(colnames(SOFA_df) == "STUDY_PATIENT_ID")])
  APACHE_df$APACHE_SUM <- rowSums(APACHE_df[,-which(colnames(APACHE_df) == "STUDY_PATIENT_ID")])
  
  SOFA_SUM_df <- as.data.frame(SOFA_df[,c("STUDY_PATIENT_ID","SOFA_SUM")])
  APACHE_SUM_df <- as.data.frame(APACHE_df[,c("STUDY_PATIENT_ID","APACHE_SUM")])

  return(list(SOFA_SUM_df,APACHE_SUM_df))
}


min_max_func <- function(feautre_col){
  normed_col <- (feautre_col-min(feautre_col,na.rm = T))/(max(feautre_col,na.rm = T)-min(feautre_col,na.rm = T))
  return(normed_col)
}

#User input
UK_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
UTSW_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/utsw/"


#I.UK
#1. Load feature data
UK_feature_df <- read.csv(paste0(UK_dir,"sofa_apache.csv"),stringsAsFactors = F)
#2.#Seperate SOFA and APACHE  and Sum up SOFA
res <- seperate_SOFA_APACHE_TOTAL_score_func(UK_feature_df)
UK_SOFA_SUM <- res[[1]]
UK_APACHE_SUM <- res[[2]]
write.csv(UK_SOFA_SUM,paste0(UK_dir,"SOFA_SUM.csv"),row.names = F)
write.csv(UK_APACHE_SUM,paste0(UK_dir,"APACHE_SUM.csv"),row.names = F)

#3.#Min_Max_Norm
UK_SOFA_SUM[,"SOFA_SUM"] <- min_max_func(UK_SOFA_SUM[,"SOFA_SUM"])
UK_APACHE_SUM[,"APACHE_SUM"] <- min_max_func(UK_APACHE_SUM[,"APACHE_SUM"])
write.csv(UK_SOFA_SUM,paste0(UK_dir,"SOFA_SUM_norm.csv"),row.names = F)
write.csv(UK_APACHE_SUM,paste0(UK_dir,"APACHE_SUM_norm.csv"),row.names = F)


#II.UTSW
#1. Load feature data
UTSW_feature_df <- read.csv(paste0(UTSW_dir,"sofa_apache.csv"),stringsAsFactors = F)

#1.#Seperate SOFA and APACHE  and Sum up SOFA
res <- seperate_SOFA_APACHE_TOTAL_score_func(UTSW_feature_df)
UTSW_SOFA <- res[[1]]
UTSW_APACHE <- res[[2]]
write.csv(UTSW_SOFA,paste0(UTSW_dir,"SOFA_SUM.csv"),row.names = F)
write.csv(UTSW_APACHE,paste0(UTSW_dir,"APACHE_SUM.csv"),row.names = F)


#2.#Min_Max_Norm
UTSW_SOFA[,"SOFA_SUM"] <- min_max_func(UTSW_SOFA[,"SOFA_SUM"])
UTSW_APACHE[,"APACHE_SUM"] <- min_max_func(UTSW_APACHE[,"APACHE_SUM"])
write.csv(UTSW_SOFA,paste0(UTSW_dir,"SOFA_SUM_norm.csv"),row.names = F)
write.csv(UTSW_APACHE,paste0(UTSW_dir,"APACHE_SUM_norm.csv"),row.names = F)

