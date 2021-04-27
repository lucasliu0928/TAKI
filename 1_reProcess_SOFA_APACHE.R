source("TAKI_Ultility.R")

################################################################################################
#1.#Seperate SOFA and APACHE  and Sum up conponents
################################################################################################
#User input
#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features/"
UK_data_dir <- paste0(data_dir,"uky/")
UTSW_data_dir <- paste0(data_dir,"utsw/")

#out dir
out_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/Updated_features/"
UK_outdir<-  paste0(out_dir,"uky/")
UTSW_outdir<-  paste0(out_dir,"utsw/")
################################################################################################

#I.UK
#1. Load feature data
UK_feature_df <- read.csv(paste0(UK_data_dir,"sofa_apache.csv"),stringsAsFactors = F)
#2. One score (Sum up all components)
UK_SOFA_SUM <- sum_score_func(UK_feature_df,"SOFA")
UK_APACHE_SUM <- sum_score_func(UK_feature_df,"APACHE")

write.csv(UK_SOFA_SUM,paste0(UK_outdir,"SOFA_SUM.csv"),row.names = F)
write.csv(UK_APACHE_SUM,paste0(UK_outdir,"APACHE_SUM.csv"),row.names = F)


#3.#Min_Max_Norm
UK_SOFA_SUM[,"SOFA_SUM"] <- min_max_func(UK_SOFA_SUM[,"SOFA_SUM"])
UK_APACHE_SUM[,"APACHE_SUM"] <- min_max_func(UK_APACHE_SUM[,"APACHE_SUM"])
write.csv(UK_SOFA_SUM,paste0(UK_outdir,"SOFA_SUM_norm.csv"),row.names = F)
write.csv(UK_APACHE_SUM,paste0(UK_outdir,"APACHE_SUM_norm.csv"),row.names = F)


#II.UTSW
#1. Load feature data
UTSW_feature_df <- read.csv(paste0(UTSW_data_dir,"sofa_apache.csv"),stringsAsFactors = F)


#2. One score (Sum up all components)
UTSW_SOFA_SUM <- sum_score_func(UTSW_feature_df,"SOFA")
UTSW_APACHE_SUM <- sum_score_func(UTSW_feature_df,"APACHE")

write.csv(UTSW_SOFA_SUM,paste0(UTSW_outdir,"SOFA_SUM.csv"),row.names = F)
write.csv(UTSW_APACHE_SUM,paste0(UTSW_outdir,"APACHE_SUM.csv"),row.names = F)


#2.#Min_Max_Norm
UTSW_SOFA_SUM[,"SOFA_SUM"] <- min_max_func(UTSW_SOFA_SUM[,"SOFA_SUM"])
UTSW_APACHE_SUM[,"APACHE_SUM"] <- min_max_func(UTSW_APACHE_SUM[,"APACHE_SUM"])
write.csv(UTSW_SOFA_SUM,paste0(UTSW_outdir,"SOFA_SUM_norm.csv"),row.names = F)
write.csv(UTSW_APACHE_SUM,paste0(UTSW_outdir,"APACHE_SUM_norm.csv"),row.names = F)

