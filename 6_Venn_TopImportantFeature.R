library(VennDiagram)
library(venn)
library(ggplot2)
library(ggrepel)
library(ggpolypath)
source("TAKI_Ultility.R")
get_important_features_and_cooordinates <-function(perf_dir, select_f_dir, num_top_f){
  #Get all important features matrix
  method_names <- c("Logreg","RF","SVM","XGB")
  imporatnce_list <- list()
  for (i in 1:length(method_names)){
    curr_file <- paste0(perf_dir,"Importance_AVG_",method_names[i],".csv")
    curr_tb <- read.csv(curr_file,stringsAsFactors = F)
    curr_tb[,2] <- round(curr_tb[,2],2)
    curr_tb <- change_listoffeature_name_intable2(curr_tb)
    colnames(curr_tb) <- paste0(method_names[i],"_",colnames(curr_tb))
    imporatnce_list[[i]] <- curr_tb
  }
  all_importance_df <- do.call(cbind,imporatnce_list)
  top15_importance_df <- all_importance_df[1:num_top_f,c(1,3,5,7)]
  
  #Get selected Final features
  selected_final_features_df <- read.csv(paste0(select_f_dir,"Importance_AVG_RF.csv"),stringsAsFactors = F)
  selected_final_features_df <- change_listoffeature_name_intable2(selected_final_features_df)
  
  #Get important features for each methods
  LR_fs <- top15_importance_df[,"Logreg_Feature"]
  RF_fs     <- top15_importance_df[,"RF_Feature"]
  SVM_fs    <- top15_importance_df[,"SVM_Feature"]
  XGB_fs    <- top15_importance_df[,"XGB_Feature"]
  Final_Selected_fs    <- selected_final_features_df[,"Feature"]
  
  #Create a coordinates for each unique features
  set.seed(100)
  unique_important_features_df <- data.frame(unique(unlist(top15_importance_df)))
  colnames(unique_important_features_df) <- "Features"
  unique_important_features_df$X <- sample(1:100,nrow(unique_important_features_df))
  unique_important_features_df$Y <- sample(1:100,nrow(unique_important_features_df))
  #Label each feature indicates which method identify each as imporatant
  unique_important_features_df[which(unique_important_features_df$Features %in% LR_fs),"Label_LR"] <- "LR"
  unique_important_features_df[which(unique_important_features_df$Features %in% RF_fs),"Label_RF"] <- "RF"
  unique_important_features_df[which(unique_important_features_df$Features %in% SVM_fs),"Label_SVM"] <- "SVM"
  unique_important_features_df[which(unique_important_features_df$Features %in% XGB_fs),"Label_XGB"] <- "XGB"
  unique_important_features_df[which(unique_important_features_df$Features %in% Final_Selected_fs),"Label_Selected"] <- "Selected"
  unique_important_features_df$Final_Group <- NA
  for (i in 1:nrow(unique_important_features_df)){
    curr_labels <- unique_important_features_df[i,c("Label_LR","Label_RF","Label_SVM","Label_XGB","Label_Selected")]
    nas_indexes <- which(is.na(curr_labels)==T)
    if(length(nas_indexes) >0){
      curr_labels <- curr_labels[-nas_indexes]
    }
    #If selected for the final model, only label as selected
    final_label <- paste0(curr_labels,collapse = ", ")
    selected_ind <- which(grepl("Selected",final_label) == T)
    if (length(selected_ind) > 0){
      final_label <- "Selected"
    }
    unique_important_features_df[i,"Final_Group"] <-final_label
  }
  return(unique_important_features_df)
}
plot_topfeatures_func<-function(unique_important_features_df,title_name){
  p <- ggplot(unique_important_features_df, aes(x=X, y=Y,color = Final_Group)) +
    geom_point(size = 2) + 
    geom_text_repel(label= as.character(unique_important_features_df$Features),size = 7) + 
    labs(title = title_name) + 
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    theme(axis.text = element_text(size = 20),
          axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=20,face="bold")) + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) + 
    theme(legend.position="bottom",legend.title = element_blank(),legend.text=element_text(size=20)) +
    scale_colour_manual(breaks = c("LR, SVM",
                                   "LR, XGB",
                                   "LR",
                                   "RF, SVM, XGB",
                                   "RF, XGB",
                                   "SVM",
                                   "Selected"),
                        values = c("deepskyblue","#E69F00","darkblue","yellow3","#CC79A7","darkred","#009E73"))
  return(p)
}

plot_topfeatures_func_MAKE<-function(unique_important_features_df,title_name){
  p <- ggplot(unique_important_features_df, aes(x=X, y=Y,color = Final_Group)) +
    geom_point(size = 2) + 
    geom_text_repel(label= as.character(unique_important_features_df$Features),size = 7) + 
    labs(title = title_name) + 
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    theme(axis.text = element_text(size = 20),
          axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=20,face="bold")) + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) + 
    theme(legend.position="bottom",legend.title = element_blank(),legend.text=element_text(size=20)) +
    scale_colour_manual(breaks = c("LR, XGB",
                                   "LR",
                                   "RF, XGB",
                                   "SVM",
                                   "Selected",
                                   "LR, RF, SVM, XGB",
                                   "LR, RF",
                                   "RF, SVM",
                                   "RF",
                                   "XGB"),
                        values = c("#E69F00","darkblue","#CC79A7","darkred","#009E73",
                                   "darkorange1","yellow3","gold4","slateblue2","deepskyblue"))
  return(p)
}


plot_venn_func <- function(set_list){
  p <- venn(set_list, ilab=FALSE,snames = c("LR","Selected","RF","XGBoost","SVM") ,
            zcolor = c("darkgreen","darkred","darkblue","darkorange","yellow"),ggplot = T,
            opacity = 0.5,plotsize = 100, size = 0)
  return(p)
}

#change feature name
change_f_names_func <- function(in_data){
  in_data <- unique_important_f_df
  in_data[,"Features"] <- gsub("Vasopressor","Pressor/Inotrope",in_data[,"Features"])
  in_data[,"Features"] <- gsub("Sodium","Serum Sodium",in_data[,"Features"])
  
  return(in_data)
}


########################### Mortatlity  ########################### 
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/CV_performance/"
perf_dir <- paste0(proj_dir, "mortality/AllClinicalFeature/")
select_f_dir <- paste0(proj_dir, "mortality/SelectedClinicalFeature15Vars/")
outdir   <- paste0(proj_dir,"mortality/")


unique_important_f_df <- get_important_features_and_cooordinates(perf_dir,select_f_dir,15)
unique_important_f_df <- change_f_names_func(unique_important_f_df)
#Report unselected/selected status features
write.csv(unique_important_f_df,paste0(outdir,"Top15UnionFeatures_SelectionStatus.csv"))


mortality_plot <- plot_topfeatures_func(unique_important_f_df,"Hospital Mortality")
png(paste0(outdir,"Mortality_Important_feature_plot.png"),width = 3000,height = 3000,res = 120)
print(mortality_plot)
dev.off()

#Ven diagram
Log_reg_fs <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_LR"])==F)]
SVM_fs     <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_RF"])==F)]
RF_fs      <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_SVM"])==F)]
XGB_fs     <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_XGB"])==F)]
Selected_fs<- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_Selected"])==F)]

set_list <- list(Log_reg_fs,Selected_fs,RF_fs,XGB_fs,SVM_fs)
mortality_venn_p <- plot_venn_func(set_list)

png(paste0(outdir,"Mortality_Important_feature_Venn.png"),width = 1000,height = 500,res = 200)
print(mortality_venn_p)
dev.off()



########################### MAKE  ########################### 
perf_dir <- paste0(proj_dir, "make120_drop50/AllClinicalFeature/")
select_f_dir <- paste0(proj_dir, "make120_drop50/SelectedClinicalFeature14Vars/")
outdir   <- paste0(proj_dir,"make120_drop50/")


unique_important_f_df <- get_important_features_and_cooordinates(perf_dir,select_f_dir,15)
unique_important_f_df <- change_f_names_func(unique_important_f_df)

#Report unselected/selected status features
write.csv(unique_important_f_df,paste0(outdir,"Top15UnionFeatures_SelectionStatus.csv"))


MAKE_plot <- plot_topfeatures_func_MAKE(unique_important_f_df,"MAKE")
png(paste0(outdir,"MAKE_Important_feature_plot.png"),width = 3000,height = 3000,res = 300)
print(MAKE_plot)
dev.off()

#Ven diagram
Log_reg_fs <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_LR"])==F)]
SVM_fs     <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_RF"])==F)]
RF_fs      <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_SVM"])==F)]
XGB_fs     <- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_XGB"])==F)]
Selected_fs<- unique_important_f_df$Features[which(is.na(unique_important_f_df[,"Label_Selected"])==F)]

set_list <- list(Log_reg_fs,Selected_fs,RF_fs,XGB_fs,SVM_fs)
make_venn_p <- plot_venn_func(set_list)

png(paste0(outdir,"MAKE_Important_feature_Venn.png"),width = 500,height = 500,res = 120)
print(make_venn_p)
dev.off()




########################### Combine Make and mortality 
library(ggpubr)
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/Important_feature_Venn_Plots/"
scatter_plot <- ggarrange(mortality_plot,MAKE_plot, 
                #labels = c("A", "B"),
                ncol = 2, nrow = 1)
png(paste0(outdir,"Mortality_MAKE_scatter.png"),width = 5000,height = 1500,res = 250)
print(scatter_plot)
dev.off()


venn_plot <- ggarrange(mortality_venn_p,make_venn_p, 
                          labels = c("                           A.Hospital Mortality", 
                                     "                                  B.MAKE"),
                          ncol = 1, nrow = 2)

png(paste0(outdir,"Mortality_MAKE_Venn.png"),width = 2000,height = 2000,res = 180)
print(venn_plot)
dev.off()
