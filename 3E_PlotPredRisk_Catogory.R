source("TAKI_Ultility.R")
library(ggplot2)

process_catogory_data_func <- function(predicted_catogory_df){
  #predicted_catogory_df <- UK_mortality_catogory_df

  predicted_catogory_df$NUM_PTs_PredINCatogory <- NA
  predicted_catogory_df$NUM_PTs_OUTCOME1 <- NA
  predicted_catogory_df$PERC_PTs_OUTCOME1 <- NA
  
  for (i in 1:nrow(predicted_catogory_df)){
    predicted_catogory_df[i,"NUM_PTs_PredINCatogory"] <- as.numeric(unlist(strsplit(predicted_catogory_df[i,"N_andPerc_PredictedInCategory"],split = " "))[1])
    predicted_catogory_df[i,"NUM_PTs_OUTCOME1"] <- as.numeric(unlist(strsplit(predicted_catogory_df[i,"N_andPerc_AcutalLabel1"],split = " "))[1])
    
    if (predicted_catogory_df[i,"NUM_PTs_OUTCOME1"] == 0 | predicted_catogory_df[i,"NUM_PTs_PredINCatogory"] == 0){
      predicted_catogory_df[i,"PERC_PTs_OUTCOME1"] <- 0
    }else{
       predicted_catogory_df[i,"PERC_PTs_OUTCOME1"] <- round((predicted_catogory_df[i,"NUM_PTs_OUTCOME1"]/predicted_catogory_df[i,"NUM_PTs_PredINCatogory"])*100,2)
    }
  }
  
  #order the levels
  predicted_catogory_df$Risk_Category <- factor(predicted_catogory_df$Risk_Category, levels = c("<=10%","(10%, 20%]", "(20%, 30%]", "(30%, 40%]", "(40%, 50%]", "(50%, 60%]", "(60%, 70%]", ">70%"))
  
  return(predicted_catogory_df)
}


plot_risk_categoryAndIncidence_func <- function(plot_data,y_left_max,outcome_name,scale_2nd_y,leg_position){
  
  p<-ggplot(data=plot_data, aes(x=Risk_Category, y=NUM_PTs_PredINCatogory)) +
    geom_bar(aes(fill = Cohort_NAME), position=position_dodge(),stat="identity")+
    scale_fill_manual(breaks = c("UKY", "UTSW"),values=c("dodgerblue4", "firebrick4")) + 
    geom_point(aes(x = Risk_Category, y = scale_2nd_y*PERC_PTs_OUTCOME1,shape = Cohort_NAME,color = Cohort_NAME), size = 5) + #scale the second y by 12
    scale_shape_manual(breaks = c("UKY", "UTSW"), values=c(15, 19)) + 
    geom_line(aes(x = Risk_Category, y = scale_2nd_y*PERC_PTs_OUTCOME1, group = Cohort_NAME,color = Cohort_NAME), size = 1) + #scale the second y by 12
    scale_y_continuous(limits = c(0, y_left_max), breaks = seq(0, y_left_max, by = 200), 
                       sec.axis = sec_axis(~./scale_2nd_y, name = "Outcome Incidence (%)")) +     # adds the secondary Y-axis
    scale_color_manual(breaks = c("UKY", "UTSW"),values=c("dodgerblue3", "firebrick3")) + 
    labs(x= "Predicted Risk Category", y = "Number of Patients")+ 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 28),
          axis.title.x = element_text(margin =ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y.left =  element_text(margin =ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
          axis.title.y.right =  element_text(margin =ggplot2::margin(t = 0, r = 0, b = 0, l = 15)),
          legend.position = leg_position,
          legend.title = element_blank(),
          legend.text =  element_text(size = 30))
  return(p)
}

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"


################################################################################################## 
######                         Mortality                                            ############## 
################################################################################################## 
#1. UKY
UK_mortality_dir <- paste0(proj_dir,"CV_performance/mortality/UK_SelectedClinicalFeature15Vars_Mortality_Risk_Catogory5_RF.csv")
UK_mortality_catogory_df <- read.csv(UK_mortality_dir,stringsAsFactors = F)
UK_mortality_catogory_df$Cohort_NAME <- "UKY"

#2.UTSW
UTSW_mortality_dir <- paste0(proj_dir,"ExternalV_performance/mortality/UTSW_SelectedClinicalFeature15Vars_Mortality_Risk_Catogory5_RF.csv")
UTSW_mortality_catogory_df <- read.csv(UTSW_mortality_dir,stringsAsFactors = F)
UTSW_mortality_catogory_df$Cohort_NAME <- "UTSW"


#3.Cobine two and get plot data
comb_mortality_category_df <- rbind(UK_mortality_catogory_df,UTSW_mortality_catogory_df)
plot_mortality_data <- process_catogory_data_func(comb_mortality_category_df)

#plot
outcome_name <- "Mortality"
max_n_pts_inCategory <- 1250
scale_2nd_y <- 20
legend_pos <- "top"
p1 <- plot_risk_categoryAndIncidence_func(plot_mortality_data,max_n_pts_inCategory,outcome_name,scale_2nd_y,legend_pos)
p1 <- p1 +  theme(plot.margin=unit(c(0,0.2,1.2,0.2),"cm"))
tiff(paste0(proj_dir,"RiskCategory_Plot/",outcome_name,".tiff"), units="in", width=10, height=7, res=300)
print(p1)
dev.off()


################################################################################################## 
######                         MAKE                                            ############## 
################################################################################################## 
#1. UKY
UK_dir <- paste0(proj_dir,"CV_performance/make120_drop50/UK_SelectedClinicalFeature14Vars_MAKE_Risk_Catogory5_RF.csv")
UK_catogory_df <- read.csv(UK_dir,stringsAsFactors = F)
UK_catogory_df$Cohort_NAME <- "UKY"

#2.UTSW
UTSW_dir <- paste0(proj_dir,"ExternalV_performance/make120_drop50/UTSW_SelectedClinicalFeature14Vars_MAKE_Risk_Catogory5_RF.csv")
UTSW_catogory_df <- read.csv(UTSW_dir,stringsAsFactors = F)
UTSW_catogory_df$Cohort_NAME <- "UTSW"


#3.Cobine two and get plot data
comb_category_df <- rbind(UK_catogory_df,UTSW_catogory_df)
plot_data <- process_catogory_data_func(comb_category_df)

#plot
outcome_name <- "MAKE"
max_n_pts_inCategory <- 1250
scale_2nd_y <- 15
legend_pos <- "none"
p2 <- plot_risk_categoryAndIncidence_func(plot_data,max_n_pts_inCategory,outcome_name,scale_2nd_y,legend_pos)
p2 <- p2 +  theme(plot.margin=unit(c(1.2,0.2,0,0.2),"cm"))

tiff(paste0(proj_dir,"RiskCategory_Plot/",outcome_name,".tiff"), units="in", width=14, height=10, res=300)
print(p2)
dev.off()

################################################################################################## 
#Combine two plot
################################################################################################## 
library(ggpubr)
p3 <- ggarrange(p1, p2,
                labels = c("A.Hospital Mortality", "B.MAKE"),
                hjust = c(-0.04,-0.08),
                font.label = list(size = 30, color = "black", face = "bold", family = NULL, align = "hv"),
                ncol = 1, nrow = 2)
tiff(paste0(proj_dir,"RiskCategory_Plot/","Mortality_And_MAKE",".tiff"), units="in", width=20, height=16, res=300)
print(p3)
dev.off()

################################################################################################## 
######                         MAKE For survivors                                           ############## 
################################################################################################## 
#1. UKY
UK_dir <- paste0(proj_dir,"CV_performance/Surviors_make120_drop50/UK_SelectedClinicalFeature14Vars_MAKE_Risk_Catogory5_RF.csv")
UK_catogory_df <- read.csv(UK_dir,stringsAsFactors = F)
UK_catogory_df$Cohort_NAME <- "UKY"

#2.UTSW
UTSW_dir <- paste0(proj_dir,"ExternalV_performance/Surviors_make120_drop50/UTSW_SelectedClinicalFeature14Vars_MAKE_Risk_Catogory5_RF.csv")
UTSW_catogory_df <- read.csv(UTSW_dir,stringsAsFactors = F)
UTSW_catogory_df$Cohort_NAME <- "UTSW"


#3.Cobine two and get plot data
comb_category_df <- rbind(UK_catogory_df,UTSW_catogory_df)
plot_data <- process_catogory_data_func(comb_category_df)

#plot
outcome_name <- "MAKE (Survivors)"
max_n_pts_inCategory <- 1250
scale_2nd_y <- 20
legend_pos <- "top"
p4 <- plot_risk_categoryAndIncidence_func(plot_data,max_n_pts_inCategory,outcome_name,scale_2nd_y,legend_pos)
p4

tiff(paste0(proj_dir,"RiskCategory_Plot/",outcome_name,".tiff"), units="in", width=14, height=10, res=300)
print(p4)
dev.off()



################################################################################################## 
#'@ADD010521                         Mortality for SOFA                                      ############## 
################################################################################################## 
#1. UKY
UK_mortality_dir <- paste0(proj_dir,"CV_performance/mortality/UK_SOFA_Mortality_Risk_Catogory4_RF.csv")
UK_mortality_catogory_df <- read.csv(UK_mortality_dir,stringsAsFactors = F)
UK_mortality_catogory_df$Cohort_NAME <- "UKY"

#2.UTSW
UTSW_mortality_dir <- paste0(proj_dir,"ExternalV_performance/mortality/UTSW_SOFA_Mortality_Risk_Catogory4_RF.csv")
UTSW_mortality_catogory_df <- read.csv(UTSW_mortality_dir,stringsAsFactors = F)
UTSW_mortality_catogory_df$Cohort_NAME <- "UTSW"


#3.Cobine two and get plot data
comb_mortality_category_df <- rbind(UK_mortality_catogory_df,UTSW_mortality_catogory_df)
plot_mortality_data <- process_catogory_data_func(comb_mortality_category_df)

#plot
outcome_name <- "Mortality"
max_n_pts_inCategory <- 1250
scale_2nd_y <- 20
legend_pos <- "top"
p1 <- plot_risk_categoryAndIncidence_func(plot_mortality_data,max_n_pts_inCategory,outcome_name,scale_2nd_y,legend_pos)
p1 <- p1 +  theme(plot.margin=unit(c(0,0.2,1.2,0.2),"cm"))
tiff(paste0(proj_dir,"RiskCategory_Plot/",outcome_name,".tiff"), units="in", width=10, height=7, res=300)
print(p1)
dev.off()

   