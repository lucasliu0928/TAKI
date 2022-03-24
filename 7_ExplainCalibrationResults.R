source("TAKI_Ultility.R")
compute_avg_pred_risk_and_risk_category2 <- function(cohort_name,outcome_name,perf_dir,method_name,featureset_folder,risk_category_list,risk_category_names){
  # perf_dir <- UK_mortality_dir
  # cohort_name <- "UK"
  # outcome_name <- "Mortality"
  # method_name <- "RF"
  # featureset_folder <- model1

  #1. Load pred table
  pred_df <- read.csv(paste0(perf_dir, featureset_folder, "/Prediction_",method_name,".csv"),stringsAsFactors = F)
  
  #2.Compute avg pred risk
  avg_risk <- get_avg_pred_func(pred_df)

  risk_count <- count_risk_category(avg_risk,risk_category_list)
  risk_count$Risk_cateogry_upperbound     <-   c(risk_category_list,1.0)
  
  #Extract raio of actual postives
  risk_count$Proportion_ACUTAL_LABEL1     <-   as.numeric(lapply(strsplit(gsub("\\(|\\)","",risk_count$N_andPerc_AcutalLabel1),split = " "),'[[',2))/100
  
  #order the levels
  #risk_count$Risk_Category <- factor(risk_count$Risk_Category, levels = risk_category_names )
  

  return(risk_count)
}

plot_calibration_manually <- function(riskCategory,model_name,risk_category_names){
  p <- ggplot(riskCategory, aes(x=Risk_cateogry_upperbound, y=Proportion_ACUTAL_LABEL1)) + 
    geom_point(color="blue",size = 8)+
    #geom_line(color="blue")+
    #geom_smooth(method=glm, se=FALSE, linetype="dashed", color="darkred") +
    geom_smooth(method=glm, method.args = list(family = "binomial"), 
                se=FALSE, linetype="dashed", color="darkred",size = 5) +
    ylim(0,1) + 
    labs(title= model_name,x ="Predicted Probability", y = "Ratio of Positives") +
    scale_x_continuous(breaks=riskCategory$Risk_cateogry_upperbound,
                       labels=risk_category_names) +
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(color="black", size=30,face = "bold"),
          axis.title = element_text(color="black", size=30,face = "bold"),
          axis.text.x  = element_text(color="black", size=25),
          axis.text.y  = element_text(color="black", size=25))
  return(p)
}


proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/Calibration_Explanation010522/"

################################################################################################## 
######                         Mortality                                            ############## 
################################################################################################## 
outcome_name <- "Mortality"
method_name <- "RF"
UK_mortality_dir <- paste0(proj_dir,"CV_performance/mortality/")
UTSW_mortality_dir <- paste0(proj_dir,"ExternalV_performance/mortality/")
model1 <- "SelectedClinicalFeature15Vars"
model2 <- "SOFA"
model3 <- "APACHE"

risk_category_list <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# risk_category_names <- c("<=10%","(10%, 20%]", "(20%, 30%]", "(30%, 40%]",
#                         "(40%, 50%]", "(50%, 60%]", "(60%, 70%]", "(70%, 80%]",
#                         "(80%, 90%]",">90%")
risk_category_names <- c("<=10%","10%-20%", "20%-30%", "30%-40%",
                         "40%-50%", "50%-60%", "60%-70%", "70%-80%",
                         "80%-90%",">90%")

#1. UK
UK_riskCategory1     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model1,risk_category_list,risk_category_names)
UK_riskCategory2     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model2,risk_category_list,risk_category_names)
UK_riskCategory3     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model3,risk_category_list,risk_category_names)
#2.UTSW
UTSW_riskCategory1     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model1,risk_category_list,risk_category_names)
UTSW_riskCategory2     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model2,risk_category_list,risk_category_names)
UTSW_riskCategory3     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model3,risk_category_list,risk_category_names)


p1 <- plot_calibration_manually(UK_riskCategory1,"Clinical Model",risk_category_names)
p2 <- plot_calibration_manually(UK_riskCategory2,model2,risk_category_names)
p3 <- plot_calibration_manually(UK_riskCategory3,paste0(model3," II "),risk_category_names)
p4 <- plot_calibration_manually(UTSW_riskCategory1,"Clinical Model",risk_category_names)
p5 <- plot_calibration_manually(UTSW_riskCategory2,model2,risk_category_names)
p6 <- plot_calibration_manually(UTSW_riskCategory3,paste0(model3," II "),risk_category_names)


png(paste0(outdir,"Mortality_UK_",model1,".png"),res = 105,width = 1800,height = 800)
print(p1)
dev.off()
png(paste0(outdir,"Mortality_UK_",model2,".png"),res = 105,width = 1800,height = 800)
print(p2)
dev.off()
png(paste0(outdir,"Mortality_UK_",model3,".png"),res = 105,width = 1800,height = 800)
print(p3)
dev.off()

png(paste0(outdir,"Mortality_UTSW_",model1,".png"),res = 105,width = 1800,height = 800)
print(p4)
dev.off()
png(paste0(outdir,"Mortality_UTSW_",model2,".png"),res = 105,width = 1800,height = 800)
print(p5)
dev.off()
png(paste0(outdir,"Mortality_UTSW_",model3,".png"),res = 105,width = 1800,height = 800)
print(p6)
dev.off()

################################################################################################## 
############## MAKE ############## 
################################################################################################## 
outcome_name <- "MAKE"
method_name <- "RF"
UK_MAKE_dir <- paste0(proj_dir,"CV_performance/make120_drop50/")
UTSW_MAKE_dir <- paste0(proj_dir,"ExternalV_performance/make120_drop50/")
model1 <- "SelectedClinicalFeature14Vars"
model2 <- "KDIGO"
# risk_category_list <- c(0.1,0.3,0.6,0.9)
# risk_category_names <- c("<=10%","(10%, 30%]", "(30%, 60%]", "(60%, 90%]", ">90%")

risk_category_list <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# risk_category_names <- c("<=10%","(10%, 20%]", "(20%, 30%]", "(30%, 40%]", 
#                          "(40%, 50%]", "(50%, 60%]", "(60%, 70%]", "(70%, 80%]","(80%, 90%]",">90%")
risk_category_names <- c("<=10%","10%-20%", "20%-30%", "30%-40%",
                         "40%-50%", "50%-60%", "60%-70%", "70%-80%",
                         "80%-90%",">90%")

#1. UK
UK_MAKEriskCategory1 <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_MAKE_dir,method_name,model1,risk_category_list,risk_category_names)
UK_MAKEriskCategory2 <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_MAKE_dir,method_name,model2,risk_category_list,risk_category_names)

#2.UTSW
UTSW_MAKEriskCategory1 <- compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_MAKE_dir,method_name,model1,risk_category_list,risk_category_names)
UTSW_MAKEriskCategory2 <- compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_MAKE_dir,method_name,model2,risk_category_list,risk_category_names)

p7 <- plot_calibration_manually(UK_MAKEriskCategory1,"Clinical Model",risk_category_names)
p8 <- plot_calibration_manually(UK_MAKEriskCategory2,model2,risk_category_names)
p9 <- plot_calibration_manually(UTSW_MAKEriskCategory1,"Clinical Model",risk_category_names)
p10 <- plot_calibration_manually(UTSW_MAKEriskCategory2,model2,risk_category_names)


png(paste0(outdir,"MAKE_UK_",model1,".png"),res = 105,width = 1800,height = 800)
print(p7)
dev.off()
png(paste0(outdir,"MAKE_UK_",model2,".png"),res = 105,width = 1800,height = 800)
print(p8)
dev.off()

png(paste0(outdir,"MAKE_UTSW_",model1,".png"),res = 105,width = 1800,height = 800)
print(p9)
dev.off()
png(paste0(outdir,"MAKE_UTSW_",model2,".png"),res = 100,width = 800,height = 500)
print(p10)
dev.off()
