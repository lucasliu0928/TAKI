source("TAKI_Ultility.R")
compute_avg_pred_risk_and_risk_category2 <- function(cohort_name,outcome_name,perf_dir,method_name,featureset_folder){
  # perf_dir <- UK_mortality_dir
  # cohort_name <- "UK"
  # outcome_name <- "Mortality"
  # method_name <- "RF"

  #1. Load pred table
  pred_df <- read.csv(paste0(perf_dir, featureset_folder, "/Prediction_",method_name,".csv"),stringsAsFactors = F)
  
  #2.Compute avg pred risk
  avg_risk <- get_avg_pred_func(pred_df)

  risk_category <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  risk_count <- count_risk_category(avg_risk,risk_category)
  
  #Extract raio of actual postives
  risk_count$Proportion_ACUTAL_LABEL1     <-   as.numeric(lapply(strsplit(gsub("\\(|\\)","",risk_count$N_andPerc_AcutalLabel1),split = " "),'[[',2))/100
  risk_count$Risk_cateogry_upperbound     <-   c(risk_category,1.0)
  
  #order the levels
  risk_count$Risk_Category <- factor(risk_count$Risk_Category, levels = c("<=10%","(10%, 20%]", "(20%, 30%]", "(30%, 40%]", 
                                                                          "(40%, 50%]", "(50%, 60%]", "(60%, 70%]", "(70%, 80%]","(80%, 90%]",">90%"))
  
  return(risk_count)
}

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"


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

#1. UK
UK_riskCategory1     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model1)
UK_riskCategory2     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model2)
UK_riskCategory3     <- compute_avg_pred_risk_and_risk_category2("UK",outcome_name,UK_mortality_dir,method_name,model3)

p1 <- plot_calibration_manually(UK_riskCategory1,model1)
p2 <- plot_calibration_manually(UK_riskCategory2,model2)
p3 <- plot_calibration_manually(UK_riskCategory3,model3)

p1
p2
p3
plot_calibration_manually <- function(riskCategory,model_name){
  p <- ggplot(riskCategory, aes(x=Risk_cateogry_upperbound, y=Proportion_ACUTAL_LABEL1)) + 
    geom_point(shape=18, color="blue")+
    geom_smooth(method=glm, se=FALSE, linetype="dashed", color="darkred") +
    ylim(0,1) + 
    labs(title= model_name,x ="Predicted Probability", y = "Ratio of Positives") +
    scale_x_discrete(breaks=c("0.1",0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0),
                     labels=c("<=10%","(10%, 20%]", "(20%, 30%]", "(30%, 40%]", 
                              "(40%, 50%]", "(50%, 60%]", "(60%, 70%]", "(70%, 80%]","(80%, 90%]",">90%"))
  return(p)
}

#2.UTSW
UTSW_riskCategory1     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model1)
UTSW_riskCategory2     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model2)
UTSW_riskCategory3     <-  compute_avg_pred_risk_and_risk_category2("UTSW",outcome_name,UTSW_mortality_dir,method_name,model3)
