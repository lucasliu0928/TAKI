library(ggplot2)
library("extrafont")
library(extrafontdb)
font_import()
fonts()
loadfonts(device = "postscript") ## for postscript devie()

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
out_dir <- paste0(proj_dir,"All_Submission_Figures/")

############################################################ 
#Submission Figure 2 (Risk Category plot Mortality and MAKE)
############################################################ 
#ebed fonts
in_dir <- paste0(proj_dir,"RiskCategory_Plot/","Mortality_And_MAKE.eps")
embed_fonts(in_dir, 
            outfile = paste0(out_dir,"Figure2",".eps"),
            options = "-dEPSCrop")

############################################################ 
#Submission Figure 3 (SHAP Mortality)
############################################################ 
#ebed fonts
in_dir <- paste0(proj_dir,"Shap0907/mortality/","RF_15vars_UKandUTSW_Mortality.eps")
embed_fonts(in_dir, 
            outfile = paste0(out_dir,"Figure3",".eps"),
            options = "-dEPSCrop")

############################################################ 
#Submission Figure 4 (SHAP MAKE)
############################################################ 
#ebed fonts
in_dir <- paste0(proj_dir,"Shap0907/make120drop50/","RF_14vars_UKandUTSW_MAKE50.eps")
embed_fonts(in_dir, 
            outfile = paste0(out_dir,"Figure4",".eps"),
            options = "-dEPSCrop")
