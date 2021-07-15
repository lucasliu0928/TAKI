source("TAKI_Ultility.R")
library("shapr")


#this script use entire UK data plus down sampleing, and validation on utsw data


#Data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"


#feature file and outcome file names
feature_file <- c("All_Feature_imputed_normed.csv")
outcome_file <- "All_outcome.csv"

####################################################################################### 
######                           Mortality Prediction                      ############
#feature file: Selected features
#Outcome file: All_outcome.csv
####################################################################################### 
#Outcome column name
outcome_colname <- "Death_inHOSP"
selected_features <- c("UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                       "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                       "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3")
#1.Get model data
train_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
train_data <- train_data[,c(selected_features,outcome_colname)]
colnames(train_data)
table(train_data$Death_inHOSP)

#Outcome index 
outcome_index <- which(colnames(train_data) == outcome_colname)

#Train data part
if (ncol(train_data) == 2){ ##For data has one feature column, must add as.data.frame, and rename col
  train_X <- as.data.frame(train_data[,-outcome_index])
  colnames(train_X) <- colnames(train_data)[1]
}else{
  train_X <- train_data[,-outcome_index]
}

#Train label
train_Y <-  train_data[,outcome_index]

#train model
trained_model  <- train(train_X, train_Y,method= "svmLinear2" , trControl = trainControl("none", classProbs = TRUE),verbose=F) # Support Vector Machines
explainer <- shapr(as.matrix(train_data), trained_model)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(train_Y)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
#>      none     lstat         rm       dis      indus
#> 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#> 2: 22.446 0.1671903 -0.7088405 0.9689007  0.3786871
#> 3: 22.446 5.9888016  5.5450861 0.5660136 -1.4304350
#> 4: 22.446 8.2142203  0.7507569 0.1893368  1.8298305
#> 5: 22.446 0.5059890  5.6875106 0.8432240  2.2471152
#> 6: 22.446 1.9929674 -3.6001959 0.8601984  3.1510530

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))
