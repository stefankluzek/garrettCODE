######Prediction Practical 

#Installing packages
install.packages('dplyr', 'pmsampsize', 'pROC', 'rms', 'Hmisc')

#Installing Calibration Curves
devtools::install_github("BavoDC/CalibrationCurves", dependencies = TRUE, build_vignettes = TRUE)

library(dplyr) #Data wrangling
library(pmsampsize) #Sample size calculation
library(pROC) #Area under the curve
library(CalibrationCurves) #Calibration Plots
library(ggplot2) #Needed for CalibrationCurves

###### Clearing Your R Studio #####
rm(list = ls())


#####Getting the data #######
#NBA injury data (already cleaned & also no missing data)
#This data was data scraped from open access sources 
#Paper: Martin CL, Arundale AJ, Kluzek S, Ferguson T, Collins GS, Bullock GS. 
#Characterization of rookie season injury and illness and career longevity among National Basketball Association players. 
#JAMA network open. 2021 Oct 1;4(10):e2128199-.


#Get the data from Open Science Framework with link below
#https://osf.io/xywq4/

  
#Save the data set to your computer R Console, directly from Github.
nba <- read.csv("https://raw.githubusercontent.com/gsb15/NBA_Rookies_Data/main/NBA_Injuries_Rookie.csv")
 

#Explore the data 
View(nba)
names(nba)

str(nba)

nrow(nba) #625 athletes

#We want to predict a major lower extremity injury
#Previous epidemiological data has severe lower extremity injuries at 10% of players within a season
#Previous prediction models in sport have R-squared (explanation of variance) at 12% (0.12)
#Within this data set, there are five predictors, all are linear for this example 

#For the sample size calculation, we will use Riley et al.'s formula, from the 'pmsampsize' package 

pmsampsize(type = "b", csrsquared = 0.12, parameters = 8, prevalence = 0.10)

#560 athletes are required



##########Missing Data ###########################

##As stated above, no missing data in this dataset


#########Assessing Non-linearity ################

#As stated above, all predictors are linear


###########################################################################
########Create the logistic model 
###########################################################################
#Will use glm function 

model   <-  glm(major_lower_extremity_injury ~ agePlayer + BMI + DraftPicktwo + GroupPosition + InjuredRookie_dichot + SeasonsPlayed, 
                family = binomial,
                data = nba)

#Checking basic outputs
summary(model)


############################################################################
##### Internal Validation for overall performance, to reduce optimism bias.
############################################################################

set.seed(17)
results <- matrix(nrow = 1,ncol = 12) #1 Row, columns for prediction performance after internal validation

performance_metrics <- data.frame() #Data frame for each bootstrap performance metrics


#Number of bootstraps 
N <- 2000


#Bootstrap for loop 
  for(boot in 1:N){
    boot_sample <- nba[sample(nrow(nba), replace = T), ] 
    
    model   <-  glm(major_lower_extremity_injury ~ agePlayer + BMI + DraftPicktwo + GroupPosition + InjuredRookie_dichot + SeasonsPlayed, 
                    family = binomial, 
                    data = boot_sample)
    
    #Getting Linear Predictors
    lp_mod <- predict(model, newdata = nba)
    
    #Column Binding with the Data Frame
    actual_v_predict <- cbind(nba, lp_mod)
    
    
    # Obtain the c statistic / AUC
    discrimination <- pROC::roc(as.factor(actual_v_predict$major_lower_extremity_injury) ~ actual_v_predict$lp_mod, ci=TRUE)
    AUC <- discrimination$auc
    AUC <- as.numeric(AUC)
    
    #R^2
    r_square <- 1 - model$deviance/model$null.deviance
    r_square <- as.numeric(r_square)
    
    #Calibration Slope 
    mod_log <- glm(actual_v_predict$major_lower_extremity_injury ~ actual_v_predict$lp_mod, family = binomial, data = actual_v_predict)
    calibration_slope <- mod_log$coefficients[2]
    calibration_slope <- as.numeric(calibration_slope)
    
    #Calibration in the Large (CITL)
    mod_log_citl <- glm(major_lower_extremity_injury ~ offset(lp_mod), family="binomial", data = actual_v_predict)
    intercept <- mod_log_citl$coef 
    intercept <- as.numeric(intercept)  
    
    
    perform =  cbind(AUC, r_square, calibration_slope, intercept)
    
    perform = data.frame(perform)
    
    performance_metrics = rbind(performance_metrics, perform)
    
    print(paste("Progress:", boot, sep=""))
    
  }



#Aggregating Prediction Performance Across 
  
  #AUC Statistic
  results[1, 1] <- mean(performance_metrics[, 1], na.rm = T)
  
  #AUC Statistic Low 95% 
  results[1, 2] <- quantile(performance_metrics[, 1], probs = 0.025, na.rm = T)
  
  #AUC Statistic High 95% 
  results[1, 3] <- quantile(performance_metrics[, 1], probs = 0.975, na.rm = T)
  
  #R2
  results[1, 4] <- mean(performance_metrics[, 2], na.rm = T)
  
  #R2 Low 95% 
  results[1, 5] <- quantile(performance_metrics[, 2], probs = 0.025, na.rm = T)
  
  #R2 High 95% 
  results[1, 6] <- quantile(performance_metrics[, 2], probs = 0.975, na.rm = T)
  
  #Calibration 
  results[1, 7] <- mean(performance_metrics[, 3], na.rm = T)
  
  #Calibration Low 95% 
  results[1, 8] <- quantile(performance_metrics[, 3], probs = 0.025, na.rm = T)
  
  #Calibration High 95% 
  results[1, 9] <- quantile(performance_metrics[, 3], probs = 0.975, na.rm = T)
  
  #Intercept (Calibration in the Large)
  results[1, 10] <- mean(performance_metrics[, 4], na.rm = T)
  
  #Intercept (Calibration in the Large) Low 95% 
  results[1, 11] <- quantile(performance_metrics[, 4], probs = 0.025, na.rm = T)
  
  #Intercept (Calibration in the Large) High 95% 
  results[1, 12] <- quantile(performance_metrics[, 4], probs = 0.975, na.rm = T)


#Making into A data frame
results <- as.data.frame(results)

#Column Names
colnames(results) <- c("Discrimination_AUC", "AUC_Low_95CI", "AUC_High_95CI",
                        "RSquare", "RSquare_Low_95CI", "RSquare_High_95CI", 
                        "Calibration_Slope", "Calibration_Slope_Low_95CI", "Calibration_Slope_High_95CI",
                        "CITL", "CITL_Low_95CI", "CITL_High_95CI")

View(results)


##########################################
#Getting Calibration Plot
##########################################

model_calibration   <-  glm(major_lower_extremity_injury ~ agePlayer + BMI + DraftPicktwo + GroupPosition + InjuredRookie_dichot + SeasonsPlayed, 
                family = binomial, 
                data = nba)

summary(model_calibration)

val.prob.ci.2(y = nba$major_lower_extremity_injury, 
              logit =  predict(model_calibration, nba),
              dostats = F, #Does not report prediction statistics, we did this above :) 
              CL.BT = T, 
              xlim = c(-0.02, 0.6)) #CL.BT = T; performs 2,000 bootstraps








