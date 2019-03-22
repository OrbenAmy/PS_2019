##########################################################################################################
### Script 5: Create real world measure of tech use
##########################################################################################################

##########################################################################################################
# 1. Loading libraries, controlling analyses and loading data ############################################
##########################################################################################################

#####################################################################################
# a) Load libraries ####
#####################################################################################
library("tidyverse")
library("foreign")
library("lavaan")
library("heplots")

options(scipen=999)

#####################################################################################
# b) Load data ####
#####################################################################################
data_mcs <- read.csv(file="../data/cleaned/1_3_MCS_dataset.csv")

##########################################################################################################
# 2. Running SCAs via three main functions, get_data, get_model and get_coef##############################
##########################################################################################################

#####################################################################################
#### Function: get_data
#### This function takes the specification and makes the apporiate dataset
####
#### Input: 
#### results_frame = a data frame showing specifications on each line
#### dataset = a character saying what dataset the specifications are for
#### data = the data of interest
####
#### Output: A list with the folliwng items
#### results_frame = retain results_frame to use in next function
#### dataset = retain dataset to use in next function
#### data = dataset of interest, now with new dv and iv from specifications
####
#### Method: It goes to the row of interest (i) of the results frame.
#### There it reads what the relevant iv and dv variables are and extracts these
#### from the dataset of interest.
#####################################################################################
get_data <- function(results_frame, dataset, data) {
  
  # Setup variable names for technology use
  if (results_frame$tech[i] == "sr_tech") {
    data$iv <- data$sr_tech
  } else if (results_frame$tech[i]  == "tech_weekday_d") {
    data$iv <- data$tech_di_wd
  } else if (results_frame$tech[i]  == "tech_weekend_d") {
    data$iv <- data$tech_di_we
  } else if (results_frame$tech[i]  == "tech_weekday_c") {
    data$iv <- data$tech_cont_wd
  } else if (results_frame$tech[i]  == "tech_weekend_c") {
    data$iv <- data$tech_cont_we
  } else if (results_frame$tech[i]  == "tech_2hr_wd") {
    data$iv <- data$tech_2hr_wd
  } else if (results_frame$tech[i]  == "tech_1hr_wd") {
    data$iv <- data$tech_1hr_wd 
  } else if (results_frame$tech[i]  == "tech_30m_wd") {
    data$iv <- data$tech_30m_wd
  } else if (results_frame$tech[i]  == "tech_2hr_we") {
    data$iv <- data$tech_2hr_we 
  } else if (results_frame$tech[i]  == "tech_1hr_we") {
    data$iv <- data$tech_1hr_we
  } else if (results_frame$tech[i]  == "tech_30m_we") {
    data$iv <- data$tech_30m_we
  }
  
  # MSetup variable names for wellbeing
  if (dataset == "psid"){
    if (results_frame$measure[i] == "selfesteem") {
      data$dv <- data$selfesteem
    } else if (results_frame$measure[i] == "wellbeing") {
      data$dv <- data$wellbeing
    } 
    
  } else if (dataset == "gui") { #gui
    if (results_frame$measure[i] == "wellbeing") {
      data$dv <- data$wellbeing_measure
    } else if (results_frame$measure[i] == "sdq") {
      data$dv <- data$sdqtot
    } 
    
  } else {
    if (results_frame$measure[i] == "selfesteem") {
      data$dv <- data$selfesteem
    } else if (results_frame$measure[i] == "wellbeing") {
      data$dv <- data$wellbeing
    } else if (results_frame$measure[i] == "sdq") {
      data$dv <- data$sdqtot
    }
  }
  
  #make output into list
  get_data_list <- list(results_frame, dataset, data)
  return(get_data_list)
}


#####################################################################################
#### Function: get_model 
#### This version is slightly different because it uses an unstandardised iv
#### 
####
#### Input: A list from the previous function containing:
#### results_frame = a data frame showing specifications on each line
#### dataset = a character saying what dataset the specifications are for
#### data = the data of interest with relevant iv and dv
####
#### Output: A list with the folliwng items
#### reg = regression model from the specification
#### results_frame = retain results_frame to use in next function
#### data = dataset of interest
####
#### Method: It goes to the row of interest (i) of the results frame.
#### There it reads what the relevant dataset and control variables,
#### this determines the model which is then run on the dataset of interest
#####################################################################################
get_model_scalepart <- function(get_data_list){
  
  #unpackage list
  results_frame <- get_data_list[[1]]
  dataset <- get_data_list[[2]]
  data <- get_data_list[[3]]
  
  # Model with control
  if (results_frame$control[i] == 1) {
    if (dataset == "mcs") {
      reg <- lm(
        #iv, gender, age, educational motivation
        scale(dv) ~ iv + scale(sex) + scale(age) + scale(edumot) + 
          #ethnicity, closeness to parents, natural father live w/ you
          scale(fd06e00) + scale(clpar) + scale(fcpaab00) + 
          #parent word activity score, parent highest academic qualification, employment of parent
          scale(fpwrdscm) + scale(fdacaq00) + scale(fd05s00) + 
          #child long-term illness, amount time pri. caretaker w/ child, Kessler scale caretaker, num. sibs in houshold
          scale(fpclsi00) + scale(fpchti00) + scale(fdkessl) + scale(fdtots00) + 
          # equivalised weekly family income
          scale(foede000), 
        data = data
      )
      
    } else {
      reg <- lm(
        scale(dv) ~ iv + scale(sex) + scale(age),
        data = data
      )
    }
  } else {
    reg <- lm(
      scale(dv) ~ iv,
      data = data
    )
  }
  
  # Return model and results_frame
  get_model_list <- list(reg, results_frame, data)
  return(get_model_list)
}

#####################################################################################
#### Function: get_coef
#### This function extracts variables of interest from specification regression
####
#### Input: A list from the previous function with the following items
#### reg = regression model from the specification
#### results_frame = retain results_frame to use in next function
#### data = dataset of interest
####
#### Output:
#### results_frame[i,] = the relevant line of the results frame with key variables
####
#### Method: The function takes the relevant regression and results frame.
#### It extracts the t_value, regression coefficiant, p_value, SE and N
#### It adds these to the relevant results frame
#####################################################################################
get_coef <- function(get_model_list) {
  
  #unpackage list
  reg <- get_model_list[[1]]
  results_frame <- get_model_list[[2]]
  
  results_frame$t_value[i] <-
    summary(reg)$coef[[2, 3]] %>% {
      ifelse(. == 0, NA, .)
    }
  results_frame$effect[i] <-
    summary(reg)$coef[[2, 1]] %>% {
      ifelse(. == 0, NA, .)
    }
  results_frame$p_value[i] <- summary(reg)$coef[[2, 4]]
  results_frame$standard_error[i] <-
    summary(reg)$coef[[2, 2]] %>% {
      ifelse(. == 0, NA, .)
    }
  results_frame$number[i] <- nobs(reg)
  results_frame$rsqrd[i] <- etasq(reg)["scale(iv)", "Partial eta^2"]
  return(results_frame[i,])
}

#####################################################################################
# a) Setup Results Frame ####
# Furthermore we add an additional results frame (suffix = boot) 
# for our bootstrapping models later in the code
#####################################################################################

###############################
#### Function: get_results_frame
#### Makes a data frame to add
#### results of SCA in
####
#### Input: 
#### measure = measures of wellbeing
#### tech = measures of tech
#### controls = relevant controls
####
#### Output:
#### results_frame = data frame
####
#### Method: it makes grid with
#### every possible combo of 
#### input variables
###############################

get_results_frame <- function(measure, tech, control){
  results_frame <- expand.grid(measure, tech, control)
  names(results_frame) <- c("measure", "tech", "control")
  results_frame[, c("t_value", "effect", "p_value", "standard_error", "number", "rsqrd")] <- NA
  return(results_frame)
}

###############################
# i) Define measures
###############################
### definewellbeing measures for each dataset
measure_mcs <- c("selfesteem", "wellbeing", "sdq")

### defince tech and controls (same for all datasets)
tech <- c("sr_tech", 
          "tech_weekday_d", "tech_weekend_d", 
          "tech_weekday_c", "tech_weekend_c", 
          "tech_2hr_wd", "tech_1hr_wd", "tech_30m_wd", 
          "tech_2hr_we", "tech_1hr_we", "tech_30m_we")
control <- c(0, 1)

###############################
# ii) Run functions
###############################
results_frame_mcs <- get_results_frame(measure_mcs, tech, control)

#####################################################################################
# b) Run Specification Curve Analyses ####
#####################################################################################
for (i in 1:nrow(results_frame_mcs)) {
    print(paste0("MCS_SCA_", i))
    results_frame_mcs[i,] <- get_coef(get_model_scalepart(get_data(
      results_frame = results_frame_mcs, dataset = "mcs", data = data_mcs
    )))}

### make results with one cont variable
results_frame_mcs_confirmatory <- results_frame_mcs

#####################################################################################
# c) Get actionable measure ####
#####################################################################################
median <- results_frame_mcs_confirmatory %>% filter(tech == "tech_weekday_c" | tech == "tech_weekend_c") %>% 
  dplyr::summarise(median = median(effect)) %>% pull(median)

print(paste0("To change 0.5 SD of well-being measures, it would take an increase of ", round(abs(0.5/median),2),
       " hours of digital tech use."))

maximum <- results_frame_mcs_confirmatory %>% filter(tech == "tech_weekday_c" |  tech == "tech_weekend_c") %>% 
  arrange(effect) %>% select(effect) %>% top_n(n=-1) %>% pull(effect)

print(paste0("To change 0.5 SD of well-being measures, it would take an increase of ", round(abs(0.5/maximum),2),
             " hours of digital tech use on a weekday, if we only choose the most negative specification."))

