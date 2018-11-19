##########################################################################################################
### Script 3: Run SCAs and permutations ####
### This scripts takes three datasets (GUI, PSID, MCS)
### and runs SCAs using regression (not structural equation modelling)
### It consistes of multiple steps 
### 1. Loading libraries, controlling analyses and loading data
### 2. Running SCAs via three main functions get_data, get_model, get_coef
### 3. Make null models for bootstrapping
### 4. Run bootstrap models
### 5. Analyse bootstrap models 
### The output includes three specification curve analyses and one csv file of the bootstrap tests
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
set.seed(2018)

#####################################################################################
# b) Turn on analyses ####
# 1 = yes, 0 = no
#####################################################################################
run_sca <- 1
run_null <- 1
run_boot <- 1

#####################################################################################
# c) Determine how many bootstraps to run
#####################################################################################
bootstraps <- 500

#####################################################################################
# d) Load data ####
#####################################################################################
data_gui <- read.csv(file="1_1_GUI_dataset.csv")
data_psid <- read.csv(file="1_2_PSID_dataset.csv")
data_mcs <- read.csv(file="1_3_MCS_dataset.csv")

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
####        furthermore for the bootstraps sample the data is bootstrapped
####
#### Method: It goes to the row of interest (i) of the results frame.
#### There it reads what the relevant iv and dv variables are and extracts these
#### from the dataset of interest.
#####################################################################################
get_data <- function(results_frame, dataset, data, boot) {
  
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
  
  # Do bootstrapping
  if(boot == TRUE){
    if (b <= bootstraps){
      k=sample(nrow(data),replace=T)
      data <- data[k, ]
    } else {}
  } else {
    
  }

  #make output into list
  get_data_list <- list(results_frame, dataset, data)
  return(get_data_list)
}


#####################################################################################
#### Function: get_model (2 versions)
#### There are two versions: get_model_scale and get_model_noscale
#### _scale standardises the variables before computing a regression
#### _noscale does not standardise the variables before computing the regression (for bootstrapping)
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
get_model_scale <- function(get_data_list){
  
  #unpackage list
  results_frame <- get_data_list[[1]]
  dataset <- get_data_list[[2]]
  data <- get_data_list[[3]]
  
  # Model with control
  if (results_frame$control[i] == 1) {
    if (dataset == "mcs") {
      reg <- lm(
        #iv, gender, age, educational motivation
        scale(dv) ~ scale(iv) + scale(sex) + scale(age) + scale(edumot) + 
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
        scale(dv) ~ scale(iv) + scale(sex) + scale(age),
        data = data
      )
    }
  } else {
    reg <- lm(
      scale(dv) ~ scale(iv),
      data = data
    )
  }
  
  # Return model and results_frame
  get_model_list <- list(reg, results_frame, data)
  return(get_model_list)
}

get_model_noscale <- function(get_data_list){
  
  #unpackage list
  results_frame <- get_data_list[[1]]
  dataset <- get_data_list[[2]]
  data <- get_data_list[[3]]
  
  # Model with control
  if (results_frame$control[i] == 1) {
    if (dataset == "mcs") {
      reg <- lm(
        #iv, gender, age, educational motivation
        dv ~ iv + sex + age + edumot + 
          #ethnicity, closeness to parents, natural father live w/ you
          fd06e00 + clpar + fcpaab00 + 
          #parent word activity score, parent highest academic qualification, employment of parent
          fpwrdscm + fdacaq00 + fd05s00 + 
          #child long-term illness, amount time pri. caretaker w/ child, Kessler scale caretaker, num. sibs in houshold
          fpclsi00 + fpchti00 + fdkessl + fdtots00 + 
          # equivalised weekly family income
          foede000, 
        data = data
      )
      
    } else {
      reg <- lm(
        dv ~ iv + sex + age,
        data = data
      )
    }
  } else {
    reg <- lm(
      dv ~ iv,
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
measure_gui <- c("sdq", "wellbeing")
measure_psid <- c("selfesteem", "wellbeing")
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
results_frame_gui <- get_results_frame(measure_gui, tech, control)
results_frame_psid <- get_results_frame(measure_psid, tech, control)
results_frame_mcs <- get_results_frame(measure_mcs, tech, control)

###############################
# iii) Make bootstrap version
###############################
results_frame_gui_boot <- results_frame_gui
results_frame_psid_boot <- results_frame_psid 
results_frame_mcs_boot <- results_frame_mcs

#####################################################################################
# b) Run Specification Curve Analyses ####
#####################################################################################
sca_gui <- list(0)
sca_psid <- list(0)
sca_mcs <- list(0)

if (run_sca == 1){

  for (b in 1:(bootstraps+1)) {
    print(paste0("gui_bootstrap_",b))  
  for (i in 1:nrow(results_frame_gui)) {
    results_frame_gui[i,] <- get_coef(get_model_scale(get_data(
      results_frame = results_frame_gui, dataset = "gui", data = data_gui, boot = TRUE
    )))
    }
    sca_gui[[b]] <- results_frame_gui
    }

  for (b in 1:(bootstraps+1)) {
    print(paste0("psid_bootstrap_",b))    
  for (i in 1:nrow(results_frame_psid)) {
    results_frame_psid[i,] <- get_coef(get_model_scale(get_data(
      results_frame = results_frame_psid, dataset = "psid", data = data_psid, boot = TRUE
    )))
  }
    sca_psid[[b]] <- results_frame_psid
  }
  
  for (b in 1:(bootstraps+1)) {
      print(paste0("mcs_bootstrap_",b))  
  for (i in 1:nrow(results_frame_mcs)) {
    results_frame_mcs[i,] <- get_coef(get_model_scale(get_data(
      results_frame = results_frame_mcs, dataset = "mcs", data = data_mcs, boot = TRUE
    )))
  }
    sca_mcs[[b]] <- results_frame_mcs
  }
  
  # Save SCAs
  write.csv(sca_gui[[bootstraps+1]], file="3_1_GUI_SCA_nonSEM.csv")
  write.csv(sca_psid[[bootstraps+1]], file="3_2_PSID_SCA_nonSEM.csv")
  write.csv(sca_mcs[[bootstraps+1]], file="3_3_MCS_SCA_nonSEM.csv")
  
  write_rds(sca_gui, "3_1_GUI_SCA_boot_nonSEM.rds")
  write_rds(sca_psid, "3_2_PSID_SCA_boot_nonSEM.rds")
  write_rds(sca_mcs, "3_3_MCS_SCA_boot_nonSEM.rds")
  
} else {
  
  # Read SCAs
  results_frame_gui <- read.csv(file="3_1_GUI_SCA_nonSEM.csv")
  results_frame_psid <- read.csv(file="3_2_PSID_SCA_nonSEM.csv")
  results_frame_mcs <- read.csv(file="3_3_MCS_SCA_nonSEM.csv")
  
  sca_gui <- read_rds("3_1_GUI_SCA_boot_nonSEM.rds")
  sca_psid <- read_rds("3_2_PSID_SCA_boot_nonSEM.rds")
  sca_mcs <- read_rds("3_3_MCS_SCA_boot_nonSEM.rds")
  
}

#####################################################################################
# c) Make results frame ####
#####################################################################################
get_boot_results <- function(boot_sca){
    all_data_frames <- do.call("rbind", boot_sca[1:bootstraps])
    results_frame <- all_data_frames %>% group_by(measure, tech, control) %>% dplyr::summarise_all(funs(mean))
    
    results_frame[, c("effect_lower", "effect_upper")] <- all_data_frames %>% group_by(measure, tech, control) %>%
      summarise(`effect_lower`=quantile(effect, probs=0.025),
                `effect_higher`=quantile(effect, probs=0.975)) %>% ungroup() %>% select(effect_lower, effect_higher)
    results_frame[, c("rsqrd_lower", "rsqrd_upper")] <- all_data_frames %>% group_by(measure, tech, control) %>%
      summarise(`rsqrd_lower`=quantile(rsqrd, probs=0.025),
                `rsqrd_higher`=quantile(rsqrd, probs=0.975)) %>% ungroup() %>% select(rsqrd_lower, rsqrd_higher)
    return(results_frame)
}

boot_results_frame_gui <- get_boot_results(sca_gui)
boot_results_frame_psid <- get_boot_results(sca_psid)
boot_results_frame_mcs <- get_boot_results(sca_mcs)

write.csv(boot_results_frame_gui, file="3_1_GUI_SCA_bootstrapped.csv")
write.csv(boot_results_frame_psid, file="3_2_PSID_SCA_bootstrapped.csv")
write.csv(boot_results_frame_mcs, file="3_3_MCS_SCA_bootstrapped.csv")

##########################################################################################################
# 3. Make Null Models for Bootstrapping ##################################################################
##########################################################################################################

#####################################################################################
#### Function: get_ynull
#### This function contrains model under null for each specification
####
#### Input: A list from the previous function with the following items
#### reg = regression model from the specification
#### results_frame = retain results_frame to use in next function
#### data = dataset of interest
####
#### Output:
#### y.null.i = dv data for specification constrained under null
####
#### Method: The function takes the relevant regression and results frame.
#### It extracts the regression coefficiant, and multiplies it by the iv
#### It substracts this from the dv creating a dataset where null is true
#####################################################################################
get_ynull <- function(get_model_list) {
  
  #unpackage list
  reg <- get_model_list[[1]]
  data <- get_model_list[[3]]
  
  # extract coefficient
  b.i <-
    summary(reg)$coef[[2, 1]] %>% {
      ifelse(. == 0, NA, .)
    }
  
  #make null model
  y.null.i <- data$dv-(b.i*data$iv)
  
  return(y.null.i)
}


###############################
# i) Run Null Models
###############################
y_null_gui <- list(0)
y_null_mcs <- list(0)
y_null_psid <- list(0)

if (run_null == 1){
  
  for (i in 1:nrow(results_frame_gui)) {
    print(paste0("GUI_NULL_", i))
    y_null_gui[[i]] <- get_ynull(get_model_noscale(get_data(
      results_frame = results_frame_gui, dataset = "gui", data = data_gui, boot = FALSE
    )))
  }
  
  for (i in 1:nrow(results_frame_psid)) {
    print(paste0("PSID_NULL_", i))
    y_null_psid[[i]] <- get_ynull(get_model_noscale(get_data(
      results_frame = results_frame_psid, dataset = "psid", data = data_psid, boot = FALSE
    )))
  }
  
  for (i in 1:nrow(results_frame_mcs)) {
    print(paste0("MCS_NULL_", i))
    y_null_mcs[[i]] <- get_ynull(get_model_noscale(get_data(
      results_frame = results_frame_mcs, dataset = "mcs", data = data_mcs, boot = FALSE
    )))
  }
  
  # Save null models
  write_rds(y_null_gui, "3_1_GUI_ynull.rds")
  write_rds(y_null_psid, "3_2_PSID_ynull.rds")
  write_rds(y_null_mcs, "3_3_MCS_ynull.rds")
  
} else {
  
  # Read null models
  y_null_gui <- read_rds("3_1_GUI_ynull.rds")
  y_null_psid <- read_rds("3_2_PSID_ynull.rds")
  y_null_mcs <- read_rds("3_3_MCS_ynull.rds")
  
}


##########################################################################################################
# 4. Run Bootstrap Models ################################################################################
##########################################################################################################

#####################################################################################
#### Function: get_boot_data
#### This function takes the specification and makes the apporiate bootstrapped dataset
#### Except the last round trough (b >= bootstraps) it runs the original function
####
#### Input: 
#### results_frame = a data frame showing specifications on each line
#### dataset = a character saying what dataset the specifications are for
#### data = the data of interest
#### y.null = list of null datasets for each specification
####
#### Output: A list with the folliwng items
#### results_frame = retain results_frame to use in next function
#### dataset = retain dataset to use in next function
#### data = dataset of interest, now with new dv and iv for specification constrained under null
####
#### Method: It goes to the row of interest (i) of the results frame.
#### There it reads what the relevant iv and dv variables are and extracts these
#### from the dataset of interest.
#### During the bootstrapping tests (b <= bootstraps) it makes a bootstrapped dataset
#### that is constrained under the null. In the last test, it makes non-bootstrapped dataset
#####################################################################################
get_boot_data <- function(results_frame, dataset, data, y.null) {
  
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
  
  # Setup variable names for wellbeing
  if (b <= bootstraps){
    data$dv <- y.null[[i]]
  } else {
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
  }}
  
  # Do bootstrapping
  if (b <= bootstraps){
  k=sample(nrow(data),replace=T)
  data <- data[k, ]
  } else {}
  
  #make output into list
  get_data_list <- list(results_frame, dataset, data)
  return(get_data_list)
}

#####################################################################################
# a) Run Bootstrapping ####
#####################################################################################
bootstraps_gui <- list(0)
bootstraps_psid <- list(0)
bootstraps_mcs <- list(0)

if (run_boot == 1){
  
for (b in 1:(bootstraps+1)) {
  print(paste0("gui_bootstrap_",b))
  results_frame_gui_it <- results_frame_gui_boot
  for (i in 1:nrow(results_frame_gui_it)) {
    results_frame_gui_it[i,] <- get_coef(get_model_noscale(get_boot_data(
      results_frame = results_frame_gui_it, dataset = "gui", data = data_gui, y.null = y_null_gui
    )))
  }
  bootstraps_gui[[b]] <- results_frame_gui_it
}

for (b in 1:(bootstraps+1)) {
  print(paste0("psid_bootstrap_",b))
  results_frame_psid_it <- results_frame_psid_boot
  for (i in 1:nrow(results_frame_psid_it)) {
    results_frame_psid_it[i,] <- get_coef(get_model_noscale(get_boot_data(
      results_frame = results_frame_psid_it, dataset = "psid", data = data_psid, y.null = y_null_psid
    )))
  }
  bootstraps_psid[[b]] <- results_frame_psid_it
}

for (b in 1:(bootstraps+1)) {
  print(paste0("mcs_bootstrap_",b))
  results_frame_mcs_it <- results_frame_mcs_boot
  for (i in 1:nrow(results_frame_mcs_it)) {
    results_frame_mcs_it[i,] <- get_coef(get_model_noscale(get_boot_data(
      results_frame = results_frame_mcs_it, dataset = "mcs", data = data_mcs, y.null = y_null_mcs
    )))
  }
  bootstraps_mcs[[b]] <- results_frame_mcs_it
}
  
  write_rds(bootstraps_gui, "3_1_GUI_boot.rds")
  write_rds(bootstraps_psid, "3_2_PSID_boot.rds")
  write_rds(bootstraps_mcs, "3_3_MCS_boot.rds")
  
} else {
  
  # Read null models
  bootstraps_gui <- read_rds("3_1_GUI_boot.rds")
  bootstraps_psid <- read_rds("3_2_PSID_boot.rds")
  bootstraps_mcs <- read_rds("3_3_MCS_boot.rds")
  
}

##########################################################################################################
# 5. Analyse Bootstrapped Models: #########################################################################
##########################################################################################################

#####################################################################################
#### Function: fill_per
#### This function takes the results of the bootstrapped models (a list of results
#### frames) and summarises them into one permutation frame
####
#### Input: 
#### boot_results = a list of results_frames from the bootstrapping procedure
#### frame = permutation frame which can hold summarised results
####
#### Output:
#### frame = permutation frame with summarised bootstrapped results
####
#### Method: The function takes the bootstrapped results and goes through them 
#### individually (m). For each tech variable and control (defined by filter value, 
#### i) it filters the bootstrapped result frame so that it only examines 
#### specifications with the relevant tech or control values. It also filters once
#### for both weekend and weekday continuous tech use as that is part of our MCS
#### hypothesis testing. It first gets the median size of the effect and then the 
#### amount of effects that are positivie or negatve. Lastly it examines the amount 
#### of results that are significant and positive or negative. 
#####################################################################################
fill_per <- function(boot_results, frame){
  for (m in 1:(bootstraps+1)){
    
    boot_results_m <- as.data.frame(boot_results[m])
    n <- 2
    
    for (i in 1:15){
      
      filter_value <- c("", "sr_tech", "tech_weekday_c", "tech_weekend_c",
                        "tech_weekday_d", "tech_weekend_d",
                        "tech_30m_wd", "tech_30m_we",
                        "tech_1hr_wd", "tech_1hr_we",
                        "tech_2hr_wd", "tech_2hr_we",
                        0, 1)[i]
      if(i == 1){
        results_subset <- boot_results_m
      } else if (i > 1 & i < 13) {
        results_subset <- boot_results_m %>% filter(tech == filter_value)
      } else if (i == 13 | i == 14) {
        results_subset <- boot_results_m %>% filter(control == filter_value)
      } else if (i == 15){
        results_subset <- boot_results_m %>% filter(tech == "tech_weekday_c" | tech == "tech_weekend_c")
      }
      
      frame[m, n] <- median(results_subset[["effect"]], na.rm = TRUE)
      n <- n+1
      frame[m, n] <- length(results_subset[results_subset$effect < 0, "effect"])
      n <- n+1
      frame[m, n] <- length(results_subset[results_subset$effect > 0, "effect"])
      n <- n+1
      sig_data <- filter(results_subset, p_value < 0.05)
      
      if(nrow(sig_data) > 0){
        frame[m, n] <- length(sig_data[sig_data$effect < 0, "effect"])
        n <- n+1
        frame[m, n] <- length(sig_data[sig_data$effect > 0, "effect"])
        n <- n+1
      } else {
        frame[m, n] <- 0
        n <- n+1
        frame[m, n] <- 0
        n <- n+1
      }
    }
  }
  
  return(frame)
}

#####################################################################################
# a) Make dataframe to hold the results ####
#####################################################################################
permutation_frame <-
  data.frame(matrix(NA, nrow = bootstraps, ncol = 76))
names(permutation_frame) <-
  c(
    "permutation_number",
    "effect",
    "sign.neg",
    "sign.pos",
    "sign.sig.neg",
    "sign.sig.pos",
    
    #self-report
    "effect.sr",
    "sign.neg.sr",
    "sign.pos.sr",
    "sign.sig.neg.sr",
    "sign.sig.pos.sr",
    
    #continuous
    "effect.cont.wd",
    "sign.neg.cont.wd",
    "sign.pos.cont.wd",
    "sign.sig.neg.cont.wd",
    "sign.sig.pos.cont.wd",
    
    "effect.cont.we",
    "sign.neg.cont.we",
    "sign.pos.cont.we",
    "sign.sig.neg.cont.we",
    "sign.sig.pos.cont.we",
    
    #dichotomous
    "effect.di.wd",
    "sign.neg.di.wd",
    "sign.pos.di.wd",
    "sign.sig.neg.di.wd",
    "sign.sig.pos.di.wd",
    
    "effect.di.we",
    "sign.neg.di.we",
    "sign.pos.di.we",
    "sign.sig.neg.di.we",
    "sign.sig.pos.di.we",
    
    #30min
    "effect.30.wd",
    "sign.neg.30.wd",
    "sign.pos.30.wd",
    "sign.sig.neg.30.wd",
    "sign.sig.pos.30.wd",
    
    "effect.30.we",
    "sign.neg.30.we",
    "sign.pos.30.we",
    "sign.sig.neg.30.we",
    "sign.sig.pos.30.we",
    
    #1hour
    "effect.1.wd",
    "sign.neg.1.wd",
    "sign.pos.1.wd",
    "sign.sig.neg.1.wd",
    "sign.sig.pos.1.wd"
    ,
    "effect.1.we",
    "sign.neg.1.we",
    "sign.pos.1.we",
    "sign.sig.neg.1.we",
    "sign.sig.pos.1.we",
    
    #2hour
    "effect.2.wd",
    "sign.neg.2.wd",
    "sign.pos.2.wd",
    "sign.sig.neg.2.wd",
    "sign.sig.pos.2.wd",
    
    "effect.2.we",
    "sign.neg.2.we",
    "sign.pos.2.we",
    "sign.sig.neg.2.we",
    "sign.sig.pos.2.we",
    
    #controls
    "effect.nc",
    "sign.neg.nc",
    "sign.pos.nc",
    "sign.sig.neg.nc",
    "sign.sig.pos.nc",
    
    "effect.c",
    "sign.neg.c",
    "sign.pos.c",
    "sign.sig.neg.c",
    "sign.sig.pos.c",
    
    #hypothesis testing
    "effect.cont.total",
    "sign.neg.cont.total",
    "sign.pos.cont.total",
    "sign.sig.neg.cont.total",
    "sign.sig.pos.cont.total"
  )

permutation_frame$permutation_number <- seq.int(bootstraps)
per_frame_gui <- permutation_frame
per_frame_psid <- permutation_frame
per_frame_mcs <- permutation_frame

#####################################################################################
# b) run fill_per function to fill permutation frames ####
#####################################################################################
per_frame_gui <- fill_per(bootstraps_gui, per_frame_gui)
per_frame_psid <- fill_per(bootstraps_psid, per_frame_psid)
per_frame_mcs <- fill_per(bootstraps_mcs, per_frame_mcs)

write.csv(per_frame_gui, file="3_per_frame_gui.csv")
write.csv(per_frame_psid, file="3_per_frame_psid.csv")
write.csv(per_frame_mcs, file="3_per_frame_mcs.csv")

#####################################################################################
# c) anaylse the permutation frame to test significance ####
#####################################################################################

#####################################################################################
#### Function: analyse_boot
#### The function analyses the bootstrapping models to test significance
####
#### Input: 
#### boot_results = the permutation frame summarising the results of the bootstraps
#### name = the name of the dataset to be added to the table
####
#### Output:
#### table = a line of a data table summarising the results of the bootstraps for
####         one of the datasets
####
#### Method: For each column of the specified tests we examine how many bootstrap
#### tests have larger effects than the original sca. Then we examine how many have
#### more effects in the direction that most effects go and lastly we examine 
#### whether they have more significant effects in the major direction.
#####################################################################################
analyse_boot <- function(boot_results, name){
  
  for(i in 1:15){
    suffixes <- c("", ".sr", ".cont.wd", ".cont.we", ".di.wd", ".di.we", ".30.wd", ".30.we",
                  ".1.wd", ".1.we", ".2.wd", ".2.we", ".nc", ".c", ".cont.total")
    suffix <- suffixes[i]
    
    ### 1. effect sizes
    assign(paste0("p1", suffix),
           mean(abs(boot_results[1:bootstraps, paste0("effect", suffix)]) >= abs(boot_results[(bootstraps+1), paste0("effect", suffix)])))
    
    ### 2. sign of effect 
    sign <- pmax(boot_results[1:bootstraps, paste0("sign.pos", suffix)], boot_results[1:bootstraps, paste0("sign.neg", suffix)])
    sign_obs <- pmax(boot_results[(bootstraps+1), paste0("sign.pos", suffix)], boot_results[(bootstraps+1), paste0("sign.neg", suffix)])
    assign(paste0("p2", suffix),
           mean(sign >= sign_obs))
    
    ### 3. sign of significant effects
    sign <- pmax(boot_results[1:bootstraps, paste0("sign.sig.pos", suffix)], boot_results[1:bootstraps, paste0("sign.sig.neg", suffix)])
    sign_obs <- pmax(boot_results[(bootstraps+1), paste0("sign.sig.pos", suffix)], boot_results[(bootstraps+1), paste0("sign.sig.neg", suffix)])
    assign(paste0("p3", suffix),
           mean(sign >= sign_obs))
    
  }
  table <- c(name,p1,p1.sr,p1.cont.wd,p1.cont.we,p1.di.wd,p1.di.we,p1.30.wd,p1.30.we, p1.1.wd, p1.1.we, p1.2.wd,
             p1.2.we,p1.nc,p1.c,p1.cont.total,p2,p2.sr,p2.cont.wd,p2.cont.we,p2.di.wd,p2.di.we,p2.30.wd,p2.30.we,p2.1.wd,
             p2.1.we,p2.2.wd,p2.2.we,p2.nc,p2.c,p2.cont.total,p3,p3.sr,p3.cont.wd,p3.cont.we,p3.di.wd,     
             p3.di.we,p3.30.wd,p3.30.we,p3.1.wd,p3.1.we,p3.2.wd,p3.2.we,p3.nc,p3.c,p3.cont.total)
  return(table)
}

bootstrap_result <- data.frame(matrix(NA, nrow = 3, ncol = 46))
names(bootstrap_result) <- c("name", "p1", "p1.sr", "p1.cont.wd", "p1.cont.we", "p1.di.wd", "p1.di.we", 
                             "p1.30.wd", "p1.30.we", "p1.1.wd", "p1.1.we", "p1.2.wd", "p1.2.we", "p1.nc", "p1.c", "p1.cont.total", 
                             "p2", "p2.sr", "p2.cont.wd", "p2.cont.we", "p2.di.wd", "p2.di.we", "p2.30.wd", "p2.30.we", "p2.1.wd", 
                             "p2.1.we", "p2.2.wd", "p2.2.we", "p2.nc", "p2.c", "p2.cont.total", "p3", "p3.sr", "p3.cont.wd", "p3.cont.we", 
                             "p3.di.wd", "p3.di.we", "p3.30.wd", "p3.30.we", "p3.1.wd", "p3.1.we", "p3.2.wd", "p3.2.we", "p3.nc", "p3.c", 
                             "p3.cont.total")
bootstrap_result[1,] <- analyse_boot(per_frame_gui, "GUI")
bootstrap_result[2,] <- analyse_boot(per_frame_psid, "PSID")
bootstrap_result[3,] <- analyse_boot(per_frame_mcs, "MCS")

write.csv(bootstrap_result, file="3_per_results.csv")
