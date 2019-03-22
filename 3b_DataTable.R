##########################################################################################################
### Script 3b: Make Results Table ####
### This scripts takes the three completed SCAs and Bootstraps for three datasets (GUI, PSID, MCS)
### It creates a table of results for all their results and also aggregate effects.
##########################################################################################################

##########################################################################################################
# Loading libraries, controlling analyses and loading data ############################################
##########################################################################################################

#####################################################################################
# a) Load libraries ####
#####################################################################################
library("plyr")
library("tidyverse")

## set to how many bootstraps I want to do
bootstraps <- 500

#####################################################################################
# b) Load SCAs, SCAs with bootstrap and Bootstrapped Models ####
#####################################################################################  
results_frame_gui <- read.csv(file="../data/objects/3_1_GUI_SCA.csv")
results_frame_psid <- read.csv(file="../data/objects/3_2_PSID_SCA.csv")
results_frame_mcs <- read.csv(file="../data/objects/3_3_MCS_SCA.csv")

boot_sca_gui <- read_rds("../data/objects/3_1_GUI_SCAboot.rds")
boot_sca_psid <- read_rds("../data/objects/3_2_PSID_SCAboot.rds")
boot_sca_mcs <- read_rds("../data/objects/3_3_MCS_SCAboot.rds")

bootstraps_gui <- read_rds("../data/objects/3_1_GUI_boot.rds")
bootstraps_psid <- read_rds("../data/objects/3_2_PSID_boot.rds")
bootstraps_mcs <- read_rds("../data/objects/3_3_MCS_boot.rds")

#####################################################################################
# b) At the beginning of this file it makes sense to add columns                 ####
#    to the SCA frames that show the sign and significance of each specification ####
#####################################################################################  
make_sign <- function(results_frame){
  results_frame$sign <- ifelse(results_frame$effect > 0, "positive", "negative")
  results_frame$sig_sign <- ifelse(results_frame$p_value > 0.05, NA, ifelse(results_frame$effect > 0, "sig_positive", "sig_negative"))
  results_frame$sign <- as.factor(results_frame$sign)
  results_frame$sig_sign <- as.factor(results_frame$sig_sign)
  return(results_frame)
}

results_frame_gui <- make_sign(results_frame_gui)
results_frame_psid <- make_sign(results_frame_psid)
results_frame_mcs <- make_sign(results_frame_mcs)

##########################################################################################################
# 1. Make Bootstrapped Model Table: ######################################################################
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

bootstrap_results <- data.frame(matrix(NA, nrow = 3, ncol = 46))
names(bootstrap_results) <- c("name", "p1", "p1.sr", "p1.cont.wd", "p1.cont.we", "p1.di.wd", "p1.di.we", 
                             "p1.30.wd", "p1.30.we", "p1.1.wd", "p1.1.we", "p1.2.wd", "p1.2.we", "p1.nc", "p1.c", "p1.cont.total", 
                             "p2", "p2.sr", "p2.cont.wd", "p2.cont.we", "p2.di.wd", "p2.di.we", "p2.30.wd", "p2.30.we", "p2.1.wd", 
                             "p2.1.we", "p2.2.wd", "p2.2.we", "p2.nc", "p2.c", "p2.cont.total", "p3", "p3.sr", "p3.cont.wd", "p3.cont.we", 
                             "p3.di.wd", "p3.di.we", "p3.30.wd", "p3.30.we", "p3.1.wd", "p3.1.we", "p3.2.wd", "p3.2.we", "p3.nc", "p3.c", 
                             "p3.cont.total")
bootstrap_results[1,] <- analyse_boot(per_frame_gui, "gui")
bootstrap_results[2,] <- analyse_boot(per_frame_psid, "psid")
bootstrap_results[3,] <- analyse_boot(per_frame_mcs, "mcs")
bootstrap_results[,2:ncol(bootstrap_results)] <- sapply(bootstrap_results[,2:ncol(bootstrap_results)],as.numeric)


##########################################################################################################
# 2. Make Table  2 #######################################################################################
##########################################################################################################
results_table <- data.frame(tech_measure = NA, sig_measure = NA,
                            observed_gui = NA, lower_gui = NA, upper_gui = NA, p_gui = NA,
                            observed_psid = NA, lower_psid = NA, upper_psid = NA, p_psid = NA,
                            observed_mcs = NA, lower_mcs = NA, upper_mcs = NA, p_mcs = NA,
                            observed_agg = NA)
empty_rows <- data.frame(matrix(nrow = 32, ncol = ncol(results_table)))
names(empty_rows) <- names(results_table)
results_table <- rbind(results_table, empty_rows)

results_table$tech_measure <- c("sr_tech", "sr_tech", "sr_tech",
                                "tech_weekday_d", "tech_weekday_d", "tech_weekday_d", 
                                "tech_weekend_d", "tech_weekend_d", "tech_weekend_d", 
                                "tech_weekday_c", "tech_weekday_c", "tech_weekday_c",
                                "tech_weekend_c", "tech_weekend_c", "tech_weekend_c",
                                "tech_30m_wd", "tech_30m_wd", "tech_30m_wd",
                                "tech_30m_we", "tech_30m_we", "tech_30m_we",
                                "tech_1hr_wd", "tech_1hr_wd", "tech_1hr_wd", 
                                "tech_1hr_we", "tech_1hr_we", "tech_1hr_we", 
                                "tech_2hr_wd", "tech_2hr_wd", "tech_2hr_wd", 
                                "tech_2hr_we", "tech_2hr_we", "tech_2hr_we")
results_table$sig_measure <- rep(c("median ES", "share of results", "share of sig results"), 11)

##########################################################################################################
# 3. Populate Table ######################################################################################
##########################################################################################################

### this function populates the results table with the observed variables 
### (observed median point estimate, share of results or share of sig results)
populate_table_observed <- function(results_gui, results_psid, results_mcs, table){
  
  results_agg <- rbind(results_gui, results_psid, results_mcs)
  
  for(j in 1:4){
    dataset <- c("gui", "psid", "mcs", "agg")[j]
    results <- get(paste0("results_", dataset))
    medians <- results %>% group_by(tech) %>% dplyr::summarise(median = median(effect))
    signs <- results %>% group_by(tech, sign) %>% dplyr::summarise(counts = n())
    sig_signs <- results %>% group_by(tech, sig_sign) %>% dplyr::summarise(counts = n()) %>% filter(is.na(sig_sign) == FALSE)
    
  for(i in 1:nrow(table)){
    if((table[i,"sig_measure"]) == "median ES"){
      table[i, paste0("observed_", dataset)] <- medians %>% filter(tech == table[i, "tech_measure"]) %>% pull(median) %>% round(2)
      } else if ((table[i,"sig_measure"]) == "share of results") {
        table[i, paste0("observed_", dataset)] <- signs %>% filter(tech == table[i, "tech_measure"]) %>% dplyr::summarise(max = max(counts)) %>% pull(max) %>% round(1)
      } else if ((table[i,"sig_measure"]) == "share of sig results") {
        sig_results <- results %>% filter(tech == table[i, "tech_measure"]) %>% filter(is.na(sig_sign) == FALSE)
        sig_results_sign <- sig_signs %>% filter(tech == table[i, "tech_measure"]) %>% dplyr::summarise(max = max(counts)) %>% pull(max) %>% round(1)
        table[i, paste0("observed_", dataset)] <- ifelse(nrow(sig_results) == 0, 0, sig_results_sign)
      } else {
      table[i, paste0("observed_", dataset)] <- NA
    }
  }
  }
  return(table)
}


### this function populates the results table with the bootstrapped p values for the different significance tests
populate_table_bootstrap <- function(bootstrap, table){
  
  for (i in 1:nrow(table)){
    if (table[i, "tech_measure"] == "sr_tech") {
      tech <- "sr"
    } else if (table[i, "tech_measure"] == "tech_weekday_d") {
      tech <- "di.wd"
    } else if (table[i, "tech_measure"] == "tech_weekend_d") {
      tech <- "di.we"
    } else if (table[i, "tech_measure"] == "tech_weekday_c") {
      tech <- "cont.wd"
    } else if (table[i, "tech_measure"] == "tech_weekend_c") {
      tech <- "cont.we"
    } else if (table[i, "tech_measure"] == "tech_30m_wd") {
      tech <- "30.wd"
    } else if (table[i, "tech_measure"] == "tech_30m_we") {
      tech <- "30.we"
    } else if (table[i, "tech_measure"] == "tech_1hr_wd") {
      tech <- "1.wd"
    } else if (table[i, "tech_measure"] == "tech_1hr_we") {
      tech <- "1.we"
    } else if (table[i, "tech_measure"] == "tech_2hr_wd") {
      tech <- "2.wd"
    } else if (table[i, "tech_measure"] == "tech_2hr_we") {
      tech <- "2.we"
    } else if (table[i, "tech_measure"] == "tech_c") {
      tech <- "cont.total"
    }
    
    for (j in 1:3){
      dataset <- c("gui", "psid", "mcs")[j]
      
      if((table[i,"sig_measure"]) == "median ES"){
        table[i, paste0("p_", dataset)] <- bootstrap %>% filter(name == dataset) %>% pull(paste0("p1.", tech)) %>% round(2)
      } else if ((table[i,"sig_measure"]) == "share of results") {
        table[i, paste0("p_", dataset)] <- bootstrap %>% filter(name == dataset) %>% pull(paste0("p2.", tech)) %>% round(2)
      } else if ((table[i,"sig_measure"]) == "share of sig results") {
        table[i, paste0("p_", dataset)] <- bootstrap %>% filter(name == dataset) %>% pull(paste0("p3.", tech)) %>% round(2)
      } 
    }
  }
  return(table)
}

### this function populates the results table with the bootstrapped confidence intervals for the median point estimates
populate_table_bounds <- function(boot_gui, boot_psid, boot_mcs, table){
  
  for(j in 1:3){
    results_frame_medians <- data.frame("sr_tech" = NA, "tech_weekday_d" = NA, "tech_weekend_d" = NA,
                                        "tech_weekday_c" = NA, "tech_weekend_c" = NA,
                                        "tech_2hr_wd" = NA, "tech_2hr_we" = NA, 
                                        "tech_1hr_wd" = NA, "tech_1hr_we" = NA, 
                                        "tech_30m_wd" = NA, "tech_30m_we" = NA)  
    
    dataset <- c("gui", "psid", "mcs")[j]
    results <- get(paste0("boot_", dataset))

    for(b in 1:(length(results)-1)){
      medians <- results[[b]] %>% group_by(tech) %>% dplyr::summarise(median = median(effect))
      results_frame_medians <- rbind(results_frame_medians, as.numeric(t(medians)[2,]))
    }
    
    results_frame_medians <- results_frame_medians[2:nrow(results_frame_medians),]
      
    for(i in 1:nrow(table)){
      if((table[i,"sig_measure"]) == "median ES"){
        table[i, c(paste0("lower_", dataset), paste0("upper_", dataset))] <- round(quantile(results_frame_medians[, paste0(table[i, "tech_measure"])], probs = c(0.025, 0.975)), 2)
      } else {
        table[i, c(paste0("lower_", dataset), paste0("upper_", dataset))] <- NA
      }
    }
  }
  return(table)
}


results_table_complete <- populate_table_observed(results_gui = results_frame_gui, 
                                                  results_psid = results_frame_psid, 
                                                  results_mcs = results_frame_mcs, 
                                                  table = results_table)
results_table_complete <- populate_table_bootstrap(bootstrap_results, results_table_complete)
results_table_complete <- populate_table_bounds(boot_gui = boot_sca_gui,
                                                boot_psid = boot_sca_psid,
                                                boot_mcs = boot_sca_mcs,
                                                results_table_complete)

##########################################################################################################
# 4. Save Table ##########################################################################################
##########################################################################################################
write.csv(results_table_complete, file = "../data/objects/3b_table2_complete.csv")
write.csv(results_table_complete %>% filter(sig_measure != "share of results"), file = "../data/objects/3b_table2.csv")

##########################################################################################################
# 5. Make Confirmatory Table ##################################################################################
##########################################################################################################

#####################################################################################
# b) Make Table ####
#####################################################################################
results_table_confirmatory <- data.frame(tech_measure = NA, sig_measure = NA,
                            observed_mcs = NA, p_mcs = NA, rsqrd = NA, 
                            observed_mcs_lower = NA, observed_mcs_higher = NA,
                            rsqrd_lower = NA, rsqrd_higher = NA)
empty_rows <- data.frame(matrix(nrow = 11, ncol = ncol(results_table_confirmatory)))
names(empty_rows) <- names(results_table_confirmatory)
results_table_confirmatory <- rbind(results_table_confirmatory, empty_rows)

results_table_confirmatory$tech_measure <- c("sr_tech", "sr_tech", "sr_tech",
                                "tech_c", "tech_c", "tech_c",
                                "tech_30m_wd", "tech_30m_wd", "tech_30m_wd",
                                "tech_1hr_wd", "tech_1hr_wd", "tech_1hr_wd")
results_table_confirmatory$sig_measure <- rep(c("median ES", "share of results", "share of sig results"), 4)

#####################################################################################
# b) Populate Table ####
#####################################################################################

###first we add in the p values 
results_table_confirmatory$p_mcs <- populate_table_bootstrap(bootstrap_results, results_table_confirmatory)[,"p_mcs"] 

###next we merge the two continuous technology use variables as they are combined in the confirmatory testing
results_frame_mcs_confirmatory <- results_frame_mcs
results_frame_mcs_confirmatory$tech <- plyr::revalue(results_frame_mcs_confirmatory$tech, c("tech_weekday_c"="tech_c", "tech_weekend_c"="tech_c"))

### we obtain the medians, rsquareds, signs and significant signs
medians <- results_frame_mcs_confirmatory %>% group_by(tech) %>% dplyr::summarise(median = median(effect))
rsqrds <- results_frame_mcs_confirmatory %>% group_by(tech) %>% dplyr::summarise(median = median(rsqrd))
signs <- results_frame_mcs_confirmatory %>% group_by(tech, sign) %>% dplyr::summarise(counts = n())
sig_signs <- results_frame_mcs_confirmatory %>% group_by(tech, sig_sign) %>% dplyr::summarise(counts = n()) %>% filter(is.na(sig_sign) == FALSE)
  
### we populate the observed values
for(i in 1:nrow(results_table_confirmatory)){
    if((results_table_confirmatory[i,"sig_measure"]) == "median ES"){
      results_table_confirmatory[i, "observed_mcs"] <- medians %>% filter(tech == results_table_confirmatory[i, "tech_measure"]) %>% pull(median) %>% round(2)
      results_table_confirmatory[i, "rsqrd"] <- rsqrds %>% filter(tech == results_table_confirmatory[i, "tech_measure"]) %>% pull(median) %>% round(3)
    } else if ((results_table_confirmatory[i,"sig_measure"]) == "share of results") {
      results_table_confirmatory[i, "observed_mcs"] <- signs %>% filter(tech == results_table_confirmatory[i, "tech_measure"]) %>% dplyr::summarise(max = max(counts)) %>% pull(max) %>% round(1)
    } else if ((results_table_confirmatory[i,"sig_measure"]) == "share of sig results") {
      sig_results <- results_frame_mcs_confirmatory %>% filter(tech == results_table_confirmatory[i, "tech_measure"]) %>% filter(is.na(sig_sign) == FALSE)
      sig_results_sign <- sig_signs %>% filter(tech == results_table_confirmatory[i, "tech_measure"]) %>% dplyr::summarise(max = max(counts)) %>% pull(max) %>% round(1)
      results_table_confirmatory[i, "observed_mcs"] <- ifelse(nrow(sig_results) == 0, 0, sig_results_sign)
    } else {
      results_table_confirmatory[i, "observed_mcs"] <- NA
    }
}

# now we add in the bootsrapped CI 
results_frame_medians <- data.frame("sr_tech" = NA, "tech_weekday_d" = NA, "tech_weekend_d" = NA,
                                    "tech_c" = NA,
                                    "tech_2hr_wd" = NA, "tech_2hr_we" = NA, 
                                    "tech_1hr_wd" = NA, "tech_1hr_we" = NA, 
                                    "tech_30m_wd" = NA, "tech_30m_we" = NA) 
results_frame_rsq <- data.frame("sr_tech" = NA, "tech_weekday_d" = NA, "tech_weekend_d" = NA,
                                    "tech_c" = NA,
                                    "tech_2hr_wd" = NA, "tech_2hr_we" = NA, 
                                    "tech_1hr_wd" = NA, "tech_1hr_we" = NA, 
                                    "tech_30m_wd" = NA, "tech_30m_we" = NA) 

for(b in 1:(length(boot_sca_mcs)-1)){
  specific_result <- boot_sca_mcs[[b]]
  specific_result$tech <- plyr::revalue(specific_result$tech, c("tech_weekday_c"="tech_c", "tech_weekend_c"="tech_c"))
  medians <- specific_result %>% group_by(tech) %>% dplyr::summarise(median = median(effect))
  median_rsq <- specific_result %>% group_by(tech) %>% dplyr::summarise(median = median(rsqrd))
  results_frame_medians <- rbind(results_frame_medians, as.numeric(t(medians)[2,]))
  results_frame_rsq <- rbind(results_frame_rsq, as.numeric(t(median_rsq)[2,]))
}

results_frame_medians <- results_frame_medians[2:nrow(results_frame_medians),]
results_frame_medians <- results_frame_medians %>% select("sr_tech", "tech_c", "tech_1hr_wd", "tech_30m_wd")
results_frame_rsq <- results_frame_rsq[2:nrow(results_frame_rsq),]
results_frame_rsq <- results_frame_rsq %>% select("sr_tech", "tech_c", "tech_1hr_wd", "tech_30m_wd")

effects_ci <- results_frame_medians %>% dplyr::summarise(`sr_tech_l`=quantile(sr_tech, probs=0.025),
            `sr_tech_h`=quantile(sr_tech, probs=0.975), `tech_c_l`=quantile(tech_c, probs=0.025),
            `tech_c_h`=quantile(tech_c, probs=0.975), `tech_1hr_wd_l`=quantile(tech_1hr_wd, probs=0.025),
            `tech_1hr_wd_h`=quantile(tech_1hr_wd, probs=0.975), `tech_30m_wd_l`=quantile(tech_30m_wd, probs=0.025),
            `tech_30m_wd_h`=quantile(tech_30m_wd, probs=0.975))
rsqrd_ci <- results_frame_rsq %>% dplyr::summarise(`sr_tech_l`=quantile(sr_tech, probs=0.025),
                                                      `sr_tech_h`=quantile(sr_tech, probs=0.975), `tech_c_l`=quantile(tech_c, probs=0.025),
                                                      `tech_c_h`=quantile(tech_c, probs=0.975), `tech_1hr_wd_l`=quantile(tech_1hr_wd, probs=0.025),
                                                      `tech_1hr_wd_h`=quantile(tech_1hr_wd, probs=0.975), `tech_30m_wd_l`=quantile(tech_30m_wd, probs=0.025),
                                                      `tech_30m_wd_h`=quantile(tech_30m_wd, probs=0.975) )

# we now add these confidence intervals to the results table
for(i in 1:nrow(results_table_confirmatory)){
  if((results_table_confirmatory[i,"sig_measure"]) == "median ES"){
    results_table_confirmatory[i, "observed_mcs_lower"] <- effects_ci %>% pull(paste0(results_table_confirmatory[i, "tech_measure"], "_l")) %>% round(2)
    results_table_confirmatory[i, "observed_mcs_higher"] <- effects_ci %>% pull(paste0(results_table_confirmatory[i, "tech_measure"], "_h")) %>% round(2)
    results_table_confirmatory[i, "rsqrd_lower"] <- rsqrd_ci %>% pull(paste0(results_table_confirmatory[i, "tech_measure"], "_l")) %>% round(3)
    results_table_confirmatory[i, "rsqrd_higher"] <- rsqrd_ci %>% pull(paste0(results_table_confirmatory[i, "tech_measure"], "_h")) %>% round(3)
  }}

#####################################################################################
# d) Save Table ####
#####################################################################################
write.csv(results_table_confirmatory, file = "../data/objects/3b_table3_complete.csv")
write.csv(results_table_confirmatory %>% filter(sig_measure != "share of results"), file = "../data/objects/3b_table3.csv")

##########################################################################################################
# 6. Make Control Table ###################################################################################
##########################################################################################################
results_frame_mcs_wide_es <- results_frame_mcs %>% select(measure, tech, control, effect) %>% spread(control, effect)
names(results_frame_mcs_wide_es) <- c("measure", "tech", "no_cont_es", "cont_es")
results_frame_mcs_wide_rsqrd <- results_frame_mcs %>% select(measure, tech, control, rsqrd) %>% spread(control, rsqrd)
names(results_frame_mcs_wide_rsqrd) <- c("measure", "tech", "no_cont_rsqrd", "cont_rsqrd")
results_frame_mcs_wide <- left_join(results_frame_mcs_wide_es, results_frame_mcs_wide_rsqrd, by = c("measure", "tech"))

### find medians
median(results_frame_mcs_wide$no_cont_es)
median(results_frame_mcs_wide$cont_es)

#paired t test
t.test(results_frame_mcs_wide$no_cont_es, 
       results_frame_mcs_wide$cont_es, 
       paired=TRUE, 
       conf.level=0.95)

# table
results_b <- results_frame_mcs_wide %>% top_n(wt = no_cont_es, n = -5)
results_t <- results_frame_mcs_wide %>% top_n(wt = no_cont_es, n = 5)
results_extremes <- rbind(results_t, results_b)
results_extremes$perc_diff <- round((abs(abs(results_extremes$no_cont_es) - abs(results_extremes$cont_es))/(rowMeans(results_extremes[, c("no_cont_es", "cont_es")])))*100, 3)
results_extremes$no_cont_es <- round(results_extremes$no_cont_es, 3)
results_extremes$cont_es <- round(results_extremes$cont_es, 3)
results_extremes$no_cont_rsqrd <- round(results_extremes$no_cont_rsqrd, 3)
results_extremes$cont_rsqrd <- round(results_extremes$cont_rsqrd, 3)

write.csv(results_extremes, file = "../data/objects/3b_table4.csv")
