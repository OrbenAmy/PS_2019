##########################################################################################################
### Script 3b: Make Results Table ####
### This scripts takes the three completed SCAs and Bootstraps for three datasets (GUI, PSID, MCS)
### It creates a table of results for all their results and also aggregate effects.
##########################################################################################################

##########################################################################################################
# 1. Loading libraries, controlling analyses and loading data ############################################
##########################################################################################################

#####################################################################################
# a) Load libraries ####
#####################################################################################
library("plyr")
library("tidyverse")

bootstraps <- 500

#####################################################################################
# b) Load SCAs ####
#####################################################################################  
results_frame_gui <- read.csv(file="3_1_GUI_SCA_nonSEM.csv")
results_frame_psid <- read.csv(file="3_2_PSID_SCA_nonSEM.csv")
results_frame_mcs <- read.csv(file="3_3_MCS_SCA_nonSEM.csv")

boot_sca_gui <- read_rds("3_1_GUI_SCA_boot_nonSEM.rds")
boot_sca_psid <- read_rds("3_2_PSID_SCA_boot_nonSEM.rds")
boot_sca_mcs <- read_rds("3_3_MCS_SCA_boot_nonSEM.rds")

#####################################################################################
# b) Make different convenience variables ####
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
#####################################################################################
# c) Load Bootstraps ####
##################################################################################### 
bootstrap_results <- read.csv(file="3_per_results.csv")
bootstrap_results$name <- tolower(bootstrap_results$name)

#####################################################################################
# c) Extract bootstrapped median effects (SCAs) ####
##################################################################################### 
extract_median_es <- function(boot_sca){
  for (i in 1:nrow(boot_sca)){
    median_effect <- median(sca_gui[[1]]$effect)
  }
}

##########################################################################################################
# 2. Make Table ##########################################################################################
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
write.csv(results_table_complete, file = "3_results_table_complete.csv")
write.csv(results_table_complete %>% filter(sig_measure != "share of results"), file = "3_results_table.csv")

##########################################################################################################
# 5. Make Confirmatory Table ##################################################################################
##########################################################################################################

#####################################################################################
# b) Make Table ####
#####################################################################################
results_table_confirmatory <- data.frame(tech_measure = NA, sig_measure = NA,
                            observed_mcs = NA, p_mcs = NA, rsqrd = NA)
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
results_table_confirmatory <- populate_table_bootstrap(bootstrap_results, results_table_confirmatory)

results_frame_mcs_confirmatory <- results_frame_mcs
results_frame_mcs_confirmatory$tech <- plyr::revalue(results_frame_mcs_confirmatory$tech, c("tech_weekday_c"="tech_c", "tech_weekend_c"="tech_c"))

medians <- results_frame_mcs_confirmatory %>% group_by(tech) %>% dplyr::summarise(median = median(effect))
rsqrds <- results_frame_mcs_confirmatory %>% group_by(tech) %>% dplyr::summarise(median = median(rsqrd))
signs <- results_frame_mcs_confirmatory %>% group_by(tech, sign) %>% dplyr::summarise(counts = n())
sig_signs <- results_frame_mcs_confirmatory %>% group_by(tech, sig_sign) %>% dplyr::summarise(counts = n()) %>% filter(is.na(sig_sign) == FALSE)
  
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

# bootsrapped CI 
bootstrap_sca_mcs <- read_rds("3_3_MCS_SCA_boot_nonSEM.rds")

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

for(b in 1:(length(bootstrap_sca_mcs)-1)){
  specific_result <- bootstrap_sca_mcs[[b]]
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

effects_ci <- results_frame_medians %>% dplyr::summarise(`effect_lower_sr`=quantile(sr_tech, probs=0.025),
            `effect_higher_sr`=quantile(sr_tech, probs=0.975), `effect_lower_c`=quantile(tech_c, probs=0.025),
            `effect_higher_c`=quantile(tech_c, probs=0.975), `effect_lower_1hr`=quantile(tech_1hr_wd, probs=0.025),
            `effect_higher_1hr`=quantile(tech_1hr_wd, probs=0.975), `effect_lower_30m`=quantile(tech_30m_wd, probs=0.025),
            `effect_higher_30m`=quantile(tech_30m_wd, probs=0.975))
rsqrd_ci <- results_frame_rsq %>% dplyr::summarise(`rsqd_lower_sr`=quantile(sr_tech, probs=0.025),
                                                      `rsqd_higher_sr`=quantile(sr_tech, probs=0.975), `rsqd_lower_c`=quantile(tech_c, probs=0.025),
                                                      `rsqd_higher_c`=quantile(tech_c, probs=0.975), `rsqd_lower_1hr`=quantile(tech_1hr_wd, probs=0.025),
                                                      `rsqd_higher_1hr`=quantile(tech_1hr_wd, probs=0.975), `rsqd_lower_30m`=quantile(tech_30m_wd, probs=0.025),
                                                      `rsqd_higher_30m`=quantile(tech_30m_wd, probs=0.975) )
ci <- c(effects_ci, rsqrd_ci)

# Join tables
results_table_confirmatory <- results_table_confirmatory %>% select(-c("p_gui", "p_psid"))

#####################################################################################
# d) Save Table ####
#####################################################################################
write.csv(ci, file = "3_results_table_confirmatory_confidenceintervals.csv")
write.csv(results_table_confirmatory, file = "3_results_table_confirmatory_complete.csv")
write.csv(results_table_confirmatory %>% filter(sig_measure != "share of results"), file = "3_results_table_confirmatory.csv")

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

write.csv(results_extremes, file = "3_results_table_controls.csv")
