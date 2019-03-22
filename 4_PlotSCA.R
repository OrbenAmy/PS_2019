##########################################################################################################
### Script 4: Plot SCAs ####
### This scripts takes three datasets (GUI, PSID, MCS)
### and plots the SCAS (plot 1), furthermore it plots the SCA for controls/no controls
##########################################################################################################

##########################################################################################################
# 1. Loading libraries, data and plots ###################################################################
##########################################################################################################

#####################################################################################
# a) Load libraries ####
#####################################################################################
library("tidyverse")
library("foreign")
library("gridExtra")
library("viridis")
library("ggalt")

#####################################################################################
# b) Load data ####
#####################################################################################
sca_gui_boot <- read.csv(file="3_1_GUI_SCA_bootstrapped.csv")
sca_psid_boot <- read.csv(file="3_2_PSID_SCA_bootstrapped.csv")
sca_mcs_boot <- read.csv(file="3_3_MCS_SCA_bootstrapped.csv")

results_frame_gui <- read.csv(file="3_1_GUI_SCA_nonSEM.csv")
results_frame_psid <- read.csv(file="3_2_PSID_SCA_nonSEM.csv")
results_frame_mcs <- read.csv(file="3_3_MCS_SCA_nonSEM.csv")

sca_gui <- left_join(results_frame_gui, sca_gui_boot[, c("measure", "tech", "control","effect_lower", "effect_upper", "rsqrd_lower", "rsqrd_upper")], by = c("measure", "tech", "control"))
sca_psid <- left_join(results_frame_psid, sca_psid_boot[, c("measure", "tech", "control","effect_lower", "effect_upper", "rsqrd_lower", "rsqrd_upper")], by = c("measure", "tech", "control"))
sca_mcs <- left_join(results_frame_mcs, sca_mcs_boot[, c("measure", "tech", "control","effect_lower", "effect_upper", "rsqrd_lower", "rsqrd_upper")], by = c("measure", "tech", "control"))

#####################################################################################
# c) Setup plots and merge data ####
#####################################################################################
theme_set(theme_classic())

temp_data_gui <- sca_gui
temp_data_psid <- sca_psid
temp_data_mcs <- sca_mcs

temp_data_gui$dataset <- rep("GUI", nrow(temp_data_gui))
temp_data_psid$dataset <- rep("PSID", nrow(temp_data_psid))
temp_data_mcs$dataset <- rep("MCS", nrow(temp_data_mcs))

temp_data_gui <- temp_data_gui[order(temp_data_gui$effect),]
temp_data_gui$index[!is.na(temp_data_gui$effect)] <-
  1:nrow(temp_data_gui[!is.na(temp_data_gui$effect),])
temp_data_psid <- temp_data_psid[order(temp_data_psid$effect),]
temp_data_psid$index[!is.na(temp_data_psid$effect)] <-
  1:nrow(temp_data_psid[!is.na(temp_data_psid$effect),])
temp_data_mcs <- temp_data_mcs[order(temp_data_mcs$effect),]
temp_data_mcs$index[!is.na(temp_data_mcs$effect)] <-
  1:nrow(temp_data_mcs[!is.na(temp_data_mcs$effect),])

temp_data_total <- rbind(temp_data_gui, temp_data_psid, temp_data_mcs)
temp_data_total$dataset <- factor(temp_data_total$dataset, levels = c("GUI", "PSID", "MCS"), ordered = TRUE)

#####################################################################################
# d) Sort by effect sizes ####
#####################################################################################
#temp_data_total <- temp_data_total[order(temp_data_total$effect),]
#temp_data_total$index[!is.na(temp_data_total$effect)] <-
#  1:nrow(temp_data_total[!is.na(temp_data_total$effect),])
temp_data_total$sig <- "0"
temp_data_total$sig[temp_data_total$p < .05] <- "1"

##########################################################################################################
# 2. Setup Functions #####################################################################################
##########################################################################################################

#####################################################################################
# a) Making specification curves ####
#####################################################################################
get_curve <- function(data) {
  
  plot <- ggplot(data, aes(x = 1:nrow(data))) +
    geom_ribbon(aes(ymin = effect_lower, ymax = effect_upper), fill = "grey90") +
    geom_point(aes(y = effect, color = sig), size = 1) +
    #geom_hline(yintercept = 0) +
    scale_color_manual(values = c("#FF0000", "#000000")) +
    scale_y_continuous(name = "Standardized Regression Coefficient") +
    facet_grid(. ~ dataset, scales = "free", space = "free") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      strip.background = element_rect(colour="white", fill="white")
    )
  return(plot)
}

 #####################################################################################
# b) make data frame to then make scatterplot 'dashboard'
#####################################################################################
get_scatter_frame <- function(results_frame) {
  
  ###############################
  # i) Setup data frame
  ###############################
  vars_wellbeing <- c("selfesteem", "wellbeing", "sdq")
  names_wellbeing <- c("Self-Esteem", "Well-Being", "Strength and Difficulties")
  
  vars_tech <- c("sr_tech", "tech_weekday_d", "tech_weekend_d",
                 "tech_weekday_c", "tech_weekend_c",
                 "tech_30m_wd", "tech_30m_we",
                 "tech_1hr_wd", "tech_1hr_we",
                 "tech_2hr_wd", "tech_2hr_we"
  )
  names_tech <- c("Retrospective Self-Report", "Participation Weekday", "Participation Weekend",
                  "Time Spent Weekday", "Time Spent Weekend",
                  "<30 min Weekday", "<30 min Weekend",
                  "<1 hour Weekday", "<1 hour Weekend",
                  "<2 hour Weekday", "<2 hour Weekend"
  )
  
  vars_control <- c(1,0)
  names_control <- c("Controls", "No Controls")
  
  axis_names <-
    c("Well-Being", "Technology Use", "controls")
  
  variables <- c(vars_wellbeing, vars_tech, vars_control)
  
  ###############################
  # ii) Sort by effect sizes
  ###############################
  results_frame <- results_frame[order(results_frame$effect),]
  results_frame$index[!is.na(results_frame$effect)] <-
    1:nrow(results_frame[!is.na(results_frame$effect),])
  results_frame$sig <- "0"
  results_frame$sig[results_frame$p < .05] <- "1"
  
  dot_data <-
    as.data.frame(array(0, dim = c(nrow(results_frame), (length(variables)))))

  ###############################
  # iii) Fill results frame
  ############################### 
  for (i in 1:nrow(results_frame)) {
    factors_x <- as.character(results_frame[[i, "measure"]])
    factors_y <- as.character(results_frame[[i, "tech"]])
    factors_c <- as.character(results_frame[[i, "control"]])
    for (l in 1:(length(variables))) {
      if ((identical(factors_x, variables[l]) == TRUE) |
          (identical(factors_y, variables[l]) == TRUE) |
          (identical(factors_c, variables[l]) == TRUE) == TRUE)  {
        dot_data[i, l] <- results_frame[[i, "index"]]
      } else {
        dot_data[i, l] <- NA
      }
    }
  }
  
  names(dot_data) <- c(names_wellbeing, names_tech, names_control)
  
  dot_data_long <-
    gather(dot_data, vars, vars_score, 1:ncol(dot_data))
  dot_data_long <- dot_data_long[complete.cases(dot_data_long),]
  dot_data_long$grouping <- 1
  
  for (i in 1:nrow(dot_data_long)) {
    dot_data_long[i, 3] <-
      ifelse((any(dot_data_long[i, 1] == names_wellbeing) == TRUE), axis_names[1],
             ifelse((any(dot_data_long[i, 1] == names_tech) == TRUE), axis_names[2], axis_names[3]))
  }
  
  dd <- as.data.frame(colMeans(dot_data, na.rm = TRUE))
  dd <- rownames_to_column(dd, var = "rowname")
  names(dd) <- c("rowname", "mean")
  dd$grouping <- 1
  for (i in 1:nrow(dd)) {
    dd[i, 3] <-
      ifelse((any(dd[i, 1] == names_wellbeing) == TRUE), axis_names[1],
             ifelse((any(dd[i, 1] == names_tech) == TRUE), axis_names[2], axis_names[3]))
  }
  dd <- dd %>% dplyr::arrange(mean) %>% dplyr::arrange(grouping)
  
  dot_data_long$grouping_or <-
    factor(dot_data_long$grouping,
           levels = axis_names,
           ordered = TRUE)
  dot_data_long$vars_or <-
    factor(dot_data_long$vars,
           ordered = TRUE,
           levels = dd$rowname)
  
  index_data <- results_frame[, c("index", "sig")]
  colnames(index_data)[1] <- "vars_score"
  dot_data_long <-
    dplyr::left_join(dot_data_long, index_data, by = "vars_score")
  
  return(dot_data_long)
}

##########################################################################################################
# 3. Run main curves #####################################################################################
##########################################################################################################
### Make curve
plot_total <- get_curve(temp_data_total)
ggsave(file="4_sca_nonsem_1.pdf", plot_total, width = 12, height = 4)

### Make dataset to scatterplot
frame_gui <- get_scatter_frame(temp_data_gui)
frame_psid <- get_scatter_frame(temp_data_psid)
frame_mcs <- get_scatter_frame(temp_data_mcs)

frame_gui$dataset <- rep("GUI", nrow(frame_gui))
frame_psid$dataset <- rep("PSID", nrow(frame_psid))
frame_mcs$dataset <- rep("MCS", nrow(frame_mcs))

frame_total <- rbind(frame_gui, frame_psid, frame_mcs)
frame_total$dataset <- factor(frame_total$dataset, levels = c("GUI", "PSID", "MCS"), ordered = TRUE)
frame_total$vars_or <- factor(frame_total$vars_or, levels = c("Strength and Difficulties", "Well-Being","Self-Esteem", 
                                                              "Participation Weekend", "Participation Weekday",
                                                              "<2 hour Weekend", "<2 hour Weekday",
                                                              "<1 hour Weekend", "<1 hour Weekday",
                                                              "<30 min Weekend", "<30 min Weekday",
                                                              "Time Spent Weekend", "Time Spent Weekday",
                                                              "Retrospective Self-Report", 
                                                              "Controls", "No Controls"))
frame_total$vars_or <- factor(frame_total$vars_or, levels=rev(levels(frame_total$vars_or))) #reverse

### Make scatterplot
scatter_total <-
  ggplot(data = frame_total, aes(x = vars_or, y = vars_score, color = sig)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("#FF0000", "#000000")) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60)) +
  coord_flip() +
  facet_grid(grouping_or ~ dataset, scales = "free", space = "free") +
  labs(y = "Specification Rank", x = "Variables") +
  theme(
    legend.position = "none",
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_rect(colour="white", fill="white")
  )
ggsave(file="4_sca_nonsem_2.pdf", scatter_total, width = 12, height = 4)

### Put curve and scatterplot together
plots <- list(plot_total, scatter_total)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}
g <- do.call("grid.arrange", c(grobs, ncol = 1))

### Save
ggsave(file="4_sca_nonsem.pdf", g, width = 12, height = 8)

##########################################################################################################
# 4. Run control curves ##################################################################################
##########################################################################################################

#####################################################################################
# a) Sort and subset data
#####################################################################################
temp_data_mcs <- temp_data_mcs[order(temp_data_mcs$effect),]
temp_data_mcs$index[!is.na(temp_data_mcs$effect)] <-
  1:nrow(temp_data_mcs[!is.na(temp_data_mcs$effect),])
temp_data_mcs$sig <- "0"
temp_data_mcs$sig[temp_data_mcs$p < .05] <- "1"

median_effect_all <- median(temp_data_mcs$effect, na.rm = TRUE)
median_p_all <- median(temp_data_mcs$p_value, na.rm = TRUE)
colours <-
  viridis(
    7,
    alpha = 1,
    begin = 0,
    end = 0.9,
    direction = -1
  )
colours_lite <-
  viridis(
    7,
    alpha = 0.008,
    begin = 0,
    end = 0.9,
    direction = -1
  )

temp_data_mcs$lb <- temp_data_mcs$effect - temp_data_mcs$standard_error
temp_data_mcs$ub <- temp_data_mcs$effect + temp_data_mcs$standard_error

temp_data_c <- temp_data_mcs %>% filter(control == 0)
temp_data_c$index2[!is.na(temp_data_c$effect)] <-
  1:nrow(temp_data_c[!is.na(temp_data_c$effect), ])
temp_data_c$col <-
  ifelse(temp_data_c$sig == 0, colours[4], colours_lite[4])
median_effect_c <- median(temp_data_c$effect, na.rm = TRUE)
median_p_c <- median(temp_data_c$p_value, na.rm = TRUE)

temp_data_nc <- temp_data_mcs %>% filter(control == 1)
temp_data_nc$index2[!is.na(temp_data_nc$effect)] <-
  1:nrow(temp_data_nc[!is.na(temp_data_nc$effect), ])
temp_data_nc$col <-
  ifelse(temp_data_nc$sig == 0, colours[4], colours_lite[4])
median_effect_nc <- median(temp_data_nc$effect, na.rm = TRUE)
median_p_nc <- median(temp_data_nc$p_value, na.rm = TRUE)

h1 <- round(median_effect_c, digits = 3)
h2 <- round(median_effect_nc, digits = 3)

mean_effect_nc <- mean(temp_data_nc$effect, na.rm = TRUE)
mean_effect_c <- mean(temp_data_c$effect, na.rm = TRUE)

#####################################################################################
# b) Plot data ####
#####################################################################################
figure_2 <- ggplot() +
  geom_ribbon(data = temp_data_c, aes(ymin = lb, ymax = ub, x = index2),
              colour = NA, fill = colours[7],alpha = 0.3) +
  geom_point(data = temp_data_c, aes(y = effect, x = index2, colour = colours[7])) +
  geom_ribbon(data = temp_data_nc, aes(ymin = lb, ymax = ub, x = index2),
              colour = NA, fill = colours[4], alpha = 0.3) +
  geom_point(data = temp_data_nc, aes(y = effect, x = index2, colour = colours[4])) +
  ggtitle("MCS Specification Curve Analysis") +
  labs(x = "Specification Rank", y = "Standardized Regression Coefficient") +
  annotate("text",x =15, y = c(-0.1,-0.12),
    label = c(paste0("Median Effect: Controls = ", h1), paste0("Median Effect: No Controls = ", h2)),
    size = 3.5, colour = c(colours[4], colours[7]), hjust = 0) +
  scale_color_identity()

ggsave(
  filename = "4_sca_controls_nonsem.pdf",
  width = 5,
  height = 5)
