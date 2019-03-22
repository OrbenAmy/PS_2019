##########################################################################################################
### Script 1: Cleaning Data ####
### This scripts takes three datasets (GUI, PSID, MCS)
### cleans them, makes necessary variables,
### saves them for further analyses 
##########################################################################################################

#####################################################################################
# Specify windows or mac ####
#####################################################################################
analysis <- 0 # change to 1 to add time recoding analyses 

#####################################################################################
# Load libraries ####
#####################################################################################
library("tidyverse")
library("foreign")
library("DescTools")

#####################################################################################
# GUI ####
#####################################################################################

#####################################################
# Load questionnaire and time use datasets
#####################################################

data_quest <- read.spss("../data/raw/gui/GUI_ChildCohortWave2/GUI_ChildCohortWave2_Data/13 year cohort data/GUI Data_ChildCohortWave2.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
names(data_quest) <- tolower(names(data_quest))

data_time <- read.spss("../data/raw/gui/GUI_ChildCohortWave2/GUI_ChildCohortWave2_Data/Time Use Data/GUI Data_ChildCohortWave2_TimeUse.sav",
                       use.value.labels = FALSE,
                       to.data.frame = TRUE,
                       use.missings = TRUE)
names(data_time) <- tolower(names(data_time))

#######################################################
# Merge the datasets of interest
# only keeping ppts that filled out time use
#######################################################
data_merge <- left_join(data_time, data_quest, by = "id")  

# Remove sub-questionnaires
rm(data_quest)
rm(data_time)

#######################################################
# Recode sex and age and weekday
#######################################################
#recode 1 = male, 2 = female to 1 = male, 0 = female
data_merge <- data_merge %>% mutate(sex = recode(p2sexw2,`1` = 1, `2` = 0))
print(paste0("In GUI there are ", table(data_merge$sex)[[1]], " girls and ", table(data_merge$sex)[[2]], " boys"))

#rename age variable for child p2agew2 to age
data_merge$age <- data_merge$p2agew2
print(paste0("In GUI there are ", table(data_merge$age)[[1]], " 12 year-olds and ", 
             table(data_merge$age)[[2]], " 13 year-olds and ",
             table(data_merge$age)[[3]], " 14 year-olds"))

#recode weekday variable to weekday (1) if it is a weekday (bweekend = 1), term time (btwerm = 1) and a school day (bt2_1 = 1)
#recode weekday variable to weekend (0) if it is a weekend (bweekend = 2)
#recode weekday variable to NA (NA) if there is an NA variable or it is a non-term-time or non-school-day weekday
print(paste0("Before removing those adolescents who filled out a time diary during the week but not during term/school time we had ", nrow(data_merge)[], " participants."))

#remove those that score NA on weekday/weekend
data_merge <- data_merge %>% mutate(wd = ifelse(bweekend == 1 & bterm == 1 & bt2_1 == 1, 1, ifelse(bweekend == 2, 0, NA))) %>% filter(is.na(wd) == FALSE)
print(paste0("After removing those adolescents who filled out a time diary during the week but not during term/school time we have ", nrow(data_merge)[], " participants."))


#######################################################
# Create technology use variales
#######################################################

#######################################
### Time Use Diary: Total Time #####
#######################################
# the time use diary entries are pre-coded by research assistants and there are four codes for technology use: 
# internet and emailing, playing electronic games, talking on the phone and texting and watching TV/videos/DVDs
# we aggregate these times together into one general measure.

tech_values <- c(11, #internet and emailing
                 12, #playing electronic games
                 13, #talking on phone and texting
                 15) #watching TV/Video/DVDs

# create time variables: these creates the variables included in thies study automatically (as there are many!)
# there are 288 time variables as there are 24 hours in the day * 4 15-minute time slots * 3 activities per time slot
time_vars_1 <- sort(as.vector(outer(paste0("bt", sprintf("%02d", 0:11), "_"), paste0(1:4, "_a"), paste, sep="")))
time_vars_a <- sort(as.vector(outer(time_vars_1, c("1", "2", "3"), paste, sep = "")))

time_vars_1 <- sort(as.vector(outer(paste0("bt", sprintf("%02d", 1:12), "_"), paste0(1:4, "_p"), paste, sep="")))
time_vars_b <- sort(as.vector(outer(time_vars_1, c("1", "2", "3"), paste, sep = "")))

# put the 12 variable in the middle as it signifies midday, not midnight  
time_vars_c <- time_vars_b[133:144]
time_vars_d <- time_vars_b[-c(133:144)]
time_vars <- c(time_vars_a, time_vars_c, time_vars_d) #this is the order of variables present in the time use dataset 

#remove unnecessary variables
rm(time_vars_1, time_vars_a, time_vars_b, time_vars_c, time_vars_d)

# add up technology use over the day. As there are three time slots for each activity this could be over 24 hours a day. 
# this needs to be taken into account when examining the results of this study.
tech_times <- data_merge %>% select(time_vars)
data_merge$tech <- apply(tech_times, MARGIN = 1, function(x) sum(x %in% tech_values))*0.25 

#######################################
### Time Use Diary: BedTime #####
#######################################
# tally tech use before bedtime
# to do this we first make empty colums to add numbers to later
data_merge$bedtime <- rep(NA, nrow(data_merge))
data_merge$tech_30m <- rep(NA, nrow(data_merge))
data_merge$tech_1hr <- rep(NA, nrow(data_merge))
data_merge$tech_2hr <- rep(NA, nrow(data_merge))

for (i in 1:nrow(data_merge)){
  
  #as sleeping is noted as 1 we first examine when they sleep
  sleep_time <- which(tech_times[i,] == 1)
  
  for (n in 2:(length(sleep_time)-1)) {
    if (sleep_time[n+1] - sleep_time[n] > 4){ # look at the last point in day where sleep is interrupted by another activity
      sleep_onset <- sleep_time[n+1] # print the column when they go to bed (start sleeping), you can get the time by plugging number into names(tech_times)[n]
      sleep_onset <- RoundTo(sleep_onset, multiple = 3, floor) + 1 #round as we always want sleeping as the first activity
      } 
  }
  
  if (sleep_onset < 193) { #if their last point in day where they fall asleep is before 4pm we code them as having a bedtime at midnight
    sleep_onset == 286
  } else {}
  
  #make variables
  data_merge[i, "bedtime"] <- sleep_onset #the sleep onset is therefore the point in the diary where sleep appears (the distance is three between each bedtime)
  data_merge[i, "tech_30m"] <- ifelse(any(tech_times[i, (sleep_onset-6):(sleep_onset-1)] %in% tech_values), 1, 0)
  data_merge[i, "tech_1hr"] <- ifelse(any(tech_times[i, (sleep_onset-12):(sleep_onset-1)] %in% tech_values), 1, 0)
  data_merge[i, "tech_2hr"] <- ifelse(any(tech_times[i, (sleep_onset-24):(sleep_onset-1)] %in% tech_values), 1, 0)
}

#######################################
### Time Use Diary: Recoding Variables #####
#######################################

#we can first look at a histogram of the technology use
#hist(data_merge$tech, main = "Histogram of time-diary tech use", xlab = "hours")

# recode into dichotomous (techdi = 0, no tech use, tech_di = 1, some tech use)
data_merge$tech_di <- ifelse(data_merge$tech == 0, 0, ifelse(is.na(data_merge$tech) == TRUE, NA, 1))

# recode into continuous (NA those who didnt fill out and those who didnt uuse tech)
data_merge$tech_cont <- data_merge$tech
is.na(data_merge[, c("tech_cont")]) <- 
  data_merge[, c("tech_cont")] == 0

# recode into weekend and weekday
data_merge$tech_di_wd <- ifelse(data_merge$wd == 1, data_merge$tech_di, NA)
data_merge$tech_di_we <- ifelse(data_merge$wd == 0, data_merge$tech_di, NA)

data_merge$tech_cont_wd <- ifelse(data_merge$wd == 1, data_merge$tech_cont, NA)
data_merge$tech_cont_we <- ifelse(data_merge$wd == 0, data_merge$tech_cont, NA)

data_merge$tech_30m_wd <- ifelse(data_merge$wd == 1, data_merge$tech_30m, NA)
data_merge$tech_30m_we <- ifelse(data_merge$wd == 0, data_merge$tech_30m, NA)

data_merge$tech_1hr_wd <- ifelse(data_merge$wd == 1, data_merge$tech_1hr, NA)
data_merge$tech_1hr_we <- ifelse(data_merge$wd == 0, data_merge$tech_1hr, NA)

data_merge$tech_2hr_wd <- ifelse(data_merge$wd == 1, data_merge$tech_2hr, NA)
data_merge$tech_2hr_we <- ifelse(data_merge$wd == 0, data_merge$tech_2hr, NA)

########################
### Retrospective ######
########################
vars_tech <- c("cq2q13", #Watch television, videos or DVDs (minutes)
               "cq2q15", #Using the computer (minutes)
               "cq2q16") #Playing video games such as Playstation, X-box, Nintendo, etc.? (minutes)
data_merge$sr_tech <- rowMeans(subset(data_merge, select = vars_tech), na.rm = TRUE)

#we can make a histogram of this
#hist(data_merge$sr_tech, main = "Histogram of Self-reported technology use", xlab = "self-report tech use")

########################
### Correlate ######
########################
cor.test(data_merge$sr_tech, data_merge$tech)
print(paste0("The correlation between general technology use (time use diaries) and self-reported technology use is ", 
             round(cor.test(data_merge$sr_tech, data_merge$tech)$estimate, digits = 3)))

#######################################################
# Create well-being variables
#######################################################

########################
### SDQ           ######
########################
# reverse SDQ measures so that higher means more wellbeing
# we do not work with prosocial as this is a postively worded scale
data_merge$sdqtot <- 40 - data_merge$w2pcd2_sdqtot #as sdqtot is sum of emot, cond, hyper and peer
data_merge$sdqemot <- 10 - data_merge$w2pcd2_sdqemot
data_merge$sdqcond <- 10 - data_merge$w2pcd2_sdqcond
data_merge$sdqhyper <- 10 - data_merge$w2pcd2_sdqhyper
data_merge$sdqpeer <- 10 - data_merge$w2pcd2_sdqpeer
#hist(data_merge$sdqtot, main = "Histogram of SDQ total", xlab = "SDQ total")

########################
### Wellbeing    ######
########################
# Short Mood and Feelings Questionnaire (13 items, 0-2)
# reverse depression measures so that higher means more wellbeing
data_merge$wellbeing_measure <- 26 - data_merge$w2depression_c
#hist(data_merge$wellbeing_measure, main = "Histogram of Wellbeing", xlab = "Wellbeing")

#######################################################
# Save dataset
#######################################################
write.csv(data_merge,file="../data/cleaned/1_1_GUI_dataset.csv")

rm(data_merge)
rm(tech_times)
rm(sleep_onset)
rm(sleep_time)
rm(tech_values)
rm(time_vars)
rm(vars_tech)
rm(i)
rm(n)

#####################################################################################
# PSID ####
#####################################################################################

#####################################################
# Load questionnaire and time use datasets
#####################################################
datasets <- c("ASSESS", "CHILD", "DEMOG", "HHROSTER", "IDMAP", "PCGCHILD", "PCGHH", "TD_ACT", "TD_ACTAGG", "TD_FOLLOW", "SEX")
year <- "14"

for (i in 1:length(datasets)){
  assign(paste("data", datasets[i], year, sep = "_"), 
         read.spss(paste0("../data/raw/psid/Child Development Supplement/2014/Data/", datasets[i], year, ".sav"),
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE))
}

#######################################################
# Merge the datasets of interest (questionnaires)
#######################################################
data_TD_ACTAGG_14 <- data_TD_ACTAGG_14 %>% mutate(id = paste(AGGRID14, AGGRSN14, sep = "_"))
data_CHILD_14 <- data_CHILD_14 %>% mutate(id = paste(C14YRID, C14CYPSN, sep = "_")) 
data_PCGCHILD_14 <- data_PCGCHILD_14 %>% mutate(id = paste(P14YRID, P14CYPSN, sep = "_")) 
data_ASSESS_14 <- data_ASSESS_14 %>% mutate(id = paste(A14YRID, A14CYPSN, sep = "_")) 

data_merge <- left_join(data_CHILD_14, data_TD_ACTAGG_14, by = "id") %>%
  left_join(., data_PCGCHILD_14, by = "id") %>%
  left_join(., data_ASSESS_14, by = "id")

is.na(data_merge[, ]) = data_merge[, ] == 99999 #define NAs 

#######################################################
# Merge sex dataset, which is separate
#######################################################
# For this we need to use the IDMAP data
data_IDMAP_14 <- data_IDMAP_14 %>% mutate(id = paste(CHLDID14, CHLDSN14, sep = "_")) %>% mutate(internal_id = paste(CDSHID14, CHLDINST14, sep = "_"))
data_SEX_14 <- data_SEX_14 %>% mutate(internal_id = paste(R14CDSHID, R14INST, sep = "_"))

data_merge_2 <- left_join(data_IDMAP_14, data_SEX_14, by = "internal_id") 

data_merge <- left_join(data_merge, data_merge_2, by = "id")
data_merge$sex <- data_merge$R14CS3SEX
data_merge <- data_merge %>% mutate_at(vars(sex), funs(recode(.,`1` = 1, `2` = 0))) #recode male = 1 and female = 0
print(paste0("In PSID there are ", table(data_merge$sex)[[1]], " girls and ", table(data_merge$sex)[[2]], " boys"))

## remove datasets
rm(data_merge_2)

#######################################################
# Recode age
#######################################################
data_merge$age <- data_merge$P14CHAGE #parent reoported child age (not child report)
print(paste0("In PSID there are ", table(data_merge$age)[[1]], " 8 year-olds, ", 
             table(data_merge$age)[[2]], " 9 year-olds, ",
             table(data_merge$age)[[3]], " 10 year-olds, ",
             table(data_merge$age)[[4]], " 11 year-olds, ",
             table(data_merge$age)[[5]], " 12 year-olds, ",
             table(data_merge$age)[[6]], " 13 year-olds, ",
             table(data_merge$age)[[7]], " 14 year-olds, ",
             table(data_merge$age)[[8]], " 15 year-olds, ",
             table(data_merge$age)[[9]], " 16 year-olds, ",
             table(data_merge$age)[[10]], " 17 year-olds."))

data_merge <- data_merge %>% filter(age > 11 & age < 16)
print(paste0("In PSID we only select ", table(data_merge$age)[[1]], " 12 year-olds, ", 
             table(data_merge$age)[[2]], " 13 year-olds, ",
             table(data_merge$age)[[3]], " 14 year-olds, ",
             table(data_merge$age)[[4]], " 15 year-olds, "))
print(paste0("In total PSID has ", nrow(data_merge), " participants."))

#######################################################
# Create technology use variales
#######################################################

########################
### Time Use Diary #####
########################
# Make a results frame for the time use measures
names(data_TD_ACT_14)[1:9] <- c("release", "family_id", "ind_id", "dow", "code_1", "start_1", "stop_1", "place_1", "code_2")
time_data <- data_TD_ACT_14 %>% 
  select(release, family_id, ind_id, dow, code_1, start_1, stop_1, place_1, code_2) %>% 
  mutate(id = paste(family_id, ind_id, sep = "_")) 

# Delete participant who did not fill out properly, they only add three or less activities
print(paste0("Before removing those adolescents who included less than 3 activities in their time use diaries we had ", length(unique(time_data$id))[], " participants."))
time_data <- time_data %>% group_by(id,dow) %>% filter(n() > 2)
print(paste0("After removing those adolescents who included less than 3 activities in their time use diaries we had ", length(unique(time_data$id))[], " participants."))

# Make a data frame
time_frame <-
  data.frame(matrix(NA, nrow = as.numeric(length(unique(time_data$id))), ncol = 9))
names(time_frame) <-
  c(
    "id",
    "tech_30m_wd",
    "tech_1hr_wd",
    "tech_2hr_wd",
    "tech_30m_we",
    "tech_1hr_we",
    "tech_2hr_we",
    "tech_total_wd",
    "tech_total_we"
  )
time_frame$id <- unique(time_data$id)

# Examine how many participants go to sleep after midnight (end of time use diary), which we find to be 279 participants
sleep_midnight <- time_data %>% filter(stop_1 == 86400) %>% tally((code_1 == 4590))
print(paste0("There were ", table(sleep_midnight$n)[1], " participants who went to bed after midnight."))

# Recode time use diaries
tech_vars <- c("5010", #lessons in using a computer
               "5010", #lessions in using other electonic device
               "5020", #playing electronic games
               "5021", #playing games on a cellphone or smartphone
               "5030", #other recreational computer or electronic activities
               "5031", #other recreational smartphone activities
               "5040", #using the computer or smartphone for homework
               "5050", #communication using a computer or other electronic device (facetime and skype)
               "5051", #online social meida based communication
               "5052", #tecting using a phone or tablet
               "5053", #creating or uploading content to internet
               "5060", #work for pay at home using computer
               "5070", #financial services on any device 
               "5080", #shopping on any device
               "5100", #using computer for media etc. 
               "5101", #media and newspaper on smartphone
               "5110", #library functions computer
               "5111", #library functions smartphone
               "5120", #computer work non specific
               "5121", #computer work installing software
               "5122", #computer work installing hardware
               "5123", #computer photographic processing
               "5130", #other activity with computer or electronic device
               "5390", #computer related travel
               "9190") #watching a show video or movie

if (analysis == 1) {
  for (i in 1:nrow(time_frame)){
    print(i)
    
    ### weekend ### 
    # filter data for participant and weekend day
    trial_data <- time_data %>% filter(id == time_frame$id[i] & (dow == 1|dow ==7))
    
    if (nrow(trial_data) == 0){
      #dont do anything if there is actually no data 
    } else {
      
      #calculate tech total
      trial_data_tech <- trial_data %>% 
        filter(code_1 %in% tech_vars| code_2 %in% tech_vars) %>% 
        mutate(time_total = stop_1 - start_1) 
      time_frame[i,"tech_total_we"] <- colSums(trial_data_tech[,"time_total"])
      
    # examine activity done last in the day
    last_act <- trial_data[which(trial_data$stop_1 == 86400), "code_1"]
    
    if (last_act == 4590){ #if participant was sleeping before midnight
      # extract when participant went to sleep
      sleeptime <- as.numeric(trial_data[which(trial_data$stop_1 == 86400) + c(-1), "stop_1"])
    } else {
      sleeptime == 86400 #if participant was not sleeping before midnight, we set their bedtime to midnight
    }
   
    trial_data_30 <- trial_data %>% # filter data for activities done prev 30 min
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60*0.5 & time_2 != 0)
    
    trial_data_1 <- trial_data %>%  # filter data for activities done prev 1 hr
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60 & time_2 != 0)
    
    trial_data_2 <- trial_data %>%  # filter data for activities done prev 2 hr
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60*2 & time_2 != 0)
    
    # add values to dataframe
    if (any(trial_data_30$code_1 %in% tech_vars| trial_data_30$code_2 %in% tech_vars)){
      time_frame[i,"tech_30m_we"] <- 1
    } else {
      time_frame[i,"tech_30m_we"] <- 0
    }
    
    if (any(trial_data_1$code_1 %in% tech_vars| trial_data_1$code_2 %in% tech_vars)){
      time_frame[i,"tech_1hr_we"] <- 1
    } else {
      time_frame[i,"tech_1hr_we"] <- 0
    }
    
    if (any(trial_data_2$code_1 %in% tech_vars| trial_data_2$code_2 %in% tech_vars)){
      time_frame[i,"tech_2hr_we"] <- 1
    } else {
      time_frame[i,"tech_2hr_we"] <- 0
    }
    }
    
    ### weekday ### 
    # filter data for participant and weekday
    trial_data <- time_data %>% filter(id == time_frame$id[i] & (dow == 2|dow == 3|dow == 4|dow == 5|dow == 6)) 
    
    if (nrow(trial_data) == 0){
      #dont do anything if there is actually no data
    } else {
      
      #calculate tech total
      trial_data_tech <- trial_data %>% 
        filter(code_1 %in% tech_vars| code_2 %in% tech_vars) %>% 
        mutate(time_total = stop_1 - start_1) 
      time_frame[i,"tech_total_wd"] <- colSums(trial_data_tech[,"time_total"])
      
    #examine activity done last in the day
    last_act <- trial_data[which(trial_data$stop_1 == 86400), "code_1"]
    
    if (last_act == 4590){ #if participant was sleeping before midnight
      # extract when participant went to sleep
      sleeptime <- as.numeric(trial_data[which(trial_data$stop_1 == 86400) + c(-1), "stop_1"])
    } else {
      sleeptime == 86400 #if participant was not sleeping before midnight, we set their bedtime to midnight
    }
    
    trial_data_30 <- trial_data %>% # filter data for activities done prev 30 min
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60*0.5 & time_2 != 0)
    
    trial_data_1 <- trial_data %>%  # filter data for activities done prev 1 hr
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60 & time_2 != 0)
    
    trial_data_2 <- trial_data %>%  # filter data for activities done prev 2 hr
      mutate(time_2 = sleeptime - start_1) %>%
      filter(time_2 <= 60*60*2 & time_2 != 0)
    
    # add values to dataframe
    if (any(trial_data_30$code_1 %in% tech_vars| trial_data_30$code_2 %in% tech_vars)){
      time_frame[i,"tech_30m_wd"] <- 1
    } else {
      time_frame[i,"tech_30m_wd"] <- 0
    }
    
    if (any(trial_data_1$code_1 %in% tech_vars| trial_data_1$code_2 %in% tech_vars)){
      time_frame[i,"tech_1hr_wd"] <- 1
    } else {
      time_frame[i,"tech_1hr_wd"] <- 0
    }
    
    if (any(trial_data_2$code_1 %in% tech_vars| trial_data_2$code_2 %in% tech_vars)){
      time_frame[i,"tech_2hr_wd"] <- 1
    } else {
      time_frame[i,"tech_2hr_wd"] <- 0
    }
    }}
  write.csv(time_frame, "../data/cleaned/1_2_PSID_bedtime_data.csv")
} else {}

#it is important to note that participants can aways note down two activities, so total tech time can be above 24 hours. 
time_frame <- read.csv("../data/cleaned/1_2_PSID_bedtime_data.csv")
data_merge <- left_join(data_merge, time_frame, by = "id") # merge datasets

# recode tech total into dichotomous tech use variable
data_merge <- data_merge %>% mutate_at(vars(tech_total_wd, tech_total_we), 
                                       funs(di = recode(.,`0` = 0, .default = 1))) #recode into binary variables
data_merge$tech_di_wd <- data_merge$tech_total_wd_di
data_merge$tech_di_we <- data_merge$tech_total_we_di

# recode into continuous
data_merge$tech_total_wd <- data_merge$tech_total_wd*(1/(60*60))
data_merge$tech_total_we <- data_merge$tech_total_we*(1/(60*60))
data_merge$tech_cont_wd <- data_merge$tech_total_wd
data_merge$tech_cont_we <- data_merge$tech_total_we
is.na(data_merge[, c("tech_cont_wd", "tech_cont_we")]) <- 
  data_merge[, c("tech_total_wd", "tech_total_we")] == 0

#hist(data_merge$tech_total_wd, main = "Histogram of time-diary tech use on weekday", xlab = "hours")
#hist(data_merge$tech_total_we, main = "Histogram of time-diary tech use on weekend", xlab = "hours")

########################
### Retrospective ######
########################
vars_tech <- c("C14G10", #in last 30 days have you used computer to do school work
               "C14G13", #in last 30 days how often did you use computer for online activties
               "C14G16", #in last 30 days how often used a computer or eleectronic device to play games
               "C14G19") #in last 40 days how often use computer for interacting with others

is.na(data_merge[, vars_tech]) <- data_merge[, vars_tech] == 0 # set 0,8,9 to NA
is.na(data_merge[, vars_tech]) <- data_merge[, vars_tech] == 8
is.na(data_merge[, vars_tech]) <- data_merge[, vars_tech] == 9

data_merge$sr_tech <- 6- rowMeans(subset(data_merge, select = vars_tech), na.rm = TRUE)
#hist(data_merge$sr_tech, main = "Histogram of self-reported technology use", xlab = "self-reported tech use")

########################
### Correlation ######
########################
print(paste0("The correlation between general weekday technology use (time use diaries) and self-reported technology use is ", 
             round(cor.test(data_merge$sr_tech, data_merge$tech_total_wd)$estimate, 3)))
print(paste0("The correlation between general weekend technology use (time use diaries) and self-reported technology use is ", 
             round(cor.test(data_merge$sr_tech, data_merge$tech_total_we)$estimate, 3)))

#######################################################
# Create well-being variables
#######################################################

########################
### Depression    ######
########################
vars_dep <- paste0("C14C", 8:17) # choose variables 
vars_dep_r <- paste0("C14C", c("8","10", "14", "15", "16"), "_r") 
vars_dep_r <- c(vars_dep_r, paste0("C14C", c("9", "11", "12", "13", "17")))

is.na(data_merge[, vars_dep]) <- data_merge[, vars_dep] == 0 # set 0,8,9 to NA
is.na(data_merge[, vars_dep]) <- data_merge[, vars_dep] == 8
is.na(data_merge[, vars_dep]) <- data_merge[, vars_dep] == 9

data_merge <- data_merge %>% mutate_at(vars(paste0("C14C", c("8","10", "14", "15", "16"))), funs(r = recode(.,`1` = 3, `2` = 2, `3` = 1))) #recode

data_merge$wellbeing <- rowMeans(subset(data_merge, select = vars_dep_r), na.rm = TRUE)
#hist(data_merge$wellbeing, main = "Histogram of wellbeing", xlab = "Wellbeing Scores")

########################
### Self Esteem   ######
########################
vars_se <- paste0("C14E", 1:5) # chose variables 

is.na(data_merge[, vars_se]) <- data_merge[, vars_se] == 0 # set 0,8,9 to NA
is.na(data_merge[, vars_se]) <- data_merge[, vars_se] == 8
is.na(data_merge[, vars_se]) <- data_merge[, vars_se] == 9

data_merge <- data_merge %>% mutate_at(vars(paste0("C14E", 1:5)), funs(recode(.,`1` = 4, `2` = 3, `3` = 2, `4` = 1))) #recode

data_merge$selfesteem <- rowMeans(subset(data_merge, select = vars_se), na.rm = TRUE)
#hist(data_merge$selfesteem, main = "Histogram of Selfesteem", xlab = "Selfesteem Scores")

#######################################################
# Save dataset
#######################################################
write.csv(data_merge, file="../data/cleaned/1_2_PSID_dataset.csv")
rm(list = (paste("data", datasets, year, sep = "_")))

#####################################################################################
# MCS ####
#####################################################################################

#####################################################
# Load questionnaire and time use datasets
#####################################################

# load data which is in separate data files
data1 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_cm_assessment.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data1) <- tolower(names(data1))

data2 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_cm_derived.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data2) <- tolower(names(data2))

data3 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_cm_interview.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data3) <- tolower(names(data3))

data4 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_cm_measurement.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data4) <- tolower(names(data4))

data5 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_family_derived.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data5) <- tolower(names(data5))

data6 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_parent_cm_interview.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data6) <- tolower(names(data6))

data7 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_parent_assessment.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data7) <- tolower(names(data7))

data8 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_parent_derived.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data8) <- tolower(names(data8))

data9 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_parent_interview.sav",
                   use.value.labels = FALSE,
                   to.data.frame = TRUE,
                   use.missings = TRUE)
names(data9) <- tolower(names(data9))

data10 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_proxy_partner_interview.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
names(data10) <- tolower(names(data10))

data11 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_hhgrid.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
names(data11) <- tolower(names(data11))

data12 <- read.spss("../data/raw/mcs/Spring 2018 release/spss/spss19/mcs6_cm_tud_harmonised.sav",
                    use.value.labels = FALSE,
                    to.data.frame = TRUE,
                    use.missings = TRUE)
names(data12) <- tolower(names(data12))

#######################################################
# Merge the datasets of interest (questionnaires)
#######################################################

#######################################################
# Combine different datasets
# We combine the datasets by making identification 
# variables for each child. We cannot use the family
# identification variables because they obfuscate 
# twins/triplets. 
#######################################################

###################
# Cohort Members
###################
data1$mcsid1 <-
  ifelse(
    data1$fcnum00 == 1,
    paste(as.character(data1$mcsid), "_1", sep = ""),
    ifelse(
      data1$fcnum00 == 2,
      paste(as.character(data1$mcsid), "_2", sep = ""),
      ifelse(data1$fcnum00 == 3, paste(as.character(data1$mcsid), "_3", sep = ""), NA)
    )
  )
data1$mcsid <- as.character(data1$mcsid)
data2$mcsid2 <-
  ifelse(
    data2$fcnum00 == 1,
    paste(as.character(data2$mcsid), "_1", sep = ""),
    ifelse(
      data2$fcnum00 == 2,
      paste(as.character(data2$mcsid), "_2", sep = ""),
      ifelse(data2$fcnum00 == 3, paste(as.character(data2$mcsid), "_3", sep = ""), NA)
    )
  )
data3$mcsid <- gsub(" ", "", data3$mcsid, fixed = TRUE)
data3$mcsid3 <-
  ifelse(
    data3$fcnum00 == 1,
    paste(as.character(data3$mcsid), "_1", sep = ""),
    ifelse(
      data3$fcnum00 == 2,
      paste(as.character(data3$mcsid), "_2", sep = ""),
      ifelse(data3$fcnum00 == 3, paste(as.character(data3$mcsid), "_3", sep = ""), NA)
    )
  )
data4$mcsid4 <-
  ifelse(
    data4$fcnum00 == 1,
    paste(as.character(data4$mcsid), "_1", sep = ""),
    ifelse(
      data4$fcnum00 == 2,
      paste(as.character(data4$mcsid), "_2", sep = ""),
      ifelse(data4$fcnum00 == 3, paste(as.character(data4$mcsid), "_3", sep = ""), NA)
    )
  )
data5$mcsid <- gsub(" ", "", data5$mcsid, fixed = TRUE)
data5$mcsid5 <- as.character(data5$mcsid)

###################
# Parents
###################
data6$mcsid <- gsub(" ", "", data6$mcsid, fixed = TRUE)
data6$mcsid6 <-
  paste(as.character(data6$mcsid),
        as.character(data6$fpnum00),
        sep = "_")
data7$mcsid7 <-
  paste(as.character(data7$mcsid),
        as.character(data7$fpnum00),
        sep = "_")
data8$mcsid <- gsub(" ", "", data8$mcsid, fixed = TRUE)
data8$mcsid8 <-
  paste(as.character(data8$mcsid),
        as.character(data8$fpnum00),
        sep = "_")
data9$mcsid <- gsub(" ", "", data9$mcsid, fixed = TRUE)
data9$mcsid9 <-
  paste(as.character(data9$mcsid),
        as.character(data9$fpnum00),
        sep = "_")
data10$mcsid <- gsub(" ", "", data10$mcsid, fixed = TRUE)
data10$mcsid10 <-
  paste(as.character(data10$mcsid),
        as.character(data10$fpnum00),
        sep = "_")

###################
# Merge
###################
# Cohort Members
data_cm <-
  dplyr::left_join(data1, data2[, is.na(match(names(data2), names(data1)))], by = c("mcsid1" = "mcsid2"))
data_cm <-
  dplyr::left_join(data_cm, data3[, is.na(match(names(data3), names(data_cm)))], by = c("mcsid1" = "mcsid3"))
data_cm <-
  dplyr::left_join(data_cm, data4[, is.na(match(names(data4), names(data_cm)))], by = c("mcsid1" = "mcsid4"))
data_cm <-
  dplyr::left_join(data_cm, data5[, is.na(match(names(data5), names(data_cm)))], by = c("mcsid" = "mcsid5"))

# Parents
data_pa <-
  dplyr::left_join(data6, data7[, is.na(match(names(data7), names(data6)))], by = c("mcsid6" = "mcsid7"))
data_pa <-
  dplyr::left_join(data_pa, data8[, is.na(match(names(data8), names(data_pa)))], by = c("mcsid6" = "mcsid8"))
data_pa <-
  dplyr::left_join(data_pa, data9[, is.na(match(names(data9), names(data_pa)))], by = c("mcsid6" = "mcsid9"))

data_pa$mcsid1_r <-
  ifelse(
    data_pa$fcnum00 == 1,
    paste(as.character(data_pa$mcsid), "_1", sep = ""),
    ifelse(
      data_pa$fcnum00 == 2,
      paste(as.character(data_pa$mcsid), "_2", sep = ""),
      ifelse(data_pa$fcnum00 == 3, paste(
        as.character(data_pa$mcsid), "_3", sep = ""
      ), NA)
    )
  )
data_pa$fpnum00_r <- ifelse(data_pa$fpnum00 == 1, 1, 0)
data_pa_1 <- data_pa %>% filter(fpnum00_r == 1)

# Merge cohort members and parents, not merging duplicate rows
data <-
  dplyr::left_join(data_cm, data_pa_1[, is.na(match(names(data_pa_1), names(data_cm)))], by = c("mcsid1" = "mcsid1_r"))

###################
# Remove datasets
###################
rm(data1)
rm(data2)
rm(data3)
rm(data4)
rm(data5)
rm(data6)
rm(data7)
rm(data8)
rm(data9)
rm(data10)
rm(data11)
rm(data_pa_1)
gc()

#######################################################
# Create technology use variales
#######################################################

########################
### Time Use Diary #####
########################
# Wrangle time use data
data12$mcsid12 <-
  ifelse(
    data12$fcnum00 == 1,
    paste(as.character(data12$mcsid), "_1", sep = ""),
    ifelse(
      data12$fcnum00 == 2,
      paste(as.character(data12$mcsid), "_2", sep = ""),
      ifelse(data12$fcnum00 == 3, paste(as.character(data12$mcsid), "_3", sep = ""), NA)
    )
  )

names(data12) <- c("mcsid", "fcnum00", "order", "time", "datacollection", "month", "year", "dow", "activity", "mcsid12")
data12$change <-  c(NA,data12[2:nrow(data12), "activity"] - data12[1:(nrow(data12)-1), "activity"])

time_frame <-
  data.frame(matrix(NA, nrow = length(unique(data12$mcsid12)), ncol = 9))
names(time_frame) <-
  c(
    "id",
    "tech_30m_wd",
    "tech_1hr_wd",
    "tech_2hr_wd",
    "tech_total_wd",
    "tech_30m_we",
    "tech_1hr_we",
    "tech_2hr_we",
    "tech_total_we"
  )
time_frame$id <- unique(data12$mcsid12)

# Examine how many participants go to sleep after midnight (end of time use diary)
sleep_midnight <- data12 %>% filter(time == 144) %>% count(sleep = (activity == 1))
print(paste0(sleep_midnight[1,2], " time use diaries did not go to sleep before midnight, ", sleep_midnight[2,2], " did."))

#######################################################
# Recode the time use data  ####
#######################################################
tech_vars <- c(33, #answering emails, instant messaging or texting
               34, #browsing or updating social networking sites
               35, #general internet browsing, programming
               37, #playing electronic games and Apps
               38) #Watching TV, DVDs and dowloaded videos

if (analysis == 1){
  for (i in 1:nrow(time_frame)){
    print(i)
    
    ### weekend ### 
    
    trial_data <- data12 %>% filter(mcsid12 == time_frame$id[i] & (dow == 1|dow ==7)) # filter data for participant and weekend day
    if (nrow(trial_data) == 0){
      #dont do anything if there is actually no data 
    } else {
      
    #sum up total technology use
    if (any(trial_data$activity %in% tech_vars)) {
      temp <- trial_data %>% filter(activity %in% tech_vars) %>% summarise(total.count = n())
      time_frame[i,"tech_total_we"] <- temp[1,1]
    } else {
      time_frame[i,"tech_total_we"] <- 0
    }
    
    # now extract bedtime technology use
    trial_data_2 <- trial_data %>% filter(change != 0) #show those where there is an activity change
    if (nrow(trial_data_2) == 0){
      #dont do anything if there is actually no data 
    } else {
      
    if (trial_data_2[nrow(trial_data_2), "activity"]==1) { #if the last activity is sleeping (they went to bed)
      sleeptime <- trial_data_2[which(trial_data_2$activity == 1), "time"] - 1 # extract when participant went to sleep
      sleeptime <- sleeptime[length(sleeptime)] 
    } else {
      sleeptime <- 144 #if they did not go to bed we set going to bed to midnight
    }
    
      trial_data_30 <- trial_data %>% # filter data for activities done prev 30 min
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 3 & time_2 >= 0)
      
      trial_data_1 <- trial_data %>%  # filter data for activities done prev 1 hr
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 6 & time_2 >= 0)
      
      trial_data_2 <- trial_data %>%  # filter data for activities done prev 2 hr
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 12 & time_2 >= 0)
      
      # add values to dataframe
      if (any(trial_data_30$activity %in% tech_vars)){
        time_frame[i,"tech_30m_we"] <- 1
      } else {
        time_frame[i,"tech_30m_we"] <- 0
      }
      
      if (any(trial_data_1$activity %in% tech_vars)){
        time_frame[i,"tech_1hr_we"] <- 1
      } else {
        time_frame[i,"tech_1hr_we"] <- 0
      }
      
      if (any(trial_data_2$activity %in% tech_vars)){
        time_frame[i,"tech_2hr_we"] <- 1
      } else {
        time_frame[i,"tech_2hr_we"] <- 0
      }
    }}

    ### weekday ### 
    trial_data <- data12 %>% filter(mcsid12 == time_frame$id[i] & (dow == 2|dow == 3|dow == 4|dow == 5|dow == 6)) # filter data for participant and weekend day
    
    if (nrow(trial_data) == 0){
      #dont do anything if there is actually no data 
    } else {
      
    #sum up total technology use
    if (any(trial_data$activity %in% tech_vars)) {
      temp <- trial_data %>% filter(activity %in% tech_vars) %>% summarise(total.count = n())
      time_frame[i,"tech_total_wd"] <- temp[1,1]
    } else {
      time_frame[i,"tech_total_wd"] <- 0
    }
    
    # now extract bedtime technology use
    trial_data_2 <- trial_data %>% filter(change != 0) #show those where there is an activity change
    if (nrow(trial_data_2) == 0){
      #dont do anything if there is actually no data 
    } else {
      
    if (trial_data_2[nrow(trial_data_2), "activity"]==1) { #if the last activity is sleeping (they went to bed)
      sleeptime <- trial_data_2[which(trial_data_2$activity == 1), "time"] - 1 # extract when participant went to sleep
      sleeptime <- sleeptime[length(sleeptime)] 
    } else {
      sleeptime <- 144 #if they did not go to bed we set going to bed to midnight
    }
      
      trial_data_30 <- trial_data %>% # filter data for activities done prev 30 min
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 3 & time_2 >= 0)
      
      trial_data_1 <- trial_data %>%  # filter data for activities done prev 1 hr
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 6 & time_2 >= 0)
      
      trial_data_2 <- trial_data %>%  # filter data for activities done prev 2 hr
        mutate(time_2 = sleeptime - time) %>%
        filter(time_2 < 12 & time_2 >= 0)
      
      # add values to dataframe
      if (any(trial_data_30$activity %in% tech_vars)){
        time_frame[i,"tech_30m_wd"] <- 1
      } else {
        time_frame[i,"tech_30m_wd"] <- 0
      }
      
      if (any(trial_data_1$activity %in% tech_vars)){
        time_frame[i,"tech_1hr_wd"] <- 1
      } else {
        time_frame[i,"tech_1hr_wd"] <- 0
      }
      
      if (any(trial_data_2$activity %in% tech_vars)){
        time_frame[i,"tech_2hr_wd"] <- 1
      } else {
        time_frame[i,"tech_2hr_wd"] <- 0
      }
    }} 
  }
  
  write.csv(time_frame, "../data/cleaned/1_3_MCS_bedtime_data.csv")
  
} else {}

# merge datasets with previous datasets
time_frame <- read.csv("../data/cleaned/1_3_MCS_bedtime_data.csv")
data <-
  dplyr::left_join(data, time_frame, by = c("mcsid6" = "id"))

# change time use variables into hours
data$tech_total_wd <- data$tech_total_wd*10/60
data$tech_total_we <- data$tech_total_we*10/60

#hist(data$tech_total_wd, main = "Histogram of weekday time-diary tech use", xlab = "hours")
#hist(data$tech_total_we, main = "Histogram of weekend time-diary tech use", xlab = "hours")

# recode into dichotomous
data$tech_di_wd <- ifelse(data$tech_total_wd == 0, 0, ifelse(is.na(data$tech_total_wd) == TRUE, NA, 1))
data$tech_di_we <- ifelse(data$tech_total_we == 0, 0, ifelse(is.na(data$tech_total_we) == TRUE, NA, 1))

# recode into continuous
data$tech_cont_wd <- data$tech_total_wd
is.na(data[, c("tech_cont_wd")]) <- 
  data[, c("tech_cont_wd")] == 0
data$tech_cont_we <- data$tech_total_we
is.na(data[, c("tech_cont_we")]) <- 
  data[, c("tech_cont_we")] == 0

########################
### Retrospective ######
########################
# Convert measures to 10 point scale
data$fctvho00r <- (10 - 1) * (data$fctvho00 - 1) / (8 - 1) + 1
data$fccomh00r <- (10 - 1) * (data$fccomh00 - 1) / (8 - 1) + 1
data$fcinth00r <- (10 - 1) * (data$fcinth00 - 1) / (8 - 1) + 1
data$fcsome00r <- (10 - 1) * (data$fcsome00 - 1) / (8 - 1) + 1

# Make technology use measure
data$sr_tech <-
  rowMeans(subset(
    data,
    select = c(
      "fctvho00r", #hours per weekday watching TV or videos on computer
      "fccomh00r", #hours per weekday spent playing electronic games
      "fcinth00r", #does the adolescent own a computer
      "fcsome00r" #does the adolescent use internet at home
    )
  ), na.rm = TRUE)
#hist(data$sr_tech, main = "Histogram of self_reported tech use", xlab = "self-reported tech use")

########################
### Correlation ######
########################
print(paste0("The correlation between general technology use on a weekday (time use diaries) and self-reported technology use is ", 
             round(cor.test(data$sr_tech, data$tech_total_wd)$estimate, 3)))

#######################################################
# Create well-being variables
#######################################################

########################
### Self Esteem   ######
########################
data$fcsati00r <- 5 - data$fcsati00 
data$fcgdql00r <- 5 - data$fcgdql00
data$fcdowl00r <- 5 - data$fcdowl00
data$fcvalu00r <- 5 - data$fcvalu00 
data$fcgdsf00r <- 5 - data$fcgdsf00 

data$selfesteem <- rowMeans(subset(data, select = c("fcsati00r", "fcgdql00r", 
                                                          "fcdowl00r", "fcvalu00r", 
                                                          "fcgdsf00r")), na.rm = TRUE)
#hist(data$selfesteem, main = "Histogram of selfesteem", xlab = "Selfesteem")

########################
### Feelings Grid ######
########################
data$fcmdsa00r <- 4 - data$fcmdsa00
data$fcmdsb00r <- 4 - data$fcmdsb00
data$fcmdsc00r <- 4 - data$fcmdsc00
data$fcmdsd00r <- 4 - data$fcmdsd00
data$fcmdse00r <- 4 - data$fcmdse00
data$fcmdsf00r <- 4 - data$fcmdsf00
data$fcmdsg00r <- 4 - data$fcmdsg00
data$fcmdsh00r <- 4 - data$fcmdsh00
data$fcmdsi00r <- 4 - data$fcmdsi00
data$fcmdsj00r <- 4 - data$fcmdsj00
data$fcmdsk00r <- 4 - data$fcmdsk00
data$fcmdsl00r <- 4 - data$fcmdsl00
data$fcmdsm00r <- 4 - data$fcmdsm00

data$wellbeing <- rowMeans(subset(data, select = c("fcmdsa00r", "fcmdsb00r", 
                                                    "fcmdsc00r", "fcmdsd00r", 
                                                    "fcmdse00r", "fcmdsf00r",
                                                   "fcmdsg00r", "fcmdsh00r",
                                                   "fcmdsi00r", "fcmdsj00r",
                                                   "fcmdsk00r", "fcmdsl00r",
                                                   "fcmdsm00r")), na.rm = TRUE)
#hist(data$wellbeing, main = "Histogram of wellbeing", xlab = "Wellbeing")

########################
### SDQ ######
########################
data$fpsdro00r <- 4 - data$fpsdro00
data$fpsdhs00r <- 4 - data$fpsdhs00
data$fpsdtt00r <- 4 - data$fpsdtt00
data$fpsdsp00r <- 4 - data$fpsdsp00
data$fpsdmw00r <- 4 - data$fpsdmw00
data$fpsdfs00r <- 4 - data$fpsdfs00
data$fpsdfb00r <- 4 - data$fpsdfb00
data$fpsdud00r <- 4 - data$fpsdud00
data$fpsddc00r <- 4 - data$fpsddc00
data$fpsdnc00r <- 4 - data$fpsdnc00
data$fpsdoa00r <- 4 - data$fpsdoa00
data$fpsdpb00r <- 4 - data$fpsdpb00
data$fpsdcs00r <- 4 - data$fpsdcs00
data$fpsdgb00r <- 4 - data$fpsdgb00
data$fpsdfe00r <- 4 - data$fpsdfe00
data$fpsdte00r <- data$fpsdte00
data$fpsdst00r <- data$fpsdst00
data$fpsdlc00r <- data$fpsdlc00
data$fpsdgf00r <- data$fpsdgf00
data$fpsdor00r <- data$fpsdor00

data$sdqtot <- rowMeans(subset(data, select = c("fpsdhs00r", "fpsdmw00r", 
                                                   "fpsdud00r", "fpsdnc00r",
                                                   "fpsdfe00r", "fpsdtt00r",
                                                   "fpsdor00r", "fpsdfb00r",
                                                   "fpsdoa00r", "fpsdcs00r", 
                                                   "fpsdsp00r", "fpsdgf00r",
                                                   "fpsdlc00r", "fpsdpb00r", 
                                                   "fpsdgb00r", "fpsdro00r", 
                                                   "fpsdfs00r", "fpsddc00r",
                                                   "fpsdst00r", "fpsdte00r")), na.rm = TRUE)

#hist(data$sdqtot, main = "Histogram of SDQ total", xlab = "SDQ Total")

###############################
# Recode Control measures
###############################
# Make ethnicity, majority vs minority
# white = 1, other = 0
data$fd06e00 <- ifelse(data$fd06e00 == 1, 1, 0)

# Mean of educational motivation
data$fcscbe00r <- 6 - data$fcscbe00
data$fcsint00r <- 6 - data$fcsint00
data$edumot <-
  rowMeans(subset(
    data,
    select = c(
      "fcscbe00r",
      "fcsint00r",
      "fcsunh00",
      "fcstir00",
      "fcscwa00",
      "fcmnwo00"
    )
  ), na.rm = TRUE)

# Mean of closeness to parents
is.na(data[,c("fcrlqm00", "fcrlqf00")]) = data[, c("fcrlqm00", "fcrlqf00")] == 5 # make NA if do not have father/mother
data$clpar <-
  rowMeans(subset(data, select = c(
    "fcrlqm00", "fcrlqf00", "fcquam00", "fcquaf00"
  )), na.rm = TRUE)

# Recode sex
# 1 = male, 0 = female
data$sex <- 2 - data$fccsex00
print(paste0("In MCS there are ", table(data$sex)[[1]], " girls and ", table(data$sex)[[2]], " boys"))

# Age
data$age <- data$fccage00
print(paste0("In MCS there are ", table(data$age)[[1]], " 13 year-olds and ", 
             table(data$age)[[2]], " 14 year-olds and ",
             table(data$age)[[3]], " 15 year-olds"))

#######################################################
# Save dataset
#######################################################
write.csv(data, file="../data/cleaned/1_3_MCS_dataset.csv")
