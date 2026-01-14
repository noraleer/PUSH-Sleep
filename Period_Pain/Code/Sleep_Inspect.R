# Libraries
library(tidyverse)
library(lme4)
library(patchwork)

###############
# FILE READING
###############

# Empathy Data
Empathy <- read_csv("./Period_Pain/Data/Empathy_Data_Updated.csv")

# Actigraphy Data
actigraphy_t1 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T1_Sleep Data_DailyActi_4.22.25.xlsx")

actigraphy_t2 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T2_Sleep Data_DailyActi_4.22.25.xlsx")

# Diary Data
diary_t1 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T1Data_updated1.10.26.xlsx")

diary_t2 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T2Data_updated1.10.26.xlsx")

##########
# CLEANING
##########

## Mutating subject_id_t- to a double
actigraphy_t1_dec <- actigraphy_t1 %>% 
  select(-start_time_t1, -end_time_t1) %>% 
  mutate(subject_id_t1 = as.double(subject_id_t1))

actigraphy_t1_dec <- actigraphy_t1_dec %>% 
  mutate(subject_id_t1 = case_when(
    # Subject 524 became NA after changing it to a double
    is.na(subject_id_t1) ~ 524,
    TRUE ~ subject_id_t1
  ))

actigraphy_t2_dec <- actigraphy_t2 %>% 
  select(-start_time_t2, -end_time_t2) %>% 
  mutate(subject_id_t2 = as.double(subject_id_t2))

## Saving T-_DD_NightDate as DateTime variables to create variable 'weekend' later on
diary_t1$T1_DD_NightDate = as.Date(diary_t1$T1_DD_NightDate, origin = "1899-12-30")
diary_t2$T2_DD_NightDate = as.Date(diary_t2$T2_DD_NightDate, origin = "1899-12-30")

## Joining the diary data and actigraphy data for each time point
t1_joined <- diary_t1 %>% 
  full_join(actigraphy_t1_dec, by = c("C_ID" = "subject_id_t1", "T1_DD_DD_Interval" = "interval_number_t1"))

t2_joined <- diary_t2 %>% 
  full_join(actigraphy_t2_dec, by = c("C_ID" = "subject_id_t2", "T2_DD__DD_Interval" = "interval_number_t2"))

## Fixing the names
names(t1_joined) <- gsub("T1", "T", names(t1_joined))
names(t1_joined) <- gsub("t1", "T", names(t1_joined))

names(t2_joined) <- gsub("T2", "T", names(t2_joined))
names(t2_joined) <- gsub("t2", "T", names(t2_joined))

setdiff(names(t1_joined), names(t2_joined))
setdiff(names(t2_joined), names(t1_joined))

t2 <- t2_joined %>%
  rename(T_DD_DD_Interval = T_DD__DD_Interval,
         T_DD_NapDuration = "T_DD_-NapDuration",
         T_DD_CaffType_Juice = T_DD_CaffType_Juices,
         T_DD_MedType3_SSRIs = T_DD_Medtype3_SSRIs,
         onset_latency_T = onset_latency_T...9,
         sleep_midpoint_T = "sleep midpoint_T") %>% 
  select(-onset_latency_T...10)

t1 <- t1_joined %>% 
  rename(T_DD_CaffAmt_LessThanTheRecommendedAmt = T_DD_CaffAmt_LessThanTheRecommendedAmount,
         T_DD_CaffAmt_TheRecommendedAmt = T_DD_CaffAmt_TheRecommendedAmount,
         T_DD_CaffAmt_GreaterThanTheRecommendedAmt = T_DD_CaffAmt_GreaterThanTheRecommendedAmount)

setdiff(names(t2), names(t1))
setdiff(names(t1), names(t2))

## Summary tables for the number of days each subject was in each timepoint
t1 %>% 
  filter(T_DD_Period == 1) %>% 
  select(C_ID, T_DD_Period, T_DD_PeriodPain, T_DD_PeriodDay) %>%
  group_by(C_ID) %>% 
  summarise(num_days = n_distinct(T_DD_PeriodDay)) %>% 
  view()
 
t2 %>% 
  filter(T_DD_Period == 1) %>% 
  select(C_ID, T_DD_Period, T_DD_PeriodPain, T_DD_PeriodDay) %>%
  group_by(C_ID) %>% 
  summarise(num_days = n_distinct(T_DD_PeriodDay)) %>% 
  view()

## Replacing 999 with NAs in t1 and t2
t1[t1==-999] = NA
t1[t1==-99] = NA
t2[t2==-999] = NA
t2[t2==-99] = NA

glimpse(t1)
glimpse(t2)

#Update DayType to 1 is friday-saturday and 0 is any other night
t1 <- t1 %>% mutate(weekend = ifelse((wday(T_DD_NightDate, label = TRUE)) %in% c("Fri","Sat"),1,0))
t2 <- t2 %>% mutate(weekend = ifelse((wday(T_DD_NightDate, label = TRUE)) %in% c("Fri","Sat"),1,0))

## Creating a variable for checking for painkiller (ibuprofen, tylenol)
## Checking MedTypes
table(t1$T_DD_MedType1)
table(t1$T_DD_MedType2)
table(t1$T_DD_MedType3)
table(t1$T_DD_MedType4)
table(t1$T_DD_MedType5)

table(t2$T_DD_MedType1)
table(t2$T_DD_MedType2)
table(t2$T_DD_MedType3)
table(t2$T_DD_MedType4)
table(t2$T_DD_MedType5)
table(t2$T_DD_MedType6)

#Add painkiller indicator
t1 <- t1 %>% mutate(painkiller = ifelse(T_DD_MedType1 %in% c("advil","ibuprofen","Ibuprofen","Ibuprofin"),1,0))
t2 <- t2 %>% mutate(painkiller = ifelse(T_DD_MedType1 %in% c("advil","Advil","ibuprofen","Ibuprofen","Ibuprofin","tylenol") |
                                        T_DD_MedType2 %in% c("advil","Advil","ibuprofen","Ibuprofen","Ibuprofin","tylenol") |
                                        T_DD_MedType6 %in% c("advil","Advil","ibuprofen","Ibuprofen","Ibuprofin","tylenol"),1,0))

#Add time variable
t1 <- t1 %>% mutate(Time = 0, 
                    T_DD_NapOnset = as.numeric(T_DD_NapOnset),
                    T_DD_NapDuration = as.numeric(T_DD_NapDuration),
                    T_DD_ActiDur = as.numeric(T_DD_ActiDur),
                    T_DD_SleepyAlert = as.numeric(T_DD_SleepyAlert),
                    T_DD_MorningDate = as.numeric(T_DD_MorningDate))

t2 <- t2 %>% mutate(Time = 1, 
                    T_DD_NapOnset = as.numeric(T_DD_NapOnset),
                    T_DD_NapDuration = as.numeric(T_DD_NapDuration),
                    T_DD_SleepyAlert = as.numeric(T_DD_SleepyAlert),
                    T_DD_MorningDate = as.numeric(T_DD_MorningDate))

t1 <- t1 %>% select(-c(T_DD_MedType1:T_DD_MedTime5))
t2 <- t2 %>% select(-c(T_DD_MedType1:T_DD_MedTime6))

t1 <- t1 %>% left_join(Empathy %>% select(C_ID, demo_child_age_check_t1:weight3t1, BodyTotalt1), by = "C_ID")
names(t1)[grepl("t1",names(t1))] <- gsub("_t1","",names(t1)[grepl("t1",names(t1))])
names(t1)[grepl("t1",names(t1))] <- gsub("t1","",names(t1)[grepl("t1",names(t1))])

t2 <- t2 %>% left_join(Empathy %>% select(C_ID, demo_child_age_check_t2:weightdifft2, BodyTotalt2), by = "C_ID")
names(t2)[grepl("t2",names(t2))] <- gsub("_t2","",names(t2)[grepl("t2",names(t2))])
names(t2)[grepl("t2",names(t2))] <- gsub("t2","",names(t2)[grepl("t2",names(t2))])
names(t2)[names(t2) == "weigh"] <- "weight2"

# Have to remove weight3 from t1, completely full of NAs after the above operations
t1 <- t1 %>% select(-weight3)

# BodyTotal was stored as a character in t2, as was BMI
t2$BMI <- as.numeric(t2$BMI)
t2$BodyTotal <- as.numeric(t2$BodyTotal)
sleepfull <- bind_rows(t1, t2)

# sleepfull <- sleepfull %>% left_join(Empathy %>% select(C_ID,BodyTotalt0) %>% mutate(BodyTotalt0 = as.numeric(BodyTotalt0)), by = "C_ID")

# rename age and change age and BMI to numeric
sleepfull <- sleepfull %>% mutate(demo_child_age_check = as.numeric(demo_child_age_check)) %>% rename(age = demo_child_age_check)
sleepfull <- sleepfull %>% mutate(BMI = as.numeric(BMI)) 

sleepfull %>% filter(C_ID == 442) %>% select(C_ID, T_DD_NightDate,T_DD_Period, T_DD_PeriodFlow, T_DD_PeriodPain) %>% View()
table(sleepfull$demo_degree)
table(sleepfull$T_DD_PeriodPain,sleepfull$T_DD_Period)
table(sleepfull$T_DD_PeriodPain)
table(sleepfull$T_DD_PeriodFlow)

####################################
# EDA
####################################

## How many unique people are there at each time point.
sleepfull %>%
  group_by(Time) %>% 
  summarise(dist = n_distinct(C_ID))

# There are 67 unique participants at timepoint 1 and 30 unique participants at timepoint 2

## How many days is someone on their period. 

table(sleepfull$T_DD_Period)
100/(539+100)

# Of the 639 total observations across both time points, there are 100 days where a participant
# was on their period, and 539 days where a participant was not on their period. This comes out 
# to ~15.7% of the observations.

table(sleepfull$T_DD_Period, sleepfull$Time)

## How many days on period at time 1 and time 2.  
sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(n = n())
# As seen here, there are 41 observations where Period Pain is NA. I'm going to look at these
# observations to see what the other period-related variables are.
sleepfull %>% filter(is.na(T_DD_Period)) %>% select(T_DD_Period:T_DD_PeriodFlow) %>% view()
# One observation with T_DD_Period = NA has T_DD_PeriodPain and T_DD_PeriodFlow as 0,
# so this value should be 0 as well. The rest are NA across the board. What should we do with these?

## Finding the average pain and flow for those on their period at each time point and across both t1 and t2
sleepfull %>% group_by(T_DD_Period) %>% summarize(avg_pain = mean(T_DD_PeriodPain, na.rm = T), avg_flow = mean(T_DD_PeriodFlow, na.rm = T), n = n())
# The average pain level experienced across both time points is 2.8.
# The average flow level experienced across both time points is 4.82.
sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(avg_pain = mean(T_DD_PeriodPain, na.rm = T), avg_flow = mean(T_DD_PeriodFlow, na.rm = T), n = n())
# The average pain level experienced across t1 is 3.19, and the average pain level experienced across t2 is 2.44
# The average flow level experienced across t1 is 5.46, and the average flow level experienced across t2 is 4.25

# Looking to find any NA observations for Pain and Flow
sleepfull %>% filter(T_DD_Period == 0 & Time == 0 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
sleepfull %>% filter(T_DD_Period == 0 & Time == 1 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
# 506 from t2 has Period = 0 but Pain and Flow = NA

## Painkiller 
# How many observed painkiller uses are there for each trial?
sleepfull %>% group_by(Time) %>% summarise(mn = mean(painkiller), n = n(), sum(painkiller))
sleepfull %>% group_by(Time, T_DD_Period) %>% summarise(n = n(), pk = sum(painkiller))
# There are 14 observed painkiller uses at time point 1, and 6 observed painkiller uses at time point 2
# At both time points, 4 of the painkiller users were on their period. 
sleepfull %>% group_by(painkiller) %>% summarise(avg_pain = mean(T_DD_PeriodPain, na.rm = T), 
                                                 avg_flow = mean(T_DD_PeriodFlow, na.rm = T), 
                                                 avg_qual = mean(T_DD_SleepQual, na.rm = T),
                                                 avg_efcy = mean(efficiency_T, na.rm = T),
                                                 avg_waso = mean(waso_T, na.rm = T),
                                                 avg_time = mean(sleep_time_T, na.rm = T))

#How much is missing?
apply(sleepfull, 2, function(x){sum(is.na(x))})
# 603 of the observations are missing values for T_DD_PeriodDay, which could be problematic if 
# we're ever interested in looking at when pain or flow is at it's highest?

## Plots

# Pain and Flow vs. Total Sleep time
ggplot(aes(x = T_DD_PeriodPain, y = sleep_time_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. Total Sleep Time", x = "Period Pain", y = "Total Sleep Time", color = "Period")
ggplot(aes(x = T_DD_PeriodFlow, y = sleep_time_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. Total Sleep Time", x = "Period Flow", y = "Total Sleep Time", color = "Period")

# Pain and Flow vs. Sleep Offset
ggplot(aes(x = T_DD_PeriodPain, y = end_time_dec_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. Sleep Offset", x = "Period Pain", y = "Sleep Offset", color = "Period")
ggplot(aes(x = T_DD_PeriodFlow, y = end_time_dec_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. Sleep Offset", x = "Period Pain", y = "Sleep Offset", color = "Period")

# Pain and Flow vs. Sleep Onset
ggplot(aes(x = T_DD_PeriodPain, y = start_time_TRM_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. Sleep Onset", x = "Period Pain", y = "Sleep Onset", color = "Period")
ggplot(aes(x = T_DD_PeriodFlow, y = start_time_TRM_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. Sleep Onset", x = "Period Flow", y = "Sleep Onset", color = "Period")

# Pain and Flow vs. Sleep Efficiency
ggplot(aes(x = T_DD_PeriodPain, y = efficiency_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. Sleep Efficiency", x = "Period Pain", y = "Sleep Efficiency", color = "Period")
ggplot(aes(x = T_DD_PeriodFlow, y = efficiency_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. Sleep Efficiency", x = "Period Flow", y = "Sleep Efficiency", color = "Period")

# Pain and Flow vs. log-Wake After Sleep Onset
ggplot(aes(x = T_DD_PeriodPain, y = log(waso_T + 1), color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. log-WASO", x = "Period Pain", y = "log-WASO", color = "Period")
ggplot(aes(x = T_DD_PeriodFlow, y = log(waso_T + 1), color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. log-WASO", x = "Period Flow", y = "log-WASO", color = "Period")

# Pain and Flow vs. log-WASO across Time 0 and 1
ggplot(aes(x = T_DD_PeriodPain, y = log(waso_T + 1), color = as.factor(Time)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Pain vs. log-WASO", x = "Period Pain", y = "log-WASO", color = "Time")
ggplot(aes(x = T_DD_PeriodFlow, y = log(waso_T + 1), color = as.factor(Time)), data = sleepfull) + geom_point() + geom_smooth() +
  labs(title = "Period Flow vs. log-WASO", x = "Period Flow", y = "log-WASO", color = "Time")

####Modeling
# Timing 
# Offset/Waketime (end_time_dec_t2)
# Onset/Falling Asleep (start_time_TRM_t2)
# Quality 
# Efficiency (efficiency_t2)
# Awakenings (waso_t2)

# JOSIE/OLIVER PRELIMINARY MODELS

# no interaction terms between period and pain/flow
# kept interaction term between age and time, but only for the 'all' models

############################
#Duration sleep_time_1
############################

## PAIN
# All
mod_sleep_time_all <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_sleep_time_all)
# t1
mod_sleep_time_0 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_sleep_time_0)
#t2
mod_sleep_time_1 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_sleep_time_1)

## FLOW
# All
mod_sleep_time_all <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_sleep_time_all)
# t1
mod_sleep_time_0 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_sleep_time_0)
# t2
mod_sleep_time_1 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_sleep_time_1)

############################
#Offset/Waketime (end_time_dec_t2)
############################

#C_ID 494 is waking up at like 10pm?
ggplot(aes(x = end_time_dec_T), data = sleepfull) + geom_histogram()
sleepfull %>% filter(end_time_dec_T > 20)

sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] <- sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] - 24

## PAIN
# All
mod_offset_all <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_offset_all)
# t1
mod_offset_0 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_offset_0)
# t2
mod_offset_1 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_offset_1)

## FLOW
# All
mod_offset_all <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_offset_all)
# t1
mod_offset_0 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_offset_0)
# t2
mod_offset_1 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_offset_1)

############################
#Onset/Falling Asleep (start_time_TRM_t2)
############################

## PAIN
# All
mod_onset_all <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_onset_all)
# t1
mod_onset_0 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_onset_0)
# t2
mod_onset_1 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_onset_1)

## FLOW
# All
mod_onset_all <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_onset_all)
# t1
mod_onset_0 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_onset_0)
# t2
mod_onset_1 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_onset_1)

############################
#Efficiency (efficiency_t2)
############################

## PAIN
# All
mod_eff_all <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_eff_all)
# t1
mod_eff_0 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_eff_0)
# t2
mod_eff_1 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_eff_1)

## FLOW
# All
mod_eff_all <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_eff_all)
# t1
mod_eff_0 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_eff_0)
# t2
mod_eff_1 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_eff_1)

############################
#Awakenings (waso_t2)
############################

hist(sleepfull$waso_T) # might need log due to right-skew

## PAIN
# All
mod_waso_all <- lmer(log(waso_T + 1) ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_waso_all)
# t1
mod_waso_0 <- lmer(log(waso_T + 1) ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_waso_0)
# t2
mod_waso_1 <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_waso_1)

## FLOW
# All
mod_waso_all <- lmer(log(waso_T + 1) ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_waso_all)
# t1
mod_waso_0 <- lmer(log(waso_T + 1) ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_waso_0)
# t2
mod_waso_1 <- lmer(log(waso_T + 1) ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_waso_1)