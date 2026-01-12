library(tidyverse)

Empathy <- read_csv("./Period_Pain/Data/Empathy_Data_Updated.csv")

actigraphy_t1 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T1_Sleep Data_DailyActi_4.22.25.xlsx")

actigraphy_t2 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T2_Sleep Data_DailyActi_4.22.25.xlsx")

diary_t1 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T1Data_updated1.10.26.xlsx")

diary_t2 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T2Data_updated1.10.26.xlsx")

##########
# CLEANING
##########

## Going to have to rename subject_id 

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

t1_joined <- diary_t1 %>% 
  full_join(actigraphy_t1_dec, by = c("C_ID" = "subject_id_t1", "T1_DD_DD_Interval" = "interval_number_t1"))

t2_joined <- diary_t2 %>% 
  full_join(actigraphy_t2_dec, by = c("C_ID" = "subject_id_t2", "T2_DD__DD_Interval" = "interval_number_t2"))

names(t1_joined) <- gsub("T1", "T", names(t1_joined))
names(t1_joined) <- gsub("t1", "T", names(t1_joined))
t1_joined
names(t2_joined) <- gsub("T2", "T", names(t2_joined))
names(t2_joined) <- gsub("t2", "T", names(t2_joined))
t2_joined

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
setdiff(names(t1), names(t2)) # Everything in t1 is present in t2, but MedType6 is not present in t1. 

# TO DO...
# 1) Make sure all variables are of the same class between the t1 and t2 datasets
# 2) Figure out what to do with MedType6 (exists in t2 and not t1)
# 3) Change all -999 variables to NA (done)
# 4) Update DayType using NightDate to be school night or not school night (done)
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

#Replacing 999 with NAs in t1 and t2
t1[t1==-999] = NA
t1[t1==-99] = NA
t1  
t2[t2==-999] = NA
t2[t2==-99] = NA

glimpse(t1)
glimpse(t2)

#Update DayType to 1 is friday-saturday and 0 is any other night
t1 <- t1 %>% mutate(weekend = ifelse((wday(T_DD_NightDate, label = TRUE)) %in% c("Fri","Sat"),1,0))
t2 <- t2 %>% mutate(weekend = ifelse((wday(T_DD_NightDate, label = TRUE)) %in% c("Fri","Sat"),1,0))

#Binding the times into one dataset

#create a variable for checking for painkiller (ibuprofen, tylenol)
table(t1$T_DD_MedType1)

table(t2$T_DD_MedType6)
table(t2$T_DD_MedType1)

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
#sleepfull <- sleepfull %>% mutate(T_DD_PeriodPain = ifelse(is.na(T_DD_PeriodPain),0,T_DD_PeriodPain))
#sleepfull <- sleepfull %>% mutate(T_DD_PeriodFlow = ifelse(is.na(T_DD_PeriodFlow),0,T_DD_PeriodFlow))


sleepfull %>% filter(C_ID == 442) %>% select(C_ID, T_DD_NightDate,T_DD_Period, T_DD_PeriodFlow, T_DD_PeriodPain) %>% View()
table(sleepfull$demo_degree)
table(sleepfull$T_DD_PeriodPain,sleepfull$T_DD_Period)
table(sleepfull$T_DD_PeriodPain)
table(sleepfull$T_DD_PeriodFlow)

####################################
# EDA
####################################

#How many unique people are there at each time point. 
#How many days is someone on their period. 
#How many are ever on their periods

table(sleepfull$T_DD_Period)
89/(538+89+1)
table(sleepfull$T_DD_Period, sleepfull$Time)
49/(374+49+1)
40/(164+40)

#How many days on period at time 1 and time 2.  
sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(n = n())

sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(mn = mean(T_DD_PeriodPain, na.rm = T), n = n())

#Person 404 is reporting period pain and flow at time 2 but period is 0. FIXED IN RAW DATA
sleepfull %>% filter(T_DD_Period == 0 & Time == 0 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
sleepfull %>% filter(T_DD_Period == 0 & Time == 1 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
sleepfull %>% filter(T_DD_Period == 0 & Time == 2 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()

sleepfull %>% filter(C_ID == 404) %>% select(C_ID, T_DD_NightDate,T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
#table(sleepfull$T_DD_PeriodFlow)
#table(sleepfull$T_DD_PeriodPain)


sleepfull %>% group_by(Time) %>% summarise(mn = mean(painkiller), n = n(), sum(painkiller))

#How much is missing?
apply(sleepfull, 2, function(x){sum(is.na(x))})

ggplot(aes(x = T_DD_PeriodPain, y = sleep_time_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = sleep_time_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()

ggplot(aes(x = T_DD_PeriodPain, y = end_time_dec_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = end_time_dec_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()

ggplot(aes(x = T_DD_PeriodPain, y = start_time_TRM_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = start_time_TRM_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()

ggplot(aes(x = T_DD_PeriodPain, y = efficiency_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = efficiency_T, color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()

ggplot(aes(x = T_DD_PeriodPain, y = log(waso_T+ 1), color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = log(waso_T+ 1), color = as.factor(T_DD_Period)), data = sleepfull) + geom_point() + geom_smooth()

ggplot(aes(x = T_DD_PeriodPain, y = log(waso_T + 1), color = as.factor(Time)), data = sleepfull) + geom_point() + geom_smooth()
ggplot(aes(x = T_DD_PeriodFlow, y = log(waso_T + 1), color = as.factor(Time)), data = sleepfull) + geom_point() + geom_smooth()

####Modeling
# Age (demo_child_age_check_t1)
# Parental Education (demo_degreet1)
# BMI (BMIt1)
# Being on period vs. not (T1_DD_Period)
# Body Map Pain Summary Score (GSS) (CBodyTotalt0)
# School vs Off Day (T1_DD_DayType) (School = 1, Off Day = 0)

#What do we do with Demo Degree NA?

############################
#Duration sleep_time_1
############################
library(lme4)

mod_sleep_time_all <- lmer(sleep_time_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
summary(mod_sleep_time_all)

mod_sleep_time_0 <- lmer(sleep_time_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 0))
mod_sleep_time_1 <- lmer(sleep_time_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 1))
summary(mod_sleep_time_0)
summary(mod_sleep_time_1)

############################
#Offset/Waketime (end_time_dec_t2)
############################

#C_ID 494 is waking up at like 10pm?
ggplot(aes(x = end_time_dec_T), data = sleepfull) + geom_histogram()
sleepfull %>% filter(end_time_dec_T > 20)

sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] <- sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] - 24

mod_offset_all <- lmer(end_time_dec_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
summary(mod_offset_all)
plot(mod_offset_all)

mod_offset_0 <- lmer(end_time_dec_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 0))
mod_offset_1 <- lmer(end_time_dec_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 1))
summary(mod_offset_0)
summary(mod_offset_1)

############################
#Onset/Falling Asleep (start_time_TRM_t2)
############################

mod_onset_all <- lmer(start_time_TRM_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
summary(mod_onset_all)
plot(mod_onset_all)

mod_onset_0 <- lmer(start_time_TRM_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 0))
mod_onset_1 <- lmer(start_time_TRM_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 1))
summary(mod_onset_0)
summary(mod_onset_1)



############################
#Efficiency (efficiency_t2)
############################

mod_eff_all <- lmer(efficiency_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
summary(mod_eff_all)
plot(mod_eff_all)

mod_eff_0 <- lmer(efficiency_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 0))
mod_eff_1 <- lmer(efficiency_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 1))
summary(mod_eff_0)
summary(mod_eff_1)


############################
#Awakenings (waso_t2)
############################

hist(sleepfull$waso_T)

mod_waso_all <- lmer(log(waso_T + 1) ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
#mod_waso_all <- lmer(waso_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + BMI + demo_degree + I(age-13) * Time +  (1|C_ID) , data = sleepfull)
summary(mod_waso_all)
plot(mod_waso_all)

mod_waso_0 <- lmer(waso_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 0))
mod_waso_1 <- lmer(waso_T ~ 1 + BodyTotalt0 + weekend + T_DD_Period + BMI + demo_degree + T_DD_Period:T_DD_PeriodPain + T_DD_Period:T_DD_PeriodFlow + I(age-13) +  (1|C_ID) , data = sleepfull %>% filter(Time == 1))
summary(mod_waso_0)
summary(mod_waso_1)




# 
# Timing 
# Offset/Waketime (end_time_dec_t2)
# Onset/Falling Asleep (start_time_TRM_t2)
# Quality 
# Efficiency (efficiency_t2)
# Awakenings (waso_t2)




# JOSIE/OLIVER PRELIMINARY MODELS

# no interaction terms between period and pain/flow
# kept interaction term between age and time

############################
#Duration sleep_time_1
############################

# pain
#To do:
#1. Drop demo_degree 
#2. Replace Body Total t0 with Body Total t1 and t2.  
#3. Should we add pain killer to model?  
#4. Jules: Update Period to be 1 id Pain>0 and/or Flow > 0 

## PAIN
mod_sleep_time_all <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_sleep_time_all)
# weekend is being dropped?

mod_sleep_time_0 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_sleep_time_0)
# weekend, age:time, and time are being dropped

mod_sleep_time_1 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_sleep_time_1)
# same drops as mod_sleep_time_0

## FLOW
mod_sleep_time_all <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_sleep_time_all)
# weekend is dropped again

mod_sleep_time_0 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_sleep_time_0)
# weekend, age:time, and time are being dropped

mod_sleep_time_1 <- lmer(sleep_time_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_sleep_time_1)
# weekend, age:time, and time are being dropped


############################
#Offset/Waketime (end_time_dec_t2)
############################

#C_ID 494 is waking up at like 10pm?
ggplot(aes(x = end_time_dec_T), data = sleepfull) + geom_histogram()
sleepfull %>% filter(end_time_dec_T > 20)

sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] <- sleepfull$end_time_dec_T[!is.na(sleepfull$end_time_dec_T) & sleepfull$end_time_dec_T > 20] - 24

## PAIN

mod_offset_all <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_offset_all)
# weekend dropped again

mod_offset_0 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_offset_0)
# weekend, age:time, and time dropped

mod_offset_1 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_offset_1)
# weekend, age:time, and time dropped

## FLOW

mod_offset_all <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_offset_all)
# weekend dropped again!!!

mod_offset_0 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_offset_0)
# weekend, age:time, and time dropped again

mod_offset_1 <- lmer(end_time_dec_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_offset_1)
# weekend, age:time, and time dropped again


############################
#Onset/Falling Asleep (start_time_TRM_t2)
############################

## PAIN

mod_onset_all <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_onset_all)
plot(mod_onset_all)
# weekend dropped again

mod_onset_0 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_onset_0)
# weekend, age:time, time dropped again

mod_onset_1 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_onset_1)
# weekend, age:time, time dropped again

## FLOW

mod_onset_all <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_onset_all)
plot(mod_onset_all)
# weekend dropped

mod_onset_0 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_onset_0)
# weekend, age:time, time dropped again

mod_onset_1 <- lmer(start_time_TRM_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_onset_1)
# weekend, age:time, time dropped again

############################
#Efficiency (efficiency_t2)
############################

## PAIN

mod_eff_all <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_eff_all)
plot(mod_eff_all)
# weekend dropped

mod_eff_0 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_eff_0)
# weekend, age:time, time dropped again

mod_eff_1 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_eff_1)
# weekend, age:time, time dropped again

## FLOW

mod_eff_all <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_eff_all)
plot(mod_eff_all)
# weekend dropped

mod_eff_0 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_eff_0)
# weekend, age:time, time dropped

mod_eff_1 <- lmer(efficiency_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_eff_1)
# weekend, age:time, time dropped

############################
#Awakenings (waso_t2)
############################

hist(sleepfull$waso_T) # might need log

## PAIN

mod_waso_all <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull)
summary(mod_waso_all)
plot(mod_waso_all)
# weekend dropped

mod_waso_0 <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_waso_0)
# weekend, age:time, time dropped

mod_waso_1 <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodPain + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_waso_1)
# weekend, age:time, time dropped

## FLOW

mod_waso_all <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull)
summary(mod_waso_all)
plot(mod_waso_all)
# weekend dropped

mod_waso_0 <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 0))
summary(mod_waso_0)
# weekend, age:time, time dropped

mod_waso_1 <- lmer(waso_T ~ 1 + BodyTotal + weekend + T_DD_Period + BMI + I(age-13) * Time + T_DD_PeriodFlow + (1|C_ID), data = sleepfull %>% filter(Time == 1))
summary(mod_waso_1)
# weekend, age:time, time dropped