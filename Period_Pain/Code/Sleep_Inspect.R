library(tidyverse)

Empathy <- readxl::read_xlsx("./Period_Pain/Data/Empathy_Data_9.25.2025_HLM Analyses.xlsx")

actigraphy_t1 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T1_Sleep Data_DailyActi_4.22.25.xlsx")

actigraphy_t2 <- readxl::read_xlsx("./Period_Pain/Data/Esleep_T2_Sleep Data_DailyActi_4.22.25.xlsx")

diary_t2 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T2Data_5.22.2025.xlsx")

diary_t1 <- readxl::read_xlsx("./Period_Pain/Data/E-Sleep_DailyDiary_FINAL_T1Data_5.22.2025.xlsb.xlsx")

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
rbind()

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
t1 <- t1 %>% mutate(Time = 1, 
                    T_DD_NapOnset = as.numeric(T_DD_NapOnset),
                    T_DD_NapDuration = as.numeric(T_DD_NapDuration),
                    T_DD_ActiDur = as.numeric(T_DD_ActiDur),
                    T_DD_SleepyAlert = as.numeric(T_DD_SleepyAlert),
                    T_DD_MorningDate = as.numeric(T_DD_MorningDate))
t2 <- t2 %>% mutate(Time = 2, 
                    T_DD_NapOnset = as.numeric(T_DD_NapOnset),
                    T_DD_NapDuration = as.numeric(T_DD_NapDuration),
                    T_DD_SleepyAlert = as.numeric(T_DD_SleepyAlert),
                    T_DD_MorningDate = as.numeric(T_DD_MorningDate))

t1 <- t1 %>% select(-c(T_DD_MedType1:T_DD_MedTime5))
t2 <- t2 %>% select(-c(T_DD_MedType1:T_DD_MedTime6))


sleepfull <- bind_rows(t1, t2)

#How many unique peopel are there at each time point. 
#How many days is someone on their period. 
#How many are ever on their periods

table(sleepfull$T_DD_Period)
89/(528+89+1)
table(sleepfull$T_DD_Period, sleepfull$Time)
49/(374+49+1)
40/(164+40)

#How many days on period at time 1 and time 2.  
sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(n = n())

sleepfull %>% group_by(Time, T_DD_Period) %>% summarize(mn = mean(T_DD_PeriodPain, na.rm = T), n = n())

sleepfull %>% filter(T_DD_Period == 0 & Time == 1 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()
sleepfull %>% filter(T_DD_Period == 0 & Time == 2 ) %>% select(C_ID, T_DD_Period,T_DD_PeriodDay, T_DD_PeriodPain, T_DD_PeriodFlow) %>% View()



#Person 404 is reporting period pain and flow at time 2 but period is 0. 












