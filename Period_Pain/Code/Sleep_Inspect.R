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
# 2) Figure out what to do with MedType6
# 3) Change all -999 variables to NA
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

glimpse(t1)
glimpse(t2)

t1[t1==-999] = NA

t1  
t2[t2==-999] = NA