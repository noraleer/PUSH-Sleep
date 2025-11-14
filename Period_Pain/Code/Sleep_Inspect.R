library(tidyverse)

Empathy <- readxl::read_xlsx("./Eeep/Empathy_Data_9.25.2025_HLM Analyses.xlsx")

actigraphy_t1 <- readxl::read_xlsx("./Eeep/Esleep_T1_Sleep Data_DailyActi_4.22.25.xlsx")

actigraphy_t2 <- readxl::read_xlsx("./Eeep/Esleep_T2_Sleep Data_DailyActi_4.22.25.xlsx")

diary_t2 <- readxl::read_xlsx("./Eeep/E-Sleep_DailyDiary_FINAL_T2Data_5.22.2025.xlsx")

diary_t1 <- readxl::read_xlsx("./Eeep/E-Sleep_DailyDiary_FINAL_T1Data_5.22.2025.xlsb.xlsx")

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

### Yadaydayda