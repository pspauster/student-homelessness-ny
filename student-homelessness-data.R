library(tidyverse)
library(openxlsx)
library(janitor)


twenty_two <- read.xlsx("https://www.nysteachs.org/_files/ugd/60b30b_00435aa177c149e7b752abe2180d7de2.xlsx?dn=INF_SED_SIRS2021-22_111422.xlsx",
                                  sheet = 3) %>% 
  clean_names()

sd_22 <- filter(twenty_two, org_type == "SCHOOL DISTRICT") %>% 
  mutate(sd = str_extract(lea_name, "\\d+"))

enrollment <- read.socrata("https://data.cityofnewyork.us/resource/c7ru-d68s.csv") %>% 
  clean_names() %>% 
  mutate(sd = as.integer(str_sub(dbn, start = 1, end = 2)))

sd_enrollment <- enrollment %>% 
  filter(sd <=32) %>% 
  group_by(year, sd) %>% 
  summarize(total_enrollment = sum(total_enrollment))

sd_22_rate <- 

write.csv(sd_22, "homeless_students_by_sd_2021-2022.csv")
