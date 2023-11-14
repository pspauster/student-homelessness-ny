library(tidyverse)
library(openxlsx)
library(janitor)
library(rvest)
library(RSocrata)


twenty_two <- read.xlsx("https://www.nysteachs.org/_files/ugd/60b30b_00435aa177c149e7b752abe2180d7de2.xlsx?dn=INF_SED_SIRS2021-22_111422.xlsx",
                                  sheet = 3) %>% 
  clean_names()

sd_22 <- filter(twenty_two, org_type == "SCHOOL DISTRICT") %>% 
  mutate(sd = str_extract(lea_name, "\\d+"))

enrollment <- read.socrata("https://data.cityofnewyork.us/resource/c7ru-d68s.csv") %>% 
  clean_names() %>% 
  mutate(sd = str_sub(dbn, start = 1, end = 2),
         sd_no = as.integer(sd))

sd_enrollment <- enrollment %>% 
  filter(sd_no <= 32) %>% 
  mutate(sd = as.character(sd_no)) %>% 
  group_by(year, sd) %>% 
  summarize(total_enrollment = sum(total_enrollment))

sd_22_rate <- inner_join(sd_22, filter(sd_enrollment, year == "2021-22"), by = "sd") %>% 
  mutate(homelessness_rate = as.numeric(total_2021_22_unduplicated)/total_enrollment*100)

write.csv(sd_22_rate, "homeless_students_by_sd_2021-2022.csv")


nysteachs_page <- read_html("https://www.nysteachs.org/data-on-student-homelessness")

links <- nysteachs_page %>% 
  html_nodes("a.MVY5Lo") %>% 
  html_attr("href")

read_teachs_data <- function(link) {
  data <- read.xlsx(link, sheet = 3)
  
    if (ncol(data)==5) {
      data_clean <- data %>% 
        setNames(c("beds_code", "lea_name", "org_type", "county_name", "total")) %>% 
        mutate(year = str_extract(link, "(?<=SIRS).{7}"))      
    } else
      data_clean <- data %>% 
        setNames(c("beds_code", "lea_name", "org_type", "county_name", "region", "total")) %>% 
        mutate(year = str_extract(link, "(?<=SIRS).{7}"))
    
    return(data_clean)
}

read_teachs_data(links[1])

student_homelessness_detailed <- map_dfr(.x = links[0:6], .f = ~read_teachs_data(.x))

time_series_clean <- student_homelessness_time %>% 
  group_by(year) %>% 
  summarize(total = sum(as.numeric(total), na.rm = T))
  
read_teachs_sum <- function(link) {
  read.xlsx(link, sheet = 1) %>% 
    filter(.[1]=="NYC, including Charter Schools in NYC"|
             .[1]=="Rest of State, including Charter Schools in ROS" |
             .[1]=="NYC (not incl Charters)"|
             .[1]=="Rest of State (not incl Charters)")%>% 
    mutate(year = str_extract(link, "(?<=SIRS).{7}"),
           count = X2,
           series = .[[1]]) %>% 
    select(year, count, series)
}

student_homelessness_time <- map_dfr(.x = links[0:9], .f = ~read_teachs_sum(.x))

#look at charters only

