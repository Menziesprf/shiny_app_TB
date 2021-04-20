library(tidyverse)
tb_raw <- read_csv("data/TB_burden_age_sex_2020-11-21.csv")

# 20 Countries with highest total incidence are selected
countries <- tb_raw %>% 
  filter(age_group == "all",
         sex == "a",
         risk_factor == "all") %>% 
  arrange(desc(best)) %>% 
  head(20) %>% 
  distinct(country) %>% 
  pull() %>% 
  paste(collapse = "|")


tb_top20 <- tb_raw %>% 
  filter(str_detect(country, countries)) %>% 
  filter(country != "China, Hong Kong SAR",
         country != "China, Macao SAR") %>% 
  mutate(country = recode(country,
                          "Democratic Republic of the Congo" = "DR Congo",
                          "Democratic People's Republic of Korea" = "DPR Korea",
                          "United Republic of Tanzania" = "Tanzania",
                          "Viet Nam" = "Vietnam"))



rm(tb_raw, countries)






