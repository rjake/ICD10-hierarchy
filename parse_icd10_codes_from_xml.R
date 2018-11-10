library(tidyverse)
setwd("~/GitHub/ICD10_codes/2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index")

get_data <-
    read.csv("icd10cm_tabular_2019.csv", 
             stringsAsFactors = F, na.strings = "")

get_regions <-
    get_data %>% 
    distinct(region_text = sectionRef, first, last, region = id)

select_data <-
    get_data %>% 
    select(region = id6,
           section = name,
           first_3 = name8,
           first_3_text = desc9,
           first_5 = name10,
           first_5_text = desc11,
           first_6 = name14,
           first_6_text = desc15,
           first_7 = name20,
           first_7_text = desc21,
           last = char,
           last_text = extension) %>% 
    distinct()

get_last <-
    select_data %>% 
    distinct(region, last, last_text) %>% 
    filter(complete.cases(.))

prep_data <-
    select_data %>% 
    select(-last, -last_text) %>%
    distinct() %>% 
    #slice(1:200) %>% 
    mutate(region = lag(region),
           first_3_text = lead(first_3_text),
           first_5 = lead(first_5, 2),
           first_5_text = lead(first_5_text, 3),
           first_6 = lead(first_6, 4),
           first_6_text = lead(first_6_text, 5),
           first_7 = lead(first_7, 6),
           first_7_text = lead(first_7_text, 7)) %>% 
    mutate(first_5 = ifelse((is.na(first_5) & !is.na(first_3)), "-", first_5),
           first_6 = ifelse((is.na(first_6) & !is.na(first_5)), "-", first_6),
           first_7 = ifelse((is.na(first_7) & !is.na(first_6)), "-", first_7)) %>%
    mutate(first_5_text = ifelse(first_5 == "-", "-", first_5_text),
           first_6_text = ifelse(first_6 == "-", "-", first_6_text),
           first_7_text = ifelse(first_7 == "-", "-", first_7_text)) %>% 
    distinct()
    

fill_data <-
    prep_data %>% 
    fill(region, .direction = "down") %>% 
    fill(section, .direction = "down") %>% 
    fill(region, .direction = "down") %>%
    fill(first_3, .direction = "down") %>%
    fill(first_3_text, .direction = "down") %>%
    fill(first_5, .direction = "down") %>%
    fill(first_5_text, .direction = "down") %>%
    fill(first_6, .direction = "down") %>%
    fill(first_6_text, .direction = "down") %>%
    fill(first_7, .direction = "down") %>%
    fill(first_7_text, .direction = "down") %>%
    distinct() %>% 
    filter(!is.na(region)) %>% 
    #filter(str_detect(id, "S40")) %>% 
    left_join(get_last) %>% 
    left_join(get_regions %>% select(contains("region"))) %>% 
    mutate(last = ifelse(is.na(last), "-", last),
           last_text = ifelse(is.na(last_text), "-", last_text)) %>% 
    select(section, 
           contains("region"), 
           everything())


a <-
    select_data %>% 
    filter(str_detect(id6, "S4"))
slice(10000:11000)
