library(tidyverse)

# txt file:  https://www.cob.cms.hhs.gov/Section111/assets/section111/icd10.dx.codes.htm
# xml file:  ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2020/icd10cm_tabular_2020.xml

# open in XML in Excel then save as CSV
get_data <- 
    read.csv(
        file = "ICD-10-CM/icd10cm_tabular_2020.csv", 
        stringsAsFactors = FALSE, 
        na.strings = ""
    )

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
           first_8 = name23,
           first_8_text = desc24,
           last = char,
           last_text = extension) %>% 
    distinct()

get_last <-
    select_data %>% 
    distinct(region_join = region, last, last_text) %>% 
    filter(complete.cases(.))

prep_data <-
    select_data %>% 
    select(-last, -last_text) %>%
    distinct() %>%
    fill(section) %>% 
    #slice(1:200) %>% 
    mutate(region       = lag(region),
           #section      = lead(section),
           first_3_text = lead(first_3_text),
           first_5      = lead(first_5, 2),
           first_5_text = lead(first_5_text, 3),
           first_6      = lead(first_6, 4),
           first_6_text = lead(first_6_text, 5),
           first_7      = lead(first_7, 6),
           first_7_text = lead(first_7_text, 7),
           first_8      = lead(first_8, 8),
           first_8_text = lead(first_8_text, 9)) %>% 
    mutate(icd10_code = coalesce(first_8, first_7, first_6, first_5, first_3)) %>% 
    filter(!is.na(icd10_code))
# 89560

fill_data <-
  prep_data %>%
    filter(str_detect(icd10_code, "E08.3")) %>% 
  fill(region, first_3, first_3_text, first_5, first_5_text, first_6, first_6_text, first_7, first_7_text) %>%
    mutate(region_join = ifelse(
        str_detect(icd10_code, "E08.31"),
        NA,
        icd10_code)
    ) %>% 
    #filter(str_detect(id, "S40")) %>% 
    left_join(get_last) %>% 
    left_join(get_regions %>% select(contains("region"))) %>% 
    mutate(first_8 = ifelse(!is.na(last), paste0(icd10_code, last), first_8),
           first_8_text = ifelse(!is.na(last), last, first_8_text),
           icd10_code = coalesce(first_8, icd10_code)) %>% 
    select(section, 
           starts_with("region"),
           icd10_code,
           everything(),
           -last, -last_text)

missing_df <-
    orig%>% 
    filter(!ICD10_CD %in% fill_data$icd10_code) %>% 
    filter(!is.na(CHAR_5))

# 53745

a <-
    fill_data %>% 
    filter(str_detect(first_3, "S42"))
slice(10000:11000)


df <- read.delim("ICD-10-CM/Section111ValidICD10-Apr2020.txt")
