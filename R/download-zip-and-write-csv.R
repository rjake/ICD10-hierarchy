library(tidyverse)
library(readxl)
library(glue)
require(beepr)

year_metadata <- 
  list(
    list(year = 2022, url = "https://www.cms.gov/files/zip/2022-code-tables-tabular-and-index.zip",                     file = "Table and Index/icd10cm_tabular_2022.xml"),
    list(year = 2021, url = "https://www.cms.gov/files/zip/2021-code-tables-tabular-and-index-updated-12162020.zip",    file = "2021-code-tables-and-index/icd10cm_tabular_2021.xml"),
    list(year = 2020, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2020-ICD-10-CM-Code-Tables.zip",       file = "2020 Table and Index/icd10cm_tabular_2020.xml"),
    list(year = 2019, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2019-ICD-10-CM-Tables-and-Index.zip",  file = "icd10cm_tabular_2019.xml"),
    list(year = 2018, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Table-And-Index.zip",      file = "icd10cm_tabular_2018.xml"),
    list(year = 2017, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-ICD10-Code-Tables-Index.zip",     file = "icd10cm_tabular_2017.xml"),
    list(year = 2016, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-CM-Code-Tables-and-Index.zip",    file = "Tabular.xml"),
    list(year = 2015, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-tables-index.zip",                file = "Tabular.xml"),
    list(year = 2014, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Tables-and-Index.zip", file = "Tabular.xml")
  )


# add names (helpful when troubleshooting)
year_metadata <- 
  set_names(
    year_metadata, 
    paste0(
      "y_", map(year_metadata, pluck, "year")
    )

# parse all xml and write csv files
map_chr(year_metadata, pluck, "year") |> 
  as.integer() |> 
  walk(
    ~({
      assign("year", .x, envir = globalenv())
      source(file = "R/parse_xml.R")
    })
  ); beepr::beep(5)
