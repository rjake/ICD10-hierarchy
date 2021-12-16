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
  )


# view as table
map_dfr(year_metadata, flatten)

# create directory (if it doesn't exist)
archive <- "file-history"
dir.create(archive) |> suppressWarnings()

# download files froc CDC
download_zip_file <- function(x, overwrite = FALSE) {
  # x = year_metadata$y_2014
  message("working on ", x$year, ' - - - - - - -') 
  archive_zip <- paste0(archive, "/cms/", x$year, ".zip")
  if (!file.exists(archive_zip) | overwrite) {
    download.file(x$url, destfile = archive_zip, mode = "wb")
  }
}


# unzip files from CDC
unzip_file <- function(x, overwrite = FALSE) {
  message("working on ", x$year, ' - - - - - - -') 
  new_xml_path <- paste0(archive, "/icd10_", x$year, ".xml")
  archive_zip <- paste0(archive, "/cms/", x$year, ".zip")

  if (file.exists(new_xml_path) & !overwrite) {
    return(
      message("file already exists for", x$year, "use 'overwrite = TRUE'")
    )
  }

  unzip(zipfile = archive_zip, files = x$file, exdir = archive)
  file.rename(file.path(archive, x$file), new_xml_path)
}


# find xml files using 'list = TRUE'
unzip(zipfile = "file-history/cms/2022.zip", list = TRUE) |> 
  filter(str_detect(Name, "abular.*xml$"))

# debugonce(unzip_file); unzip_file(x = year_metadata$y_2016)

# download all zip files
map(year_metadata, possibly(download_zip_file, NULL)); beepr::beep(5)

# unzip all xml files
map(year_metadata, possibly(unzip_file, NULL)); beepr::beep(5)


# parse all xml and write csv files
map_chr(year_metadata, pluck, "year") |> 
  as.integer() |> 
  walk(
    ~({
      assign("year", .x, envir = globalenv())
      source(file = "R/parse_xml.R")
    })
  ); beepr::beep(5)
