library(tidyverse)
library(readxl)
library(glue)
require(beepr)

# might need to download and inspect to find file name
year_metadata <- {
  list(
    list(year = 2024L, url = "https://www.cms.gov/files/zip/2024-code-tables-tabular-and-index-updated-06/29/2023.zip", file = "Table and Index/icd10cm_tabular_2024.xml"),
    list(year = 2023L, url = "https://www.cms.gov/files/zip/2023-code-tables-tabular-and-index-updated-01/11/2023.zip", file = "icd10cm_tabular_2023.xml"),
    list(year = 2022L, url = "https://www.cms.gov/files/zip/2022-code-tables-tabular-and-index.zip",                    file = "Table and Index/icd10cm_tabular_2022.xml"),
    list(year = 2021L, url = "https://www.cms.gov/files/zip/2021-code-tables-tabular-and-index-updated-12162020.zip",   file = "2021-code-tables-and-index/icd10cm_tabular_2021.xml"),
    list(year = 2020L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2020-ICD-10-CM-Code-Tables.zip",      file = "2020 Table and Index/icd10cm_tabular_2020.xml"),
    list(year = 2019L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2019-ICD-10-CM-Tables-and-Index.zip", file = "icd10cm_tabular_2019.xml"),
    list(year = 2018L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Table-And-Index.zip",     file = "icd10cm_tabular_2018.xml"),
    list(year = 2017L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-ICD10-Code-Tables-Index.zip",    file = "icd10cm_tabular_2017.xml"),
    list(year = 2016L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-CM-Code-Tables-and-Index.zip",   file = "Tabular.xml"),
    list(year = 2015L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-tables-index.zip",               file = "Tabular.xml"),
    list(year = 2014L, url = "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Tables-and-Index.zip",file = "Tabular.xml")
  ) |> 
 set_names(paste0("y_", 2024:2014))
}
 
# view as table
map_dfr(year_metadata, flatten)   

# create archive directory if it doesn't exist
archive <- "file-history"
if (!file.exists(archive)) {
  dir.create(paste0(archive, "/cms/"), recursive = TRUE, showWarnings = FALSE)
}

# download files from CDC
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


# debugonce(unzip_file); unzip_file(x = year_metadata$y_2016)

# download all zip files
map(year_metadata, possibly(download_zip_file, NULL)); beepr::beep(5)

# find xml files using 'list = TRUE'
unzip(zipfile = "file-history/cms/2022.zip", list = TRUE) |> 
  filter(str_detect(Name, "abular.*xml$"))

# unzip all xml files
map(year_metadata, possibly(unzip_file, NULL)); beepr::beep(5)


# parse all xml and write csv files
map_int(year_metadata, pluck, "year") |> 
  walk(
    ~({
      assign("year", .x, envir = globalenv())
      source(file = "R/parse_xml.R")
    })
  ); beepr::beep(5)
