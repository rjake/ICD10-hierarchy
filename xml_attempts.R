#ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2019/
#https://stackoverflow.com/questions/48324165/scraping-table-from-xml

setwd("2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index")
library(tidyverse)
library(xml2)
library(XML)
library(rvest)

xml_doc <- "icd10cm_tabular_2019.xml"
xsd_doc <- "icd10cm_tabular.xsd"


a <- html_table(read_xml(xml_doc))
    
get_xml <- xmlParse(xml_doc, parentFirst = T)
    
icd_str <- xmlParse(xml_doc)

icd_nodes <- 
    getNodeSet(icd_str, "/ICD10CM.tabular/chapter")#/section/diag/diag

head(icd_nodes)

icd_detail <-
    xmlToDataFrame(nodes = icd_nodes, homogeneous = T)

icd_header <-
    getNodeSet(icd_str, "/ICD10CM.tabular/*[not(self::data)]")

icd_df <-
    as.data.frame(as.list(setNames(xmlSApply(icd_header, xmlValue), 
                                   xmlSApply(icd_header, xmlName))))

icd_final <-
    merge(icd_df, icd_detail)

rm(icd_str, icd_nodes, icd_detail, icd_header, icd_df)

a <- icd_final %>% head()


tides = system.file("exampleData","tides.xml", package="XML")

tides.str <- xmlParse(tides)
tides.detail <- xmlToDataFrame(nodes = getNodeSet(tides.str, "/datainfo/data/item"))

tides.header <- getNodeSet(tides.str, "/datainfo/*[not(self::data)]")
tides.df <- as.data.frame(as.list(setNames(xmlSApply(tides.header, xmlValue), 
                                           xmlSApply(tides.header, xmlName))))

tides.final <- merge(tides.df, tides.detail)









#method 1
doc <- 
    read_xml(xml_doc) %>% 
    xml_ns_strip()

'<section id="S40-S49">
<desc>Injuries to the shoulder and upper arm (S40-S49)</desc>
<diag>
<name>S40</name>
<desc>Superficial injury of shoulder and upper arm</desc>
<sevenChrNote>
<note>The appropriate 7th character is to be added to each code from category S40</note>
</sevenChrNote>
<sevenChrDef>
<extension char="A">initial encounter</extension>
<extension char="D">subsequent encounter</extension>
<extension char="S">sequela</extension>
</sevenChrDef>
<diag>
<name>S40.0</name>
<desc>Contusion of shoulder and upper arm</desc>
<diag>
<name>S40.01</name>
<desc>Contusion of shoulder</desc>
<diag>
<name>S40.011</name>
<desc>Contusion of right shoulder</desc>
</diag>
'

a <- 
    xml_find_all(doc, ".//DailyData") %>%
    map_df(~{
        xml_find_all(.x, ".//HourlyData") %>% 
            map_df(~{
                xml_find_all(.x, ".//FuelTotal") %>% 
                    map_df(~{
                        data_frame(
                            fuel = xml_text(xml_find_first(.x, ".//Fuel"), trim=TRUE),
                            output = xml_double(xml_find_first(.x, ".//Output"))
                        )
                    }) %>% 
                    mutate(hour = xml_double(xml_find_first(.x, ".//Hour")))
            }) %>% 
            mutate(day = as.Date(xml_text(xml_find_first(.x, ".//Day"), trim=TRUE)))
    })

#method 2
library(xml2)
library(xslt)
library(rvest)
library(tidyverse)

doc <- read_xml(xml_doc)

xsl <- read_xml("http://reports.ieso.ca/docrefs/stylesheet/GenOutputbyFuelHourly_HTML_t1-1.xsl")

xml_xslt(doc, xsl) %>% 
    html_node(xpath=".//table[contains(., 'HYDRO')]") %>% 
    html_table(header=TRUE, fill=TRUE) %>% 
    tbl_df()

a <-  xmlToList(xsd_doc)
b <- a$complexType$sequence$element

df <- 
    do.call(rbind, 
            xmlToList(xsd_doc)$complexType$sequence) %>% 
    data.frame(row.names=NULL)
