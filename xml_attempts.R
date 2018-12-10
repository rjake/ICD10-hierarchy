#ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2019/
#https://stackoverflow.com/questions/48324165/scraping-table-from-xml

#setwd("2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index")
library(tidyverse)
#library(xml2)
#library(XML)
library(rvest)

xml_doc <- "2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index/icd10cm_tabular_2019.xml"
xsd_doc <- "2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index/icd10cm_tabular.xsd"

icd_chapters <-
    xml_find_all(load_xml, ".//chapter") %>% 
    map_df(~{
        data_frame(
            name = xml_text(xml_find_first(.x, ".//name"), trim=TRUE),
            desc = xml_text(xml_find_first(.x, ".//desc"))
        )})

icd_section_ref <-
    data_frame(
        node = xml_find_all(load_xml, ".//sectionRef"),
        first = node %>% xml_attr("first"),
        last = node %>% xml_attr("last")
        #xml_attrs("first")#,
        #desc = xml_text(xml_find_first(.x, ".//desc"))
    )
xml_find_all(load_xml, ".//sectionRef") %>% 
    map(xml_attrs) %>% 
    map_df(~as.list(.))

icd_dx <-
    data_frame(node = xml_find_all(load_xml, ".//diag//diag")) %>% 
    mutate(name = xml_find_first(node, ".//name") %>% xml_text(),
           desc = xml_find_first(node, ".//desc") %>% xml_text()) %>% 
    select(-node) %>% 
    distinct()


icd_ext <-
    tibble(node = xml_find_first(load_xml, ".//extension")) %>% 
    mutate(char = xml_find_all(node, ".//extension") %>% as.list(xml_text()))
,
char = node %>% xml_attr(node, "char")

)

xml_attr(icd_ext$node, ".//extension")
%>% 
    mutate( )

icd_ext_2 <-
    icd_ext %>% 
    mutate(first_3 = node %>% 
               map(~ xml_find_first(., "//extension"))) %>% 
    select(-node) %>% 
    unnest(first_3)


%>% 
    map(~ xml_attr(., "node-id"))

mutate(first_3 = xml_find_all(load_xml, ".//name") %>% as.list())




xml_find_first(load_xml, ".//diag//diag") %>% 
    xml_find_first(".//name") %>% xml_text() 


map_df(~{
    data_frame(
        name = xml_text(xml_find_first(.x, ".//name"), trim=TRUE),
        desc = xml_text(xml_find_first(.x, ".//desc"))
    )})











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








library(tidyverse)
#library()
library(xml2)
xml_doc <- "2019-ICD-10-CM/NCHS_Package_20180605/2019 Table and Index/icd10cm_tabular_2019.xml"

load_xml <-
    read_xml(xml_doc) %>% 
    xml_ns_strip()
sectionRef


icd_chapters <-
    xml_find_all(load_xml, ".//chapter") %>% 
    map_df(~{
        data_frame(
            name = xml_text(xml_find_first(.x, ".//name"), trim=TRUE),
            desc = xml_text(xml_find_first(.x, ".//desc"))
    )})




a <-
    xml_find_all(load_xml, ".//chapter") %>%
    map_df(~{
        xml_find_all(.x, ".//name") %>% 
            map_df(~{
                xml_find_all(.x, ".//desc") %>% 
                    map_df(~{
                        xml_find_all(.x, ".//sectionRef") %>% 
                            map_df(~{
                                data_frame(
                                    fuel = xml_text(xml_find_first(.x, ".//Fuel"), trim=TRUE),
                                    output = xml_double(xml_find_first(.x, ".//Output"))
                                    )
                            }) %>%
                            mutate(hour = xml_double(xml_find_first(.x, ".//sectionRef")))
                    }) %>% 
                    mutate(hour = xml_double(xml_find_first(.x, ".//desc")))
            }) %>% 
            mutate(day = as.Date(xml_text(xml_find_first(.x, ".//name"), trim=TRUE)))
    })



xml_child(load_xml)

load_xml$doc
a <- 
    xml_children(load_xml)

a[[1]]$node

xml_text(load_xml) %>% head()
xml_find_all(load_xml, ".//name") %>% head(100)

a <- 
    xml_find_all(load_xml, ".//diag")
%>% 
    map_chr(~{
        xml_attrs(.x) %>% 
            as.list()
    })

icd_chapters <- 
    xml_find_all(load_xml, ".//chapter")

icd_sections <-
    xml_find_all(load_xml, ".//section")

icd_diag <-
    xml_find_all(load_xml, ".//name")

rows_df <- 
    data_frame(row = seq_along(rows),
               nodeset = rows) %>% 
    head(100)

cells_df <- 
    rows_df %>%
    mutate(col_name_raw = nodeset %>% map(~ xml_name(.)),
           cell_text = nodeset %>% map(~ xml_text(.)),
           i = nodeset %>% map(~ seq_along(.))) %>%
    select(row, i, col_name_raw, cell_text) %>%
    unnest(.preserve = "row")

rows_df <- 
    data_frame(row = seq_along(rows),
               node = rows) %>%
    filter(row == 1) %>% 
    mutate(node_id = node %>% 
               map(~ xml_find_first(., "ancestor::xmlnode")) %>% 
               map(~ xml_attr(., "node-id"))) %>%
    mutate(subnode_id = node %>% map(~ xml_parent(.)) %>% map(~ xml_attr(., "subnode-id"))) %>%
    mutate(text = node %>% map(~ xml_text(.))) %>%
    select(-node) %>% 
    unnest()



rc <-  read_xml(xml_doc, as = "text", encoding = "UTF-8")
xml_to_df <- function(node) {
    x <- c(.name = xml_name(node),
           as.list(xml_attrs(node)),
           .value = xml_text(node)) %>% # data added here instead of in the 'else' statement
        as.data.frame(stringsAsFactors = FALSE)
}

x <- xml_to_df(rc)


library(rvest)
rows <- read_xml(xml_doc) %>% xml_nodes('chapter')
df <- 
    data.frame(
        section = rows %>% xml_attrs("section"))
        ,
        ID = rows %>% xml_attr("name"),
        date = rows %>% xml_attr("desc"),
        body = rows %>% xml_attr("body")
)

x[[2]] %>% 
    select(contains("section")) %>%
    map(bind_rows, .id = "row") %>% 
    bind_rows