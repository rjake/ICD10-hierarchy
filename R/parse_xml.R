# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2019/
# https://stackoverflow.com/questions/48324165/scraping-table-from-xml

library(tidyverse)
library(xml2)

xml_doc <- 
  "input/icd10cm_tabular_2020.xml"
  #"R/mock_up_example/mock_icd.xml"

load_xml <- read_xml(xml_doc)


# chapters ----
chapter_nodes <- xml_find_all(load_xml, ".//chapter")
icd_chapters <-
  tibble(
    chapter = xml_text(xml_find_first(chapter_nodes, ".//name")),
    chapter_desc = xml_text(xml_find_first(chapter_nodes, ".//desc"))
  ) %>% 
  mutate(
    category = str_remove_all(chapter_desc, ".*\\(|\\)")#,
    #chapter_desc = trimws(str_extract(chapter_desc, "[^\\(]+"))
  ) %>% 
    separate_rows(category)



# sections ----
section_nodes <- xml_find_all(load_xml, ".//section")
icd_section <-
  tibble(
    id = xml_attr(section_nodes, "id"),
    desc = xml_text(xml_find_first(section_nodes, ".//desc"))
  )

# dx ----
parse_dx <- function(n, title, join_field) {
  # need to grab //diag/diag/diag x # of times
  rep_n <- ifelse(n == 3, 1, n - 2)
  
  xpath <-
    paste0(
      ".//",
      paste(rep("diag", rep_n), collapse = "/")
    )

  # pulls back description etc
  dx_nodes <- xml_find_all(load_xml, xpath)

  
  tibble(
    dx = xml_find_first(dx_nodes, "name") %>% xml_text(),
    desc = xml_find_first(dx_nodes, "desc") %>% xml_text(),
    join = 
      str_sub(dx, 1, -2) %>% # remove last character
      str_remove("\\.$")  # remove trailing period
  ) %>%
    # dx_nodes brings back all dx below this level
    # this will ensure we just have the codes of length n
    filter(nchar(str_remove(dx, "\\.")) == n) %>% 
    # rename columns
    select(
      "{title}" := dx,
      "{title}_desc" := desc,
      "{join_field}" := join # will be used in full_joins()
    )
}

icd_dx <-
  parse_dx(7, "extension", "subcategory_3") %>%
  full_join(parse_dx(6, "subcategory_3", "subcategory_2")) %>%
  full_join(parse_dx(5, "subcategory_2", "subcategory_1")) %>%
  full_join(parse_dx(4, "subcategory_1", "category")) %>%
  full_join(parse_dx(3, "category", "supercategory")) %>%
  mutate(
    icd10_code = 
      coalesce(extension, subcategory_3, subcategory_2, subcategory_1, category)
  ) %>%
  select(
    icd10_code, 
    starts_with("category"), 
    matches("1"), 
    matches("2"), 
    matches("3"), 
    matches("extension")
  ) %>%
  arrange(icd10_code)


# extensions ----
ext_node <- xml_find_all(load_xml, ".//diag/sevenChrDef/extension")
icd_extensions <-
  tibble(
    name = xml_find_first(ext_node, "../../name") %>% xml_text(),
    note = xml_find_first(ext_node, "../../sevenChrNote/note") %>% xml_text(),
    char = xml_attr(ext_node, "char"),
    text = xml_text(ext_node)
  )

# full dataset
prep_extensions <-
  icd_extensions %>%
  mutate( # extract referenced code patterns
    applies_to =
      note %>%
        str_remove_all("O30") %>%
        # expand to S12.1, S12.2, etc
        str_replace("S12.0-S12.6", paste0("S12.", 0:6, collapse = ", ")) %>%
        # pull out dx codes
        str_extract_all("[A-Z]\\d{2}([\\.A-Z0-9]+)?")
  ) %>%
  unnest(applies_to) %>%
  mutate( # create fields to join to in next step
    applies_to = str_remove(applies_to, "\\.$"),
    length = nchar(applies_to),
    category = ifelse(length == 3, str_sub(applies_to, 1, 3), NA),
    subcategory_1 = ifelse(length == 5, str_sub(applies_to, 1, 5), NA),
    subcategory_2 = ifelse(length == 6, str_sub(applies_to, 1, 6), NA)
  )



# see what it looks like
prep_extensions %>%
  group_by(note) %>%
  summarise(
    n = n_distinct(applies_to),
    codes = paste(unique(applies_to), collapse = ", "),
    chars = paste(unique(char), collapse = ", ")
  ) %>%
  ungroup() %>%
  filter(n > 1) #%>% 
  # mutate(
  #   note = str_remove_all(note, "The appropriate 7th character is to be added to|One of the following 7th characters is to be assigned to( each)? code(s in subcategory)?|to designate ((lateral|sever)ity|the stage) of (the disease|glaucoma)")
  # )


join_dx <- function(df, join_var, v1, v2) {
  df %>%
    left_join(
      prep_extensions %>%
        distinct(
          {{join_var}}, 
          {{v1}} := char, 
          {{v2}} := text
        ) %>%
        drop_na()
    )
}


#final_diagnoses <- 
  icd_dx %>%
  # odd but need to bring over the code & text each time so not overwritten
  join_dx(category, x3, x3_desc) %>%
  join_dx(subcategory_1, x5, x5_desc) %>%
  join_dx(subcategory_2, x6, x6_desc) %>%
  mutate(
    extension = coalesce(str_sub(extension, 8), x3, x5, x6),
    extension_desc = coalesce(extension_desc, x3_desc, x5_desc, x6_desc)
  ) %>%
  select(-starts_with("x")) %>% 
  mutate(
    icd10_code = # recompile icd10_code
      case_when(
        # some codes are 8 digits long w/o extension, keep as-is
        nchar(icd10_code) == 8 ~ icd10_code, 
        # pad code with Xs if less than 7 digits
        !is.na(extension) ~ paste0(str_pad(icd10_code, 7, "right", "X"), extension),
        TRUE ~ icd10_code
      )
  ) %>%
  left_join(icd_chapters) %>% 
  #  select(icd10_code, starts_with("x")) %>% 
  select(icd10_code, starts_with("chap"), everything()) %>% 
  fill(chapter) %>% 
  fill(chapter_desc)

nrow(final_diagnoses)
# 75429

head(final_diagnoses, 10)

write_csv(final_diagnoses, "output/icd10_diagnosis_hierarchy.csv")
