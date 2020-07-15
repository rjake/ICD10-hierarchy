# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2019/
# https://stackoverflow.com/questions/48324165/scraping-table-from-xml

library(tidyverse)
library(xml2)

xml_doc <- "input/icd10cm_tabular_2020.xml"#"R/mock_up_example/mock_icd.xml"
load_xml <- read_xml(xml_doc)


# chapters ----
chapter_nodes <- xml_find_all(load_xml, ".//chapter")
icd_chapters <-
  tibble(
    name = xml_text(xml_find_first(chapter_nodes, ".//name")),
    desc = xml_text(xml_find_first(chapter_nodes, ".//desc"))
  )



# sections ----
section_nodes <- xml_find_all(load_xml, ".//section")
icd_section <-
  tibble(
    id = xml_attr(section_nodes, "id"),
    desc = xml_text(xml_find_first(section_nodes, ".//desc"))
  )

# dx ----
parse_dx <- function(n, j) {
  rep_n <- ifelse(n == 3, 1, n - 4)
  xpath <-
    paste0(
      ".//",
      paste(rep("diag", rep_n), collapse = "/")
    )

  dx_nodes <- xml_find_all(load_xml, xpath)

  tibble(
    dx = xml_find_first(dx_nodes, "name") %>% xml_text(),
    desc = xml_find_first(dx_nodes, "desc") %>% xml_text(),
    join = # remove last character
    dx %>%
      str_sub(1, -2) %>%
      str_remove("\\.$")
  ) %>%
    filter(nchar(dx) == n) %>%
    select(
      "first_{n}" := dx,
      "first_{n}_text" := desc,
      "first_{j}" := join
    )
}

icd_dx <-
  parse_dx(8, 7) %>%
  full_join(parse_dx(7, 6)) %>%
  full_join(parse_dx(6, 5)) %>%
  full_join(parse_dx(5, 3)) %>%
  full_join(parse_dx(3, 2)) %>%
  mutate(icd10_code = coalesce(first_8, first_7, first_6, first_5, first_3)) %>%
  select(icd10_code, matches("3"), matches("5"), matches("6"), matches("7"), matches("8")) %>%
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
        str_replace("S12.0-S12.6", paste0("S12.", 0:6, collapse = ", ")) %>%
        str_extract_all("[A-Z]\\d{2}([\\.A-Z0-9]+)?")
  ) %>%
  unnest(applies_to) %>%
  mutate( # create fields to join to in next step
    applies_to = str_remove(applies_to, "\\.$"),
    length = nchar(applies_to),
    first_3 = ifelse(length == 3, str_sub(applies_to, 1, 3), NA),
    first_5 = ifelse(length == 5, str_sub(applies_to, 1, 5), NA),
    first_6 = ifelse(length == 6, str_sub(applies_to, 1, 6), NA)
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
  filter(n > 1) %>% 
  mutate(
    note = str_remove_all(note, "The appropriate 7th character is to be added to|One of the following 7th characters is to be assigned to( each)? code(s in subcategory)?|to designate ((lateral|sever)ity|the stage) of (the disease|glaucoma)")
  )


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


final_diagnoses <- 
  icd_dx %>%
  join_dx(first_3, x3, x3_txt) %>%
  join_dx(first_5, x5, x5_txt) %>%
  join_dx(first_6, x6, x6_txt) %>%
  mutate(
    last_char = coalesce(x3, x5, x6),
    extension = coalesce(x3_txt, x5_txt, x6_txt)
  ) %>%
  select(-starts_with("x")) %>%
  mutate(
    icd10_code = # build dx from longest to shortest
    coalesce(first_8, first_7, first_6, first_5, first_3),
    icd10_code = # pad with X if there is gap before the 8th character
    ifelse(
      !is.na(last_char),
      paste0(str_pad(icd10_code, 7, "right", "X"), last_char),
      icd10_code
    )
  )

head(final_diagnoses, 10)

write_csv(final_diagnoses, "output/icd10_diagnosis_hierarchy.csv")
