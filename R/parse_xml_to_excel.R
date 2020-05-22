library(tidyverse)

# xml file: ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2020/icd10cm_tabular_2020.xml)

# txt file:  https://www.cob.cms.hhs.gov/Section111/assets/section111/icd10.dx.codes.htm


full_dx_list <-
  readxl::read_excel("input/Section111ValidICD10-Apr2020.xlsx", guess_max = 7e4) %>%
  select(icd10_code = 1, description = 2) %>% 
  mutate(
    icd10_code = str_replace( # add a "." if more than 3 char
      icd10_code, 
      "(.{3})(.+)", 
      "\\1.\\2"
    )
  )

filter(full_dx_list, str_detect(icd10_code, "A33|U07|S42.001"))
  
# open in XML in Excel then save as CSV
get_data <-
  read.csv(
    file = "input/icd10cm_tabular_2020.csv",
    stringsAsFactors = FALSE,
    na.strings = ""
  ) %>%
  mutate(row = row_number())


base_df <-
  get_data %>%
  mutate(row = row_number()) %>%
  # slice(1:120) %>%
  select(row, everything(), -c(version:note5))


keep_filled_only <- function(df, ignore = ...) {
  df %>%
    filter_at(vars(-{{ignore}}), any_vars(!is.na(.)))
}


raw_sections <-
  base_df %>%
  select(row, id, sectionRef:last) %>%
  keep_filled_only(ignore = row)


raw_name_desc <-
  get_data %>%
  select(row, matches("id6|name|desc")) %>%
  keep_filled_only(ignore = row)


# prep_name_desc <-
#   raw_name_desc %>%
#   select(
#     section = name,
#     section_text = desc,
#     region = id6,
#     region_text = desc7,
#     first_3 = name8,
#     first_3_text = desc9,
#     first_5 = name10,
#     first_5_text = desc11,
#     first_6 = name14,
#     first_6_text = desc15,
#     first_7 = name20,
#     first_7_text = desc21,
#     first_8 = name23,
#     first_8_text = desc24,
#   ) %>%
#   fill(section, section_text, region, region_text, first_3) %>%
#   group_by(first_3) %>% fill(first_3_text) %>% ungroup() %>%
#   group_by(first_3_text) %>% fill(first_5) %>% ungroup() %>%
#   group_by(first_5) %>% fill(first_6_text) %>% ungroup() %>%
#   group_by(first_6_text) %>% fill(first_7) %>% ungroup() %>%
#   group_by(first_7) %>% fill(first_7_text) %>%
#   ungroup() %>%
#   filter_at(vars(matches("first.*text")), any_vars(!is.na(.)))

offset_select <- function(x, ...) {
  start <- x
  end <- 13 - x

  raw_name_desc %>%
    slice(start:(n() - end)) %>%
    select(...)
}


prep_name_desc <-
  bind_cols(
    offset_select(1,section = name),
    offset_select(2, section_text = desc),
    offset_select(3, region = id6),
    offset_select(3, region_text = desc7),
    offset_select(4, first_3 = name8),
    offset_select(5, first_3_text = desc9),
    offset_select(6, first_5 = name10),
    offset_select(7, first_5_text = desc11),
    offset_select(8, first_6 = name14),
    offset_select(9, first_6_text = desc15),
    offset_select(10, first_7 = name20),
    offset_select(11, first_7_text = desc21),
    offset_select(12, first_8 = name23),
    offset_select(12, first_8_text = desc24)
  ) %>%
  keep_filled_only(ignore = region) %>%
  filter_at(vars(-region), any_vars(!is.na(.))) %>%
  mutate(row = row_number()) %>%
  keep_filled_only(ignore = row)


fill_name_desc <-
  prep_name_desc %>%
  fill(section, section_text, region, region_text, first_3) %>%
  mutate(section = paste(as.roman(section), section_text, sep = " - ")) %>%
  select(row, everything(), -section_text) %>%
  group_by(first_3)      %>% fill(first_3_text) %>% #ungroup() %>%
  group_by(first_3_text) %>% fill(first_5)      %>% #ungroup() %>%
  group_by(first_5)      %>% fill(first_5_text) %>% #ungroup() %>%
  group_by(first_5_text) %>% fill(first_6)      %>% #ungroup() %>%
  group_by(first_6)      %>% fill(first_6_text) %>% #ungroup() %>%
  group_by(first_6_text) %>% fill(first_7)      %>% #ungroup() %>%
  group_by(first_7)      %>% fill(first_7_text) %>%
  ungroup()


raw_extensions <- # the 8th character in code, ex: S42.___A
  get_data %>%
  select(matches("note|exten|char")) %>%
  mutate( # these fields are mutually exclusive
    note = coalesce(note32, note42, note58),
    extension = coalesce(extension, extension43, extension59),
    last_char = coalesce(char, char44, char60)
  ) %>%
  select(note, extension, last_char) %>%
  filter_all(any_vars(!is.na(.))) %>%
  fill(note) %>%
  drop_na()

prep_extensions <- 
  raw_extensions %>%
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
    codes = paste(unique(applies_to), collapse = ", ")
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
        distinct({{join_var}}, {{v1}} := last_char, {{v2}} := extension) %>%
        drop_na()
    )
}


final_diagnoses <-
  fill_name_desc %>%
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
