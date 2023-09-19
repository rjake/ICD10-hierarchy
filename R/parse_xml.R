# https://stackoverflow.com/questions/48324165/scraping-table-from-xml
# workspace ----
library(tidyverse)
library(stringi)
library(xml2)
library(readxl)
library(glue)
library(vroom)

# requires 'year' variable from  R/download-and-unzip-files.R
# year <- 2014 
message("working on ", year, ' - - - - - - -') # print heading

xml_path <- paste0("file-history/icd10_", year, ".xml")

# bring in xml
load_xml <- read_xml(xml_path)

# chapters ----
chapter_nodes <- xml_find_all(load_xml, ".//chapter")
icd_chapters <-
  tibble(
    chapter = xml_text(xml_find_first(chapter_nodes, ".//name")),
    chapter_desc = xml_text(xml_find_first(chapter_nodes, ".//desc"))
  ) %>% 
  mutate(
    chapter = str_remove_all(chapter_desc, ".*\\(|\\)"),
    category = chapter,
    #chapter_desc = trimws(str_extract(chapter_desc, "[^\\(]+"))
  ) %>% 
    separate_rows(category)



# sections ----
section_nodes <- xml_find_all(load_xml, ".//section")
icd_sections <-
  tibble(
    section = xml_attr(section_nodes, "id"),
    section_desc = xml_text(xml_find_first(section_nodes, ".//desc"))
  ) %>% 
  mutate(
    section = str_remove_all(section_desc, ".*\\(|\\)"),
    category = section
  ) %>% 
  separate_rows(category) %>% 
  group_by(category) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

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

all_sub_category <-
  parse_dx(7, "extension", "subcategory_3") %>%
  full_join(parse_dx(6, "subcategory_3", "subcategory_2"), by = "subcategory_3") %>%
  full_join(parse_dx(5, "subcategory_2", "subcategory_1"), by = "subcategory_2") %>%
  full_join(parse_dx(4, "subcategory_1", "category"), by = "subcategory_1") %>%
  full_join(parse_dx(3, "category", "supercategory"), by = "category")

# as ascii ----
standardize_ascii <- function(x) {
  x |> 
    tolower() |> 
    stringi::stri_trans_general("latin-ascii")
}

as_ascii <-
  all_sub_category %>% 
  #  filter(category == "S42") %>% select(icd10_code, matches("[y123]_desc")) %>% 
  mutate(across(matches("cat.*desc"), standardize_ascii))


icd_dx <-
  as_ascii |>
  mutate(
    icd10_code = 
      coalesce(extension, subcategory_3, subcategory_2, subcategory_1, category)
  ) %>%
  #filter(str_detect(icd10_code, "S42")) %>%
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
    text = xml_text(ext_node) |> standardize_ascii()
  )

# full dataset
prep_extensions <-
  icd_extensions %>%
  mutate( # extract referenced code patterns
    applies_to =
      note %>% # 'The appropriate 7th character is to be added to each code from category M1A, S42, T49.123' %>%
        str_remove_all("O30") %>%
        # expand to S12.1, S12.2, etc
        str_replace("S12.0-S12.6", paste0("S12.", 0:6, collapse = ", ")) %>%
        # pull out dx codes
        str_extract_all("[A-Z](\\d\\w)([\\.A-Z0-9]+)?")
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
  filter(str_detect(name, "S4")) %>% 
  group_by(note) %>%
  summarise(
    n = n_distinct(applies_to),
    codes = paste(unique(applies_to), collapse = ", "),
    chars = paste(unique(char), collapse = ", ")
  ) %>%
  ungroup() #%>% filter(n > 1) #%>% 
  # mutate(
  #   note = str_remove_all(note, "The appropriate 7th character is to be added to|One of the following 7th characters is to be assigned to( each)? code(s in subcategory)?|to designate ((lateral|sever)ity|the stage) of (the disease|glaucoma)")
  # )


join_dx <- function(df, join_var, loc) {
  # df <- icd_dx %>% filter(icd10_code == "S42.271"); join_var <- quo(subcategory_2); loc <- 6; 
  # df <- icd_dx %>% filter(icd10_code == "S42.271"); join_var <- quo(subcategory_1); loc <- 5; 
  # df <- icd_dx %>% filter(icd10_code == "S42.271"); join_var <- quo(category); loc <- 3; 
  
  df %>%
    # filter out any extension field that already has a value
    filter(if_all(starts_with("x"), ~is.na(.x))) %>% 
    inner_join(
      prep_extensions %>%
        filter(length == loc) %>% 
        distinct(
          {{join_var}}, 
          "x{{loc}}" := char, 
          "x{{loc}}_desc" := text
        ) %>% 
        drop_na(),#, by = deparse(substitute(join_var)) # creates string
      relationship = "many-to-many"
    ) %>% 
    select(icd10_code, tail(names(.), 2)) %>% 
    distinct() |> 
    suppressMessages()
}

# with extensions ----
with_extensions <-
  icd_dx %>%
  # odd but need to bring over the code & text each time so not overwritten
  left_join(join_dx(., subcategory_2, 6), by = "icd10_code", relationship = "many-to-many") %>%
  left_join(join_dx(., subcategory_1, 5), by = "icd10_code", relationship = "many-to-many") %>%
  left_join(join_dx(., category, 3), by = "icd10_code", relationship = "many-to-many") %>%
  #filter(icd10_code == "S42.261") %>% 
  mutate(
    extension = coalesce(str_sub(extension, 8), x3, x5, x6),
    extension_desc = coalesce(extension_desc, x3_desc, x5_desc, x6_desc),
    icd10_code = 
      ifelse(
        test = nchar(icd10_code) == 3 & !is.na(extension), 
        yes = paste0(icd10_code, "."),
        no = icd10_code
      )
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
  ) |> 
  distinct()

paste(nrow(with_extensions) - n_distinct(with_extensions$icd10_code), "dupes")
#count(with_extensions, icd10_code, sort = TRUE)

# final join ----
final_joins <- 
  with_extensions %>%
  left_join(icd_chapters, by = "category") %>% 
  #  select(icd10_code, starts_with("x")) %>% 
  arrange(icd10_code) %>% 
  fill(chapter) %>% 
  fill(chapter_desc) %>% 
  left_join(icd_sections, by = "category") %>% 
  fill(section) %>% 
  fill(section_desc) %>% 
  mutate(
    description =
      paste(
        coalesce(
          subcategory_3_desc, 
          subcategory_2_desc, 
          subcategory_1_desc, 
          category_desc
        ),
        replace_na(extension_desc, "")
      ) %>% tolower()
  ) %>%
  select(
    icd10_code, description, starts_with("chap"), starts_with("sect"), everything()
  )



#' make hyphens, parentheses literal
#' @examples 
#' preserve_punctuation('a-b#')
# preserve_punctuation <- function(x) {
#   str_replace_all(
#     string = x,
#     pattern = "([[:punct:]])", 
#     replacement = 
#       str_c(
#         "\\", "\\", # 2 escaped '\' 
#         "\\1"       # capture group
#       )
#   ) 
# }


#' find text difference between two strings
#' @examples 
#' find_difference(x = '1 2 3 4-6', ref = '1 3')
find_difference <- function(x, ref) {
  clean_ref <-
    coalesce(ref, "") |> 
    str_replace_all(',', " ") |>
    trimws() |> 
    str_split(" ") 
  
  clean_x <- 
    coalesce(x, "") |> 
    str_replace_all(",", " ") |> 
    str_split(" ") 
  
  map2(clean_x, clean_ref, setdiff) |> 
    map_chr(paste, collapse = " ") |> 
    str_replace_all(" {2,}", " ") |> 
    trimws()
}


# final diagnoses ----
with_diff_cols <-
  final_joins %>% 
  #filter(category == "S42") %>% select(icd10_code, matches("[y123]_desc")) %>% 
  mutate(
    subcategory_1_diff = find_difference(subcategory_1_desc, category_desc),
    subcategory_2_diff = find_difference(subcategory_2_desc, subcategory_1_desc),
    subcategory_3_diff = find_difference(subcategory_3_desc, subcategory_2_desc)
  ) |> 
  mutate_all(trimws)#%>%select(ends_with("diff"))
  

# with_diff_cols |> filter(icd10_code == "S52.271S") |> gather() |> print(n = Inf)


append_higher_dx <- function(field, ...){
  # TODO:
    # Note: Using an external vector in selections is ambiguous.
    # i Use `all_of(field)` instead of `field` to silence this message.
    # i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
  with_diff_cols |> 
    drop_na(field) |> 
    filter(!get(field) %in% with_diff_cols$icd10_code) |> 
    select(
      icd10_code = field,
      description = paste0(field, "_desc"),
      chapter:field,
      "{field}" := field,
      "{field}_desc" := paste0(field, "_desc"),
      ...
    ) |>
    distinct() |>
    mutate(
      # remove ending commas from description fields, ex dx: O10.10
      across(matches("desc"), ~str_remove(.x, ",$"))
    )
}

final_diagnosis <- 
  with_diff_cols |> 
  bind_rows(
    append_higher_dx("subcategory_3", subcategory_1_diff, subcategory_2_diff, subcategory_3_diff),
    append_higher_dx("subcategory_2", subcategory_1_diff, subcategory_2_diff),
    append_higher_dx("subcategory_1", subcategory_1_diff),
    append_higher_dx("category")
  ) |>
  arrange(icd10_code) |> 
  mutate(
    year = year,
    derived_code_ind = as.integer(!icd10_code %in% with_diff_cols$icd10_code),
    update_date = Sys.Date()
  )

# check for missing dx codes
final_diagnosis |> count(derived_code_ind)

# check for dupes
paste(nrow(final_diagnosis) - n_distinct(final_diagnosis$icd10_code), "dupes")
# count(final_diagnosis, icd10_code, sort = TRUE)
# filter(final_diagnosis, icd10_code == "S42.271A") |> distinct()
# waldo::compare(.Last.value[1,], .Last.value[2,])


#filter(with_diff_cols, icd10_code == "C44.10")
#filter(final_diagnosis, icd10_code == "C44.10")


# write to csv
vroom::vroom_write(
  final_diagnosis, 
  glue("output/icd10_diagnosis_hierarchy_{year}.csv"), 
  delim = ",",
  na = ""
)
#write_csv(final_diagnosis, "~/github/Chop-Data-Blocks/data/lookup_diagnosis_icd10.csv", na = "")
