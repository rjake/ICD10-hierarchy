library(tidyverse)

# given this data set of pt visits, I want to create dx groups
raw_dx <- read.csv("https://raw.githubusercontent.com/rjake/ICD10-hierarchy/master/output/icd10_diagnosis_hierarchy.csv")

# Y codes are pretty rare, while S codes are very common
# I want a min of 20 patients in each group and if that thersheold isn't met
# to keep rolling up the dx until I have at least 20 ppl. If there aren't enough
# ppl in S42.415A then try S42.415 if that doesn't have 20 ppl, try S42.41, etc
# the code should max out at 3 characters (S43)

# I was imagining a function with these params
df <- raw_dx
col <- "icd10_code"
len <- max(nchar(df[[col]])) #- 1
threshold <- 20

# this will hold my results
res <- 
  tibble(
    field = character(0),
    shorten = character(0),
    n = integer(0)
  )


while(len >= 3) { # while the dx is at least 3 characters long 
  # this is going to pair the orig dx with a rolled up 
  field_counts <-
    tibble(
      field = df[[col]],
      shorten = substring(field, 1, len)
    ) %>% 
    filter(!field %in% res$field) %>% 
    mutate(shorten = substring(field, 1, len)) %>% 
    add_count(shorten)
  
  # find the groups that had >= threshold
  if (max(field_counts$n) >= threshold) {
    high_counts <- filter(field_counts, n >= threshold)
    res <- bind_rows(res, high_counts)
    
    # if any remain and we're at the min of 3 characters, add them as-is
    if (nrow(field_counts) > nrow(high_counts) & len == 3) {
      res <- bind_rows(res, filter(field_counts, !field %in% high_counts$field))
    }
  } else if (len == 3) {
    res <- bind_rows(res, field_counts)
  }
  # reset len
  len <- len - 1
}


# an example plot of how the dx cohorts rolled up
res %>% 
  mutate(
    start = 
      substr(field, 1, 1) %>% 
      fct_infreq(),
    nchar = nchar(shorten)
  ) %>% 
  ggplot(aes(y = nchar)) +
  geom_bar() +
  facet_wrap(~start)
