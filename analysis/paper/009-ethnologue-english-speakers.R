# highly iconic sites in the Western canon of culture history

# Only one of the controversial sites discussed above is in the group of articles where the talk page is longer than the article itself, and none of the site on the UNESCO heritage in danger list are in this group.


library(tidyverse)
library(rvest)

el <- here::here("analysis/data/raw_data/English _ Ethnologue.htm")

number_of_speakers <-
el %>%
  read_html() %>%
  html_nodes(".views-field-field-population-1 .field-content") %>%
  html_text() %>%
  str_remove_all(" in .*") %>%
  parse_number()

country <-
  el %>%
  read_html() %>%
  html_nodes(".views-field-field-population-1 .field-content") %>%
  html_text() %>%
  str_remove_all(", all users.*") %>%
  str_remove_all(".* in ")


tibble(country = country,
       number_of_speakers= number_of_speakers) %>%
  arrange(desc(number_of_speakers)) %>% View
