library(tidyverse)

## Here's a map of countries showing which has the most WP pages for WH sites
library("rnaturalearth")
library("rnaturalearthdata")
world <-
  ne_countries(scale = "medium", returnclass = "sf")

page_data_for_all_pages <-
  read_rds(here::here("analysis/data/raw_data/page_data_for_all_pages.rds"))

# all WH sites, with and without WP pages (n = 846)
wh_wiki_table <-
  readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls"))

# get country of site from location text
# Laos, Czech Republic, Micronesia, Zimbabwe, South Sudan, Chad,
# Central African Republic, Congo, Gabon, Cameroon, Nigeria, Bosnia and Herzegovina
#  Cote d'Ivoire, Sierra Leone, Guyana, Belize,
country_names <-  paste(c(world$name,
                          "United States",
                          "Czech Republic",
                          "Antigua & Barbuda",
                          "Saint Kitts and Nevis",
                          "Lao"
),
collapse="|")

page_data_for_all_pages_location <-
  page_data_for_all_pages %>%
  mutate(country = str_extract(Location,
                               regex(country_names,
                                     ignore.case=TRUE))) %>%
  mutate(country = case_when(
    country == "United States" ~ "United States of America",
    country == "Czech Republic" ~ "Czechia",
    country == "Antigua & Barbuda" ~ "Antigua and Barb.",
    country == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    country == "Lao" ~ "Lao People's Democratic Republic",
    TRUE ~ as.character(country)))


wh_wiki_table_location <-
  wh_wiki_table %>%
  mutate(country = str_extract(states_name_en,
                               regex(country_names,
                                     ignore.case=TRUE))) %>%
  mutate(country = case_when(
    country == "United States" ~ "United States of America",
    country == "Czech Republic" ~ "Czechia",
    country == "Antigua & Barbuda" ~ "Antigua and Barb.",
    country == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    country == "Lao" ~ "Lao PDR",
    TRUE ~ as.character(country)))

wh_wk_country_counts <-
  page_data_for_all_pages_location %>%
  count(country) %>%
  filter(!is.na(country))

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size,
           base_family = base_family) %+replace%
    theme(panel.grid = element_blank() )
}

median_number_wp_pages_per_country <-
  median(wh_wk_country_counts$n)

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

mode_number_wp_pages_per_country <-
  round(estimate_mode(wh_wk_country_counts$n), 1)

wh_wk_country_counts_hist <-
  ggplot(wh_wk_country_counts,
         aes(n)) +
  geom_histogram() +
  annotate("text", x = 15, y = 30,
           label = str_glue('Median = {median_number_wp_pages_per_country} WP pages/country\nMode = {mode_number_wp_pages_per_country} WP pages/country'),
           size = 2) +
  theme_nogrid(12)

wh_wk_country_counts_bar <-
  ggplot(wh_wk_country_counts,
         aes(reorder(country, n),
             n)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 6) +
  xlab("")

sf_map_data <-
  world %>%
  mutate(name = case_when(
    name == "United States" ~ "United States of America",
    name == "Lao PDR" ~ "Lao People's Democratic Republic" ,
    TRUE ~ as.character(name))) %>%
  left_join(wh_wk_country_counts,
            by = c( 'name' = 'country')) %>%
  select(name, n, geometry) %>%
 # mutate(n = ifelse(is.na(n), 0, n)) %>%
  filter(name != "Antartica")

wh_wk_country_count_map <-
  ggplot(data = sf_map_data) +
  geom_sf(aes(fill = n), lwd = 0) +
  scale_fill_viridis_c(na.value="black",
                       name = "Number of\nWikipedia articles\non CS-WHL") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_minimal(base_size = 8)  +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
 #ggtitle("Cultural Sites on the UNESCO World Heritage list with Wikipedia pages",
 #        subtitle = str_glue("The top five are {wh_wk_country_counts %>%
 #                                                arrange(-n) %>%
 #                                                slice(1:5) %>%
 #                                                pull(country) %>%
 #                                                str_flatten(collapse = ', ')}")) +
  NULL

wh_wk_country_count_map +
  annotation_custom(ggplotGrob(wh_wk_country_counts_hist),
                    xmin = -190,
                    xmax = -90,
                    ymin= 10,
                    ymax=-70)



ggsave(here::here("analysis/figures/wh_country_wikipedia_count_map.png"),
       width = 10,
       height = 6,
       dpi = 300)

