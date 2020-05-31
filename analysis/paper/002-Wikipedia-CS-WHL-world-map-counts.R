library(tidyverse)

## Here's a map of countries showing which has the most WP pages for WH sites
library("rnaturalearth")
library("rnaturalearthdata")
world <-
  ne_countries(scale = "medium", returnclass = "sf")

# all WH sites with WP pages (n = 584)
if(!exists("page_data_for_all_pages")){
  page_data_for_all_pages <-
    readRDS(here::here("analysis/data/raw_data/page_data_for_all_pages.rds"))
}

# all WH sites, with and without WP pages (n = 846)
if(!exists("wh_wiki_table")){
wh_wiki_table <-
  readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls"))
}

# expand sites that are in multiple countries
wh_wiki_table_exp <-
wh_wiki_table %>%
  filter(category == "Cultural") %>%
  separate_rows(states_name_en, sep = ",")

# get country of site from location text
# Laos, Czech Republic, Micronesia, Zimbabwe, South Sudan, Chad,
# Central African Republic, Congo, Gabon, Cameroon, Nigeria, Bosnia and Herzegovina
#  Cote d'Ivoire, Sierra Leone, Guyana, Belize,
country_names <-  paste0(c(world$name,
                          "United States",
                          "Czech Republic",
                          "Antigua & Barbuda",
                          "Saint Kitts and Nevis",
                          "Lao"),
collapse="|")

page_data_for_all_pages_location <- # wp data
  page_data_for_all_pages %>%
  mutate(country = str_extract(Location,
                               regex(country_names,
                                     ignore.case=TRUE))) %>%
  mutate(country2 = ifelse(str_detect(Location, "Nigeria"),
                          "Nigeria", # Niger/ier
                          country)) %>%
  mutate(country2 = case_when(
    country2 == "United States" ~ "United States of America",
    country2 == "Czech Republic" ~ "Czechia",
    country2 == "Antigua & Barbuda" ~ "Antigua and Barb.",
    country2 == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    country2 == "Lao" ~ "Lao People's Democratic Republic",
    is.na(country2) ~ "Russia",
    TRUE ~ as.character(country2)))


##### checking

#####

wh_wiki_table_location <- # unesco data
  wh_wiki_table_exp %>%
  mutate(country = str_extract(states_name_en,
                               regex(country_names,
                                     ignore.case=TRUE))) %>%
  mutate(country = case_when(
    country == "United States" ~ "United States of America",
    country == "Czech Republic" ~ "Czechia",
    country == "Antigua & Barbuda" ~ "Antigua and Barb.",
    country == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    country == "Lao" ~ "Lao PDR",
    TRUE ~ as.character(country))) %>%
  # deal with Niger/Nigeria
  mutate(country = ifelse(country == "Niger",
                          states_name_en,
                          country))


wh_wk_country_counts <-  # WP data
  page_data_for_all_pages_location %>%
  count(country2) %>%
  filter(!is.na(country2))

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
           label = str_glue('Median = {median_number_wp_pages_per_country} WP articles/country\nMode = {mode_number_wp_pages_per_country} WP articles/country'),
           size = 2) +
  labs(x = "Number of CS-WHL articles") +
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
    name == "Lao PDR" ~ "Lao People's Democratic Republic" ,
    name == "United States" ~ "United States of America",
    name == "Czech Rep." ~ "Czech Repu", # no idea why
    TRUE ~ as.character(name))) %>%
   left_join(wh_wk_country_counts,
            by = c( 'name' = 'country2')) %>%
  select(name, n, geometry) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  filter(name != "Antartica") # %>% sf::st_drop_geometry() %>% View

###


###

wh_wk_country_count_map <-
  ggplot(data = sf_map_data) +
  geom_sf(aes(fill = n), lwd = 0) +
  scale_fill_viridis_c(# na.value="black",
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

# ratio of WP sites to UNESCO sites per country

source(here::here("analysis/paper/001-UNESCO-CS-WHL-world-map-counts.R"))
library(sf)

sf_map_data_unesco_wk_prop <-
sf_map_data_unesco %>% # official count of actual UNESCO WHL sites
  select(name, n) %>%
  st_drop_geometry() %>%
  arrange() %>%
  mutate(name = case_when(
    name == "Lao PDR" ~ "Lao People's Democratic Republic" ,
    name == "United States" ~ "United States of America",
    name == "Czech Rep." ~ "Czech Repu", # no idea why
    TRUE ~ as.character(name))) %>%
  left_join(sf_map_data %>%  # WP articles for CS-WHL
            select(name, n) %>%
            st_drop_geometry(),
            by = "name") %>%
  mutate(prop_sites_with_wp_articles = n.y / n.x ) %>%
  mutate(prop_sites_with_wp_articles = ifelse(prop_sites_with_wp_articles > 1,
                                              1, prop_sites_with_wp_articles))


sf_map_data_unesco_wk_prop_sf <-
sf_map_data_unesco %>%
  mutate(name = case_when(
    name == "Lao PDR" ~ "Lao People's Democratic Republic" ,
    name == "United States" ~ "United States of America",
    name == "Czech Rep." ~ "Czech Repu", # no idea why
    TRUE ~ as.character(name))) %>%
  left_join(sf_map_data_unesco_wk_prop) %>%
  mutate(prop_sites_with_wp_articles = ifelse(is.na(prop_sites_with_wp_articles),
                                              0,
                                              prop_sites_with_wp_articles))


wh_wk_country_prop_map <-
  ggplot(data = sf_map_data_unesco_wk_prop_sf) +
  geom_sf(aes(fill = prop_sites_with_wp_articles), lwd = 0) +
  scale_fill_viridis_c( #na.value="black",
                       name = "Proportion of CS-WHL\nsites in a country that\nhave Wikipedia\narticles") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_minimal(base_size = 8)  +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  NULL

library(cowplot)

plot_grid(wh_wk_country_count_map +
            annotation_custom(ggplotGrob(wh_wk_country_counts_hist),
                              xmin = -190,
                              xmax = -90,
                              ymin= 10,
                              ymax=-70),
          wh_wk_country_prop_map,
          ncol = 1)

ggsave(here::here("analysis/figures/wh_country_wikipedia_count_prop_map.png"),
       width = 10,
       height = 10,
       dpi = 300)




