library(tidyverse)

wh_unesco <- readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls")) %>%
  filter(category %in% c("Cultural"))

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size,
           base_family = base_family) %+replace%
    theme(panel.grid = element_blank() )
}


wh_unesco_countries <-
  wh_unesco %>%
  separate_rows(states_name_en, convert = TRUE, sep = ",") %>%
  mutate(country = case_when(
    states_name_en == "Bolivia (Plurinational State of)"  ~ "Bolivia",
    states_name_en == "Cabo Verde"  ~ "Republic of Cabo Verde",
    states_name_en == "Czechia"  ~ "Czech Republic",
    states_name_en == "Democratic People's Republic of Korea"  ~ "Republic of Korea",
    states_name_en == "Gambia (the)"  ~ "The Gambia",
    states_name_en == "Holy See"  ~ "Vatican",
    states_name_en == "Iran (Islamic Republic of)"  ~ "Iran",
    states_name_en == "Lao People's Democratic Republic"  ~ "Lao PDR",
    states_name_en == "Micronesia (Federated States of)"  ~ "Federated States of Micronesia",
    states_name_en == "Syrian Arab Republic"  ~ "Syria",
    states_name_en == "the former Yugoslav Republic of Macedonia"  ~ "Macedonia",
    states_name_en == "United Kingdom of Great Britain and Northern Ireland"  ~ "United Kingdom",
    states_name_en == "United Republic of Tanzania"  ~ "Tanzania",
    states_name_en == "United States of America"  ~ "United States",
    states_name_en == "Venezuela (Bolivarian Republic of)"  ~ "Venezuela",
    states_name_en == "United States of America"  ~ "United States",
    states_name_en == "Viet Nam"  ~ "Vietnam",
    states_name_en == "Republic of Moldova"  ~ "Moldova",
    TRUE ~ as.character(states_name_en)
  ))

wh_unesco_countries_count <-
  wh_unesco_countries %>%
  count(country)

median_number_unesco_sites_per_country <-
  median(wh_unesco_countries_count$n)

mode_number_unesco_sites_per_country <-
  round(estimate_mode(wh_unesco_countries_count$n), 1)

wh_unesco_country_counts_hist <-
  ggplot(wh_unesco_countries_count,
         aes(n)) +
  geom_histogram() +
  annotate("text", x = 30, y = 30,
           label = str_glue('Median = {median_number_unesco_sites_per_country} CS-WHL per country\nMode = {mode_number_unesco_sites_per_country} CS-WHL per country'),
           size = 2.5) +
  xlab("Number of sites") +
  theme_nogrid(8)

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")

sf_map_data_unesco <-
  world %>%
  left_join(wh_unesco_countries_count,
            by = c( 'name_long' = 'country')) %>%
  select(name, n, geometry) %>%
  # mutate(n = ifelse(is.na(n), 0, n)) %>%
  filter(name != "Antartica")

wh_country_unesco_count_map <-
  ggplot(data = sf_map_data_unesco) +
  geom_sf(aes(fill = n), lwd = 0) +
  scale_fill_viridis_c(na.value="black",
                       name = "Number\nof sites") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_minimal(base_size = 8)  +
  theme(legend.direction = "horizontal", legend.position = "bottom")
  # ggtitle("Cultural Sites on the UNESCO World Heritage list",
  #        subtitle = "from https://whc.unesco.org/en/syndication")

wh_country_unesco_count_map +
  annotation_custom(ggplotGrob(wh_unesco_country_counts_hist),
                    xmin = -190,
                    xmax = -90,
                    ymin= 10,
                    ymax=-70)

ggsave(here::here("analysis/figures/wh_country_unesco_count_map.png"),
       width = 10,
       height = 6,
       dpi = 300)

