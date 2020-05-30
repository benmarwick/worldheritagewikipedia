
# Spatial patterns

# all WH sites with WP pages (n = 584)
if(!exists("page_data_for_all_pages")){
  page_data_for_all_pages <-
    readRDS(here::here("analysis/data/raw_data/page_data_for_all_pages.rds"))
}

# load the random pages revision histories
if(!exists("ten_k_random_wp_pages_lst")){
  ten_k_random_wp_pages_lst <-
    readRDS(here::here("analysis/data/raw_data/random_page_data_lst.rds"))
}

if(!exists("wh_unesco_danger")){
  wh_unesco_danger <- readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls")) %>%
    filter(category %in% c("Cultural")) %>%
    filter(danger == 1)
}

# all WH sites, with and without WP pages (n = 846)
if(!exists("wh_wiki_table")){
  wh_wiki_table <-
    readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls"))
}

## IP addresses


ip_regex <- "(?(?=.*?(\\d+\\.\\d+\\.\\d+\\.\\d+).*?)(\\1|))"

get_ip_addresses <- function(x){
  re <- regexpr(
    ip_regex,
    x, perl = TRUE)
  regmatches(x, re)
}

# get locations from IP addresses
library(rgeolocate)
ip_db <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")

# How many IP edits on each page?

# analyse the countries of IP address vs country of WH site
# wh sites on wp
revision_history_page_details <-
  tibble(revision_history_page_details = map(page_data_for_all_pages$page_info_t,
                                             ~.x$revision_history_page_details)) %>%
  mutate(Site =         page_data_for_all_pages$Site,
         Country =      page_data_for_all_pages$country,
         rh_n_editors = map_int(revision_history_page_details, ~n_distinct(.x$rh_user)),
         rh_n_edits =   map_int(revision_history_page_details, ~nrow(.x)),
         rh_user_simpson_idx = page_data_for_all_pages$rh_user_simpson_idx,
         rh_user_bot_prop = page_data_for_all_pages$rh_user_bot_prop,
         rh_revert_prop = page_data_for_all_pages$rh_revert_prop)

# this takes a few moments...
revision_history_page_details_ip_addresses <-
  revision_history_page_details %>%
  mutate(edits_from_ip_addresses = map(revision_history_page_details,
                                       ~.x %>%
                                         filter(get_ip_addresses(rh_user) != "") %>%
                                         mutate(ip_location = map(rh_user,
                                                                  ~maxmind(.x, ip_db)))  %>%
                                         unnest(ip_location)))

revision_history_page_details_ip_addresses_df <-
  revision_history_page_details_ip_addresses %>%
  unnest(edits_from_ip_addresses)

revision_history_page_details_ip_addresses_countries <-
  revision_history_page_details_ip_addresses_df %>%
  select(Site, Country, country_name, rh_n_edits ) %>%
  filter_all(all_vars(!is.na(.))) %>%
  group_by(Site, Country, rh_n_edits) %>%
  count(country_name) %>%
  mutate(prop_anon_of_all_edits = n / rh_n_edits) %>%
  mutate(internal_edits = ifelse(Country ==  country_name,
                                 TRUE, FALSE),
         n_anon_edits_total = n) %>%
  left_join(revision_history_page_details_ip_addresses_df) %>%
  distinct(Site,
           Country,
           prop_anon_of_all_edits,
           internal_edits,
           .keep_all = TRUE) %>%
  group_by(Site,  Country) %>%
  summarise(sum_prop_internal = sum(n_anon_edits_total[internal_edits]) / sum(n_anon_edits_total),
            sum_prop_external = 1 - sum_prop_internal,
            rh_n_edits = unique(rh_n_edits),
            rh_n_edits_anon = sum(n_anon_edits_total),
            prop_anon_of_all_edits = sum(n) / rh_n_edits)


randoms_revision_history_page_details <-
  map_df(ten_k_random_wp_pages_lst,
         ~.x$revision_history_page_details,
         .id = "page")

randoms_revision_history_page_details_ip <-
  randoms_revision_history_page_details %>%
  mutate(is_ip_address = ifelse(get_ip_addresses(rh_user) != "", TRUE, FALSE),
         page = as.numeric(page))

randoms_revision_history_page_details_ip_prop <-
  randoms_revision_history_page_details_ip %>%
  group_by(page) %>%
  summarise(prop_ip_addresses = sum(is_ip_address) / n() )

both_ip_props <-
  bind_rows(
    revision_history_page_details_ip_addresses_countries %>%
      ungroup %>%
      select(prop_anon_of_all_edits) %>%
      mutate(source = "World Heritage Sites",
             prop_ip_addresses = prop_anon_of_all_edits),

    randoms_revision_history_page_details_ip_prop %>%
      ungroup %>%
      select(prop_ip_addresses) %>%
      mutate(source = "Random pages"))

# Plot of proportion of edits that are anon: WH vs random
plot_props_of_anon_edits <-
ggplot(both_ip_props,
       aes(fill = source,
           prop_ip_addresses)) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis_d(labels = c("Random",
                                  "CS-WHL"),
                       option = "A") +
  theme_minimal(base_size = 8) +
  xlab("Proportion of all edits that are anonymous") +
  theme(legend.position = c(0.7, 0.7))


# Histogram of proportion of all anonymous edits that come from same country as WH site
ggplot(revision_history_page_details_ip_addresses_countries,
       aes(sum_prop_internal)) +
  geom_histogram() +
  theme_minimal()

ggplot(revision_history_page_details_ip_addresses_countries,
       aes(sum_prop_internal,
           prop_anon_of_all_edits)) +
  geom_point() +
  theme_minimal() +
  coord_equal()


## Circular chord plot

adjacency_list_3_cols <-
  revision_history_page_details_ip_addresses_df %>%
  filter(!is.na(Country),
         !is.na(country_name)) %>%
  group_by(Country, country_name) %>%
  count() %>%
  rename(to = Country,
         from = country_name,
         value = n) %>%
  ungroup() %>%
  mutate(from = ifelse(from == "Hashemite Kingdom of Jordan", "Jordan", from)) %>%
  select(from, to , value) %>%
  filter(value > 100) %>%
  left_join( revision_history_page_details_ip_addresses_df %>%
               select(country_name, continent_name) %>%
               distinct(),
             by = c('from' = 'country_name')) %>%
  arrange(continent_name,  from, to)


adjacency_list <-
  adjacency_list_3_cols%>%
  select(-continent_name)

get_countries_in_continent <- function(x) {

  xx <-
    revision_history_page_details_ip_addresses_df %>%
    select(country_name, continent_name) %>%
    filter(continent_name == x) %>%
    pull(country_name) %>%
    unique()


  xx[
    unique( c(adjacency_list$from,
              adjacency_list$to))
  ]
}

africa   <- get_countries_in_continent("Africa")
asia    <- get_countries_in_continent("Asia")
europe  <-get_countries_in_continent("Europe")
north_america  <- get_countries_in_continent("North America")
oceania <-  get_countries_in_continent("Oceania")
south_america  <-  get_countries_in_continent("South America")

library(circlize)
library(cowplot)
circos.clear()

# define a function that emits the desired plot
p1 <- function() {
  chordDiagram(adjacency_list,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               diffHeight = -uh(2, "mm"),
               annotationTrack = "grid",
               preAllocateTracks = list(track.height = max(0.4)))

  # we go back to the first track and customize sector labels
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
                cex = 0.75)
  }, bg.border = NA) # here set bg.border to NA is important
}

# create multi-panel plot
ggdraw(p1) +
  draw_plot(plot_props_of_anon_edits,
            x = 0.05, y = 0.65,
            width = 0.25, height = 0.25)

ggsave(here::here("analysis/figures/wh_countries_circle_chord_fig.png"),
       dpi = 300, width = 10, height = 10)


