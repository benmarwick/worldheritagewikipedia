library(tidyverse)
## Temporal patterns

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

## Page ages: are WH pages younger than the typical WP page? What's their relationship to their inscription year?

page_data_for_all_pages_rh_tbl <-
  map_df(page_data_for_all_pages$page_info_t,
         ~.x$revision_history_page_details, .id = "Site")

randoms_revision_history_page_details <-
  map_df(ten_k_random_wp_pages_lst,
         ~.x$revision_history_page_details,
         .id = "page")

# get first and last edit, and inscription year
library(lubridate)
page_data_for_all_pages_rh_tbl_edit_start_end_span <-
  page_data_for_all_pages_rh_tbl %>%
  group_by(Site) %>%
  summarise(first_edit_date = min(rh_date),
            last_edit_date = max(rh_date),
            edit_span = last_edit_date - first_edit_date) %>%
  mutate(Site = as.numeric(Site)) %>%
  arrange(Site) %>%
  bind_cols(page_data_for_all_pages) %>%
  mutate(inscription_year = as.Date(ISOdate(Year_num, 1, 1))) %>%
  mutate(diff_first_edit_inscription_year = as.numeric( as.POSIXlt(inscription_year) - ymd_hms(first_edit_date) ) / 365 )

page_data_for_random_rh_tbl_edit_start_end_span <-
  randoms_revision_history_page_details %>%
  group_by(page) %>%
  summarise(first_edit_date = min(rh_date),
            last_edit_date = max(rh_date),
            edit_span = last_edit_date - first_edit_date) %>%
  mutate(page = as.numeric(page)) %>%
  arrange(page)

# plot of age of articles
age_of_articles_plot <-
bind_rows(

  page_data_for_all_pages_rh_tbl_edit_start_end_span %>%
    select(edit_span) %>%
    mutate(source = "World Heritage"),

  page_data_for_random_rh_tbl_edit_start_end_span %>%
    select(edit_span) %>%
    mutate(source = "Random")

) %>%
  ggplot() +
  geom_density(aes(as.numeric(edit_span / 365),
                   fill = source),
               alpha = 0.3) +
  geom_vline(xintercept = year(Sys.Date()) - 2001,
             colour = "red") +
  annotate("text", x = 15, y = 0.2, label = "start of Wikipedia → ") +
  xlab("Age of Wikipedia page (years)") +
  scale_fill_viridis_d(labels = c("Random",
                                  "CS-WHL"),
                       option = "A") +
  theme_bw(base_size = 8) +
  xlab("Age of article in years") +
  theme(legend.position = c(0.2, 0.7))

## Time between inscription year and first edit
library(ggrepel)

time_between_inscription_year_and_first_edit <-
ggplot(page_data_for_all_pages_rh_tbl_edit_start_end_span,
       aes(Year_num,
           diff_first_edit_inscription_year)) +
  geom_jitter(alpha =  0.1, size = 3)  +
  geom_vline(xintercept = 2001, colour = "red") +
  annotate("text", x = 2000, y = -30,
           label = "start of Wikipedia",
           angle = 90,
           size = 3) +
  geom_hline(yintercept = 0,
             colour = "black",
             linetype = "dotted") +
  annotate("text", x = 1987, y = -5,
           label = "CS-WHL site inscribed\npre-WP",
           angle = 0,
           size = 3) +
  annotate("text", x = 2010, y = -20,
           label = "CS-WHL site inscribed\nafter WP created",
           angle = 0,
           size = 3) +
  annotate("text", x = 2010, y = 13,
           label = "WP article created\nafter CS-WHL inscription",
           angle = 0,
           size = 3)   +
  theme_minimal(base_size = 12) +
  xlab("Year of inscription on World Heritage list") +
  ylab("Difference between year of\ninscriptionand first edit on Wikipedia")

time_series_unesco_whl_inscriptions <-
ggplot(page_data_for_all_pages_rh_tbl_edit_start_end_span,
       aes(Year_num)) +
  geom_histogram() +
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = seq(1975, 2019, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("Year of inscription on World Heritage list")

histogram_appear_on_wp_inscription_plot <-
ggplot(page_data_for_all_pages_rh_tbl_edit_start_end_span,
       aes(diff_first_edit_inscription_year)) +
  geom_histogram() +
  theme_minimal(base_size = 12)  +
  geom_segment(x = 0, xend = 0, y = 0, yend = 50, colour = "red") +
  geom_segment(x = -10, xend = -10, y = 0, yend = 50, colour = "blue") +
  geom_segment(x = -22, xend = -22, y = 0, yend = 50, colour = "blue") +
  ylim(0, 70) +
  annotate("text", x = 10, y = 59, label = "Article appeared\nbefore inscription\n→", size =8) +
  annotate("text", x =-10,  y = 59, label = "Article appeared\nafter inscription\n←", size = 8) +
  annotate("text", x =-22,  y = 5, label = "Inscribed in 1983→",
           size = 3, colour = "white", hjust = 1  ) +
  annotate("text", x =-10,  y = 5, label = "Inscribed in 1994→",
           size = 3, colour = "white", hjust = 1 ) +
  annotate("text", x =0,  y = 5, label = "Inscribed in 2001→",
           size = 3, colour = "white", hjust = 1 ) +
  xlab("Difference between year of inscription on World Heritage list and first edit on Wikipedia")


# multiple panel plots of time between inscription and article
library(cowplot)

plot_grid(
          time_series_unesco_whl_inscriptions,
          histogram_appear_on_wp_inscription_plot +
            draw_plot(age_of_articles_plot,
                      x = -45, y = 40,
                      width = 20, height = 30),
          rel_heights = c(1, 2),
          ncol = 1
          )

ggsave(here::here("analysis/figures/wh_wikipedia_time_inscription.png"),
       dpi = 300, width = 10, height = 10)

library(tidyverse)
library(lubridate)

## time series of edits: breakouts relating to events? monthly seems like a good level of aggregation. What can we do for spike detection?
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

revision_history_page_details_wordcount <-
  revision_history_page_details %>%
  bind_cols(page_data_for_all_pages %>% select(page_wordcount)) %>%
  mutate(editors_per_1k_words = rh_n_editors / (page_wordcount / 1000))

wh_pages_talk_page_length <-
  left_join(page_data_for_all_pages,
            revision_history_page_details_wordcount)

wh_pages_edit_time_series <-
  wh_pages_talk_page_length %>%
  unnest(revision_history_page_details) %>%
  filter(Year_num >= 2001) %>% # only sites inscribed after WP started
  group_by(Site) %>%
  mutate(the_d = floor_date(rh_date, "day")) %>%
  mutate(the_m = floor_date(rh_date, "month")) %>%
  group_by(Site, the_m) %>%
  tally() %>%
  filter(n() > 150) %>% # explore this value
  left_join(wh_pages_talk_page_length) %>%
  mutate(vline_year = ifelse(Year_num >= 2001, Year_num, NA)) %>%
  mutate(vline_year = ymd_hms(ISOdatetime(vline_year, 1, 1, 1, 0, 0)))

# facet plot of time series and line for year of inscription
face_plot_time_series <-
ggplot(wh_pages_edit_time_series) +
  geom_vline(aes(xintercept = vline_year), colour = "red") +
  geom_line(aes(the_m,
                n)) +
  facet_wrap( ~ str_wrap(Site, 30), scales = "free") +
  theme_minimal(base_size = 6) +
  labs(x = "",
       y = "")

ggsave(here::here("analysis/figures/wh_wikipedia_time_series_facet.png"),
       dpi = 300, width = 12, height = 10)

