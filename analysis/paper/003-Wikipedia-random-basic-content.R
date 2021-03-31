## Basic qualities of the content of WP articles about WH sites
library(tidyverse)

# load the random pages
if(!exists("ten_k_random_wp_pages")){
ten_k_random_wp_pages <-
  readRDS(here::here("analysis/data/raw_data/random_page_data_tbl.rds"))
}

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

ten_k_random_wp_pages_basic_qualities <-
  ten_k_random_wp_pages %>%
  select(page_wordcount,
         page_wikilinks_out,
         page_cited_items_on) %>%
  mutate(rnd_page_wikilinks_out_per_1k_words = page_wikilinks_out / page_wordcount * 1000,
         rnd_page_cited_items_on_per_1k_words = page_cited_items_on / page_wordcount * 1000,
         rnd_page_wordcount = page_wordcount) %>%
  select(-page_wikilinks_out,
         -page_cited_items_on,
         -page_wordcount)

ten_k_random_wp_pages_basic_qualities_long <-
  ten_k_random_wp_pages_basic_qualities %>%
  gather(variable, value)

page_data_for_all_pages_basic_qualities <-
  page_data_for_all_pages %>%
  select(page_wordcount,
         page_wikilinks_out,
         page_cited_items_on) %>%
  mutate(page_wikilinks_out_per_1k_words = page_wikilinks_out / page_wordcount * 1000,
         page_cited_items_on_per_1k_words = page_cited_items_on / page_wordcount * 1000) %>%
  select(-page_wikilinks_out,
         -page_cited_items_on)

page_data_for_all_pages_basic_qualities_long <-
  page_data_for_all_pages_basic_qualities %>%
  gather(variable, value) %>%
  bind_rows(ten_k_random_wp_pages_basic_qualities_long) %>%
  mutate(source = ifelse(str_detect(variable, "rnd" ),
                         "10k Random pages",
                         "World Heritage pages")) %>%
  mutate(variable = str_remove(variable, "rnd_"))

label_names1 <- list(
  "page_wordcount"="Article word count",
  "page_wikilinks_out_per_1k_words"="Number of Wikilinks\nout per 1k words",
  "page_cited_items_on_per_1k_words"="Number of sources cited\nper 1k words"
)

labeller1 <- function(variable,value){
  return(label_names1[value])
}

# density plots --------------------------------------------------
basic_content_plot <-
ggplot(page_data_for_all_pages_basic_qualities_long,
       aes(value)) +
  geom_density(aes(fill = source),
               alpha = 0.3) +
  scale_x_log10() +
  scale_fill_viridis_d(option = "A") +
  guides(fill = FALSE) +
  facet_wrap( ~ variable,
              scales = "free",
              labeller = labeller1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs( x = "", y = "") +
  # ggtitle("Basic qualities about Wikipedia article content") +
  NULL

## Basic qualities of the consumption of WP articles about WH sites

library(ggrepel)
ten_k_random_wp_pages_attent <-
  ten_k_random_wp_pages %>%
  select(page_wikilinks_in,
         page_views_last_n_days_total)

page_data_for_all_pages_attent <-
  page_data_for_all_pages %>%
  select(page_wikilinks_in,
         page_views_last_n_days_total)

page_data_for_all_pages_attent_long <-
  page_data_for_all_pages_attent %>%
  gather(variable, value) %>%
  bind_rows(ten_k_random_wp_pages_attent %>%
              gather(variable, value) %>%
              mutate(variable =  str_glue('rnd_{variable}'))) %>%
  mutate(source = ifelse(str_detect(variable, "rnd" ), "10k Random articles",
                         "World Heritage articles")) %>%
  mutate(variable = str_remove(variable, "rnd_"))

# density plots -----------------------------------------------

label_names2 <- list(
  "page_wikilinks_in"="Number of Wikilinks to the article",
  "page_views_last_n_days_total"="Total article views\n(last 100 days)"
)

labeller2 <- function(variable,value){
  return(label_names2[value])
}

basic_consumption_plot <-
ggplot(page_data_for_all_pages_attent_long,
       aes(value)) +
  geom_density(aes(fill = source),
               alpha = 0.3) +
  scale_x_log10(label = scales::label_number_si() ) +
  scale_fill_viridis_d(option = "A") +
  guides(fill = FALSE) +
  facet_wrap( ~ variable,
              scales = "free",
              labeller = labeller2) +
  theme_minimal() +
  labs( x = "", y = "") +
 # ggtitle("Basic qualities about consumption of Wikipedia articles") +
  NULL


## Scatterplot showing the WP sites that get the most attention

library(ggrepel)
basic_consumption_scatter_plot <-
page_data_for_all_pages %>%
  select( Site,
          page_wordcount,
          page_wikilinks_in,
          page_views_last_n_days_total)  %>%
  ggplot(aes(page_wikilinks_in,
             page_views_last_n_days_total,
             label = str_wrap(Site, 30))) +
  geom_point(alpha = 0.4,
             aes(size = page_wordcount,
                 colour = page_wordcount)) +
  geom_text_repel(
    data          = page_data_for_all_pages %>%
      filter(page_views_last_n_days_total > 1e5),
    segment.size  = 0.2,
   # force = 0,
    size = 2.5,
    segment.color = "grey50",
    nudge_x = -0.1,
    nudge_y = -0.1,
    direction     = "both",
    bg.color      = "white",
    bg.r          = 0.1,
   max.overlaps = 50
  ) +
  scale_y_log10(label = scales::label_number_si()) +
  scale_x_log10() +
  #coord_fixed(0.5) +
  scale_color_viridis_c(name = "Wordcount"
                        #,
                      #  labels = c(20000, 15000, 10000, 5000),
                      #  breaks = c(20000, 15000, 10000, 5000)
                      ) +
  scale_size_continuous(name = "Wordcount"
                        #,
                      #  labels = c(20000, 15000, 10000, 5000),
                      #  breaks = c(20000, 15000, 10000, 5000)
                        ) +
  guides(color= guide_legend(),
         size=guide_legend()) +
  theme_minimal(base_size = 8) +
  theme(legend.position=c(.1, .75)) +
  labs(x = "Number of Wikilinks to the page",
       y = "Total page views (last 100 days)") +
  # ggtitle("Basic qualities about the consumption of Wikipedia articles") +
  NULL

# how many of those are on the danger list?
popular_sites_on_the_danger_list <-
page_data_for_all_pages %>%
  filter(page_views_last_n_days_total > 1e5) %>%
  inner_join(wh_unesco_danger, by = c("Site" = "name_en")) %>%
  select(Site)

library(patchwork)

# content plot
basic_content_plot

ggsave(here::here("analysis/figures/wh_wikipedia_articles_basic_content.png"),
       width = 12,
       height = 4,
       dpi = 300)

# consumption plot
design <- "
  11
  22
  22
"
basic_consumption_plot + basic_consumption_scatter_plot +
  plot_layout(design = design)

ggsave(here::here("analysis/figures/wh_wikipedia_articles_basic_consumption.png"),
       width = 12,
       height = 12,
       dpi = 300)
