## Basic qualities of the editing process of WP articles about WH sites

# number of edits, total, edit density (prop/words), size of edits
# how many bot edits, how many reverts per page
# diversity of editors

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

# random sites

ten_k_random_wp_pages_revision_history_page_details <-
  map_df(ten_k_random_wp_pages_lst,
         ~.x$revision_history_page_details,
         .id = 'Site')

# very slow!
ten_k_random_wp_pages_revision_history_page_details_summary_tbl <-
  ten_k_random_wp_pages_revision_history_page_details %>%
  mutate(Site = as.numeric(Site)) %>%
  group_by(Site) %>%
  summarise(
    rh_n_editors = n_distinct(rh_user),
    rh_n_edits =   n(),
    rh_n_bot_edits = sum(bot),
    rh_n_reverts = sum(revert)) %>%
  mutate(rh_revert_prop = rh_n_reverts / rh_n_edits,
         rh_bot_edits_prop = rh_n_bot_edits / rh_n_edits)

# plot both samples, number of edits per page
edits_per_page_plot <-
bind_rows(revision_history_page_details %>%
            select(Site, rh_n_edits) %>%
            mutate(Site = "wh"),
          ten_k_random_wp_pages_revision_history_page_details_summary_tbl %>%
            select(Site, rh_n_edits) %>%
            mutate(Site = "rand")) %>%
  ggplot(aes(rh_n_edits,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  scale_x_log10(label = scales::label_number_si()) +
  guides(fill = FALSE) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  labs(title = "Number of edits per page",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Edits per 1k words, as a measure of edit density

revision_history_page_details_wordcount <-
  revision_history_page_details %>%
  bind_cols(page_data_for_all_pages %>% select(page_wordcount)) %>%
  mutate(edits_per_1k_words = rh_n_edits / (page_wordcount / 1000))

ten_k_random_wp_pages_revision_history_page_details_summary_tbl_all_dets <-
  bind_cols(ten_k_random_wp_pages,
            ten_k_random_wp_pages_revision_history_page_details_summary_tbl) %>%
  mutate(edits_per_1k_words = rh_n_edits / (page_wordcount / 1000))

edits_per_1k_words_plot <-
bind_rows(revision_history_page_details_wordcount %>%
            select(Site, edits_per_1k_words) %>%
            mutate(Site = "wh"),
          ten_k_random_wp_pages_revision_history_page_details_summary_tbl_all_dets %>%
            select(Site, edits_per_1k_words) %>%
            mutate(Site = "rand")) %>%
  ggplot(aes(edits_per_1k_words,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(title = "Number of edits per 1k words per page",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::label_number_si()) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL

## Number of unique editors per page

# plot both samples, number of edits per page
unique_editors_per_page_plot  <-
bind_rows(revision_history_page_details %>%
            select(Site, rh_n_editors) %>%
            mutate(Site = "wh"),
          ten_k_random_wp_pages_revision_history_page_details_summary_tbl %>%
            select(Site, rh_n_editors) %>%
            mutate(Site = "rand")) %>%
  ggplot(aes(rh_n_editors,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(title = "Number of unique editors per page",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::label_number_si()) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL


## Number of unique editors per 1k words per page

revision_history_page_details_wordcount <-
  revision_history_page_details %>%
  bind_cols(page_data_for_all_pages %>% select(page_wordcount)) %>%
  mutate(editors_per_1k_words = rh_n_editors / (page_wordcount / 1000))

ten_k_random_wp_pages_revision_history_page_details_summary_tbl_all_dets <-
  bind_cols(ten_k_random_wp_pages,
            ten_k_random_wp_pages_revision_history_page_details_summary_tbl) %>%
  mutate(editors_per_1k_words = rh_n_editors / (page_wordcount / 1000))

unique_editors_per_1k_words_plot  <-
bind_rows(revision_history_page_details_wordcount %>%
            select(Site, editors_per_1k_words) %>%
            mutate(Site = "wh"),
          ten_k_random_wp_pages_revision_history_page_details_summary_tbl_all_dets %>%
            select(Site, editors_per_1k_words) %>%
            mutate(Site = "rand")) %>%
  ggplot(aes(editors_per_1k_words,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(title = "Number of unique editors per 1k words per page",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::label_number_si()) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL

## Size of edits, absolute and raw

page_data_for_all_pages_rh_tbl <-
  map_df(page_data_for_all_pages$page_info_t,
         ~.x$revision_history_page_details, .id = "Site")

label_names <- list(
  "mean_abs_diff"="Absolute edit sizes",
  "mean_diff"="Raw edit sizes"
)

labeller <- function(variable,value){
  return(label_names[value])
}

average_edit_size_plot  <-
bind_rows(page_data_for_all_pages_rh_tbl %>%
            select(Site, rh_diff_size) %>%
            group_by(Site) %>%
            summarise(mean_diff = mean((rh_diff_size)),
                      mean_abs_diff = mean(abs(rh_diff_size))) %>%
            mutate(Site = "wh"),
          ten_k_random_wp_pages_revision_history_page_details %>%
            select(Site, rh_diff_size) %>%
            group_by(Site) %>%
            summarise(mean_diff = mean((rh_diff_size)),
                      mean_abs_diff = mean(abs(rh_diff_size))) %>%
            mutate(Site = "rand")) %>%
  gather(variable, value, -Site) %>%
  filter( variable == "mean_abs_diff") %>%
  ggplot(aes(value,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(title = "Average size of edits per article (bytes)",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::label_number_si()) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL

## How many edits made by bots? Proportion of edits per page made by bots

bot_edit_prop_plot  <-
bind_rows(

  page_data_for_all_pages_rh_tbl %>%
    group_by(Site) %>%
    summarise( n_bot_edits = sum(bot),
               n_edits = n()) %>%
    mutate(prop_bot_edits = n_bot_edits / n_edits,
           Site = "wh"),

  ten_k_random_wp_pages_revision_history_page_details %>%
    group_by(Site) %>%
    summarise( n_bot_edits = sum(bot),
               n_edits = n()) %>%
    mutate(prop_bot_edits = n_bot_edits / n_edits,
           Site = "rand")
) %>%

  ggplot(aes(prop_bot_edits,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(x = "Proportion of edits made by bots per article",
       title = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::comma) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL

## Which WH pages are most edited by bots?
library(ggrepel)

wh_bots <-
  tibble(revision_history_page_details = map(page_data_for_all_pages$page_info_t,
                                             ~.x$revision_history_page_details))  %>%
  mutate(Site = page_data_for_all_pages$Site) %>%
  mutate(n_bot_edits = map_dbl(revision_history_page_details, ~sum(.x$bot)),
         n_edits = map_int(revision_history_page_details, ~nrow(.))) %>%
  mutate(prop_bot_edits = n_bot_edits / n_edits) %>%
  arrange(desc(prop_bot_edits))

which_articles_have_bots_plot <-
ggplot(wh_bots %>%
         filter(prop_bot_edits >= 0),
       aes(n_bot_edits,
           n_edits,
           label = str_wrap(Site, 30))) +
  geom_point(alpha = 0.4,
             aes(size =   prop_bot_edits,
                 colour = prop_bot_edits)) +
  geom_text_repel(
    data            = wh_bots %>%
      filter(prop_bot_edits > 0.3),
    size = 2,
    segment.size  = 0.2,
    force = 20,
    segment.color = "grey50",
    direction     = "both",
    max.overlaps = 50,
    bg.color      = "white",
    bg.r          = 0.1
  ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_viridis_c(name = "Proportion of\nedits by bots") +
  scale_size_continuous(name = "Proportion of\nedits by bots") +
  guides(color = guide_legend(),
         size =  guide_legend()) +
  theme_minimal(base_size = 12) +
  theme(legend.position=c(.15, .8)) +
  labs(x = "Number of edits by bots",
       y = "Total number of edits") +
  NULL

# how many of those are on the danger list?
bot_sites_on_the_danger_list <-
  wh_bots %>%
  filter(prop_bot_edits >= 0.3) %>%
  inner_join(wh_unesco_danger, by = c("Site" = "name_en")) %>%
  select(Site)

## which bots are most active on WH pages vs random pages?
# Cydebot: category edits
# Smackbot: dates in templates
# ClueBot NG: vandalism

which_bots_plot <-
bind_rows(
  page_data_for_all_pages_rh_tbl %>%
    filter(bot == 1) %>%
    group_by(rh_user) %>%
    tally(sort = TRUE) %>%
    slice(1:10) %>%
    mutate( Site = "World Heritage Sites"),

  ten_k_random_wp_pages_revision_history_page_details %>%
    filter(bot == 1) %>%
    group_by(rh_user) %>%
    tally(sort = TRUE) %>%
    slice(1:10) %>%
    mutate( Site = "Random pages")) %>%

  ggplot(aes(reorder(rh_user, n),
             n,
             fill = Site)) +
  geom_col(position = position_dodge(), colour = "black") +
  theme_bw(base_size = 6) +
  coord_flip() +
  guides(fill = FALSE) +
  scale_fill_viridis_d(option = "A", alpha = 0.4) +
  # facet_wrap( ~ Site, scales = "free") +
  # theme(legend.position = c(0.7, 0.7)) +
  labs(x = "",
       y = "")

library(cowplot)
bot_plot <-
ggdraw(which_articles_have_bots_plot) +
  draw_plot(which_bots_plot,
            x = .28,
            y = .7,
            width = .3,
            height = .33)

plot_grid(
  plot_grid(edits_per_1k_words_plot,
            unique_editors_per_1k_words_plot,
            average_edit_size_plot,
            nrow = 1),
  plot_grid(bot_edit_prop_plot,
            bot_plot,
            nrow = 1,
            rel_widths = c(1,2)),
        nrow = 2,
        rel_heights = c(1,2),
        align="hv")

ggsave(here::here("analysis/figures/wh_wikipedia_articles_production.png"),
       width = 12,
       height = 8,
       dpi = 300)



