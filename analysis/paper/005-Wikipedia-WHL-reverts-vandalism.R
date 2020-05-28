
## So about vandalism? do WH sites get more than random ones?

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

page_data_for_all_pages_rh_tbl <-
  map_df(page_data_for_all_pages$page_info_t,
         ~.x$revision_history_page_details, .id = "Site")

ten_k_random_wp_pages_revision_history_page_details <-
  map_df(ten_k_random_wp_pages_lst,
         ~.x$revision_history_page_details,
         .id = 'Site')

# edits containing the word 'vandal'
edits_with_vandal_plot <-
bind_rows(
  page_data_for_all_pages_rh_tbl %>%
    mutate(vandalism = ifelse(str_detect(rh_comment, "vandal"), 1, 0)) %>%
    group_by(Site) %>%
    summarise( n_vandal_edits = sum(vandalism),
               n_edits = n()) %>%
    mutate(prop_vandal_edits = n_vandal_edits / n_edits,
           Site = "World Heritage Sites"),

  ten_k_random_wp_pages_revision_history_page_details %>%
    mutate(vandalism = ifelse(str_detect(rh_comment, "vandal"), 1, 0)) %>%
    group_by(Site) %>%
    summarise( n_vandal_edits = sum(vandalism),
               n_edits = n()) %>%
    mutate(prop_vandal_edits = n_vandal_edits / n_edits,
           Site = "Random pages")
) %>%

  ggplot(aes(prop_vandal_edits,
             fill = Site)) +
  geom_density(alpha = 0.4) +
  scale_x_log10() +
  guides(fill = FALSE) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  labs(title = "Proportion of edits relating to vandalism per article",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL


## Which WH pages have the most vandalism?

vandal_edits <-
  tibble(revision_history_page_details = map(page_data_for_all_pages$page_info_t,
                                             ~.x$revision_history_page_details))  %>%
  mutate(Site = page_data_for_all_pages$Site) %>%
  mutate(n_vandal_edits = map_dbl(revision_history_page_details,
                                  ~sum(ifelse(str_detect(.x$rh_comment, "vandalism"), 1, 0))),
         n_edits = map_int(revision_history_page_details, ~nrow(.))) %>%
  mutate(prop_vandal_edits = n_vandal_edits / n_edits) %>%
  arrange(desc(prop_vandal_edits))

vandal_edits_articles_plot <-
ggplot(vandal_edits,
       aes(n_vandal_edits,
           n_edits,
           label = str_wrap(Site, 30))) +
  geom_point(alpha = 0.4,
             aes(size =   prop_vandal_edits,
                 colour = prop_vandal_edits)) +
  geom_text_repel(
    data            = vandal_edits %>%
      filter(prop_vandal_edits > 0.03),
    segment.size  = 0.2,
    size = 2,
    force = 40,
    segment.color = "grey50",
    direction     = "both",
    max.overlaps = 50,
    bg.color      = "white",
    bg.r          = 0.1
  ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_viridis_c(name = "Proportion of\nedits about\nvandalism") +
  scale_size_continuous(name = "Proportion of\nedits about\nvandalism") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme_minimal(base_size = 10) +
  theme(legend.position=c(0.85, 0.2)) +
  labs(x = "Number of edits about vandalism",
       y = "Total number of edits") +
 # ggtitle("Editing about vandalism on Wikipedia articles") +
  NULL

## Reverts

reverting_edits_plot <-
bind_rows(
  page_data_for_all_pages_rh_tbl %>%
    mutate(revert = ifelse(revert, 1, 0)) %>%
    group_by(Site) %>%
    summarise( n_revert_edits = sum(revert),
               n_edits = n()) %>%
    mutate(prop_revert_edits = n_revert_edits / n_edits,
           Site = "World Heritage Sites"),

  ten_k_random_wp_pages_revision_history_page_details %>%
    mutate(revert = ifelse(revert, 1, 0)) %>%
    group_by(Site) %>%
    summarise( n_revert_edits = sum(revert),
               n_edits = n()) %>%
    mutate(prop_revert_edits = n_revert_edits / n_edits,
           Site = "Random pages")
) %>%

  ggplot(aes(prop_revert_edits,
             fill = Site)) +
  geom_density(alpha = 0.4) +
  scale_x_log10() +
  guides(fill = FALSE) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  labs(title = "Proportion of reverting edits per article",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL

## Which articles have the most reverts?

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


articles_with_revert_edits_plot <-
ggplot(revision_history_page_details,
       aes(rh_revert_prop,
           rh_n_editors,
           label = str_wrap(Site, 15))) +
  geom_point(alpha = 0.4,
             aes(size =   rh_n_edits,
                 colour = rh_n_edits)) +
  geom_text_repel(
    data            = revision_history_page_details %>%
      filter(rh_revert_prop > 0.1),
    size = 2,
    segment.size  = 0.2,
    force = 40,
    segment.color = "grey50",
    direction     = "both",
    max.overlaps = 50,
    bg.color      = "white",
    bg.r          = 0.1
  ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_viridis_c(name = "Proportion of\n edits reverted") +
  scale_size_continuous(name = "Proportion of\n edits reverted") +
  guides(color= guide_legend(), size=guide_legend()) +
  theme_minimal(base_size = 10) +
  theme(legend.position=c(.15, .2)) +
  labs(y = "Number of editors",
       x = "Proportion of revert edits") +
  # ggtitle("Reverted edits on Wikipedia articles") +
  NULL

# how many of those are on the danger list?
reverted_sites_on_the_danger_list <-
  revision_history_page_details %>%
  filter(rh_revert_prop >= 0.1) %>%
  inner_join(wh_unesco_danger, by = c("Site" = "name_en")) %>%
  select(Site)

# create multipanel plot

library(cowplot)

plot_grid(
  plot_grid(edits_with_vandal_plot,
            reverting_edits_plot,
            nrow = 1),
  plot_grid(vandal_edits_articles_plot,
            articles_with_revert_edits_plot,
            nrow = 1,
            rel_widths = c(1,1)),
  nrow = 2,
  rel_heights = c(1,3),
  align="hv")

ggsave(here::here("analysis/figures/wh_wikipedia_articles_vandals_reverts.png"),
       width = 12,
       height = 8,
       dpi = 300)

