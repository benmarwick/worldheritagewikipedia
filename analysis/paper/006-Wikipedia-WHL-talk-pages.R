## What's the typical distribution of talk page length?


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

# load the random pages
if(!exists("ten_k_random_wp_pages")){
  ten_k_random_wp_pages <-
    readRDS(here::here("analysis/data/raw_data/random_page_data_tbl.rds"))
}

# the talk pages for all WH sites, with WP pages
if(!exists("wh_wiki_talk_pages")){
wh_wiki_talk_pages <-
  readRDS(here::here("analysis/data/raw_data/wh_wiki_table_talk_page_content.rds"))
}

if(!exists("wh_wiki_table")){
wh_wiki_table <-
  readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls")) %>%
  filter(category %in% c("Cultural"))
}

if(!exists("wh_unesco_danger")){
  wh_unesco_danger <- readxl::read_excel(here::here("analysis/data/raw_data/whc-sites-2019.xls")) %>%
    filter(category %in% c("Cultural")) %>%
    filter(danger == 1)
}

talk_page_word_count_distribution_plot <-
bind_rows(

  ten_k_random_wp_pages %>%
    select(talk_page_wordcount) %>%
    mutate(Site = "Random pages"),

  page_data_for_all_pages %>%
    select(talk_page_wordcount) %>%
    mutate(Site = "World Heritage pages")
) %>%

  ggplot(aes(talk_page_wordcount,
             fill = Site)) +
  geom_density(alpha = 0.3) +
  labs(title = "Talk page word count",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  scale_x_log10(label = scales::label_number_si()) +
  scale_fill_viridis_d(labels = c("Random",
                                  "World Heritage"),
                       option = "A") +
  theme_bw(base_size = 6) +
  theme(plot.title = element_text(hjust = 0.5)) +
  NULL


## which WH pages have the longest talk pages?
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


revision_history_page_details_wordcount <-
  revision_history_page_details %>%
  bind_cols(page_data_for_all_pages %>% select(page_wordcount)) %>%
  mutate(edits_per_1k_words = rh_n_edits / (page_wordcount / 1000))

wh_pages_talk_page_length <-
  left_join(page_data_for_all_pages,
            revision_history_page_details_wordcount)

talk_page_word_count_by_edits_plot <-
ggplot(wh_pages_talk_page_length,
       aes(page_wordcount,
           talk_page_wordcount,
           label = str_wrap(Site, 30))) +
  geom_point(alpha = 0.5,
             aes(size =   (edits_per_1k_words),
                 colour = (edits_per_1k_words))) +
  geom_text_repel(
    data  = wh_pages_talk_page_length %>%
      mutate(ratio_length = talk_page_wordcount / page_wordcount ) %>%
      filter(ratio_length > 1),
    size = 3,
    segment.size  = 0.2,
    force = 20,
    segment.color = "grey50",
    direction     = "both",
    max.overlaps = 50,
    bg.color      = "white",
    bg.r          = 0.1
  ) +
  scale_y_log10(labels = scales::label_number_si()) +
  scale_x_log10(labels = scales::label_number_si()) +
  scale_color_viridis_c(name = "Edits per\n1k words") +
  scale_size_continuous(name = "Edits per\n1k words") +
  guides(color= guide_legend(),
         size=guide_legend()) +
  theme_minimal(base_size = 12) +
  theme(legend.position=c(.875, .2)) +
  labs(x = "Article word count",
       y = "Talk page word count") +
  ggtitle("")



talk_page_word_count_by_edits_plot_old <-
ggplot(wh_pages_talk_page_length,
       aes(rh_n_edits,
           talk_page_wordcount,
           label = str_wrap(Site, 30))) +
  geom_point(alpha = 0.5,
             aes(size =   (talk_page_wordcount),
                 colour = (talk_page_wordcount))) +
  geom_text_repel(
    data            = wh_pages_talk_page_length %>%
      filter(talk_page_wordcount > 5000),
    size = 3,
    segment.size  = 0.2,
    force = 20,
    segment.color = "grey50",
    direction     = "both",
    max.overlaps = 50,
    bg.color      = "white",
    bg.r          = 0.1
  ) +
  scale_y_log10(labels = scales::label_number_si()) +
  scale_x_log10(labels = scales::label_number_si()) +
  scale_color_viridis_c(name = "Talk page\nword count") +
  scale_size_continuous(name = "Talk page\nword count") +
  guides(color= guide_legend(),
         size=guide_legend()) +
  theme_minimal(base_size = 12) +
  theme(legend.position=c(.875, .2)) +
  labs(x = "Number of edits",
       y = "Talk page word count") +
  ggtitle("")

# combine talk page length plots

library(cowplot)
talk_page_length_plot <-
  ggdraw(talk_page_word_count_by_edits_plot) +
  draw_plot(talk_page_word_count_distribution_plot,
            x = .070,
            y = .61,
            width = .2,
            height = .3)

ggsave(here::here("analysis/figures/wh_wikipedia_articles_talk_pages.png"),
       width = 12,
       height = 8,
       dpi = 300)

# site on the in danger list?
talk_pages_danger_list <-
wh_pages_talk_page_length %>%
  mutate(ratio_length = talk_page_wordcount / page_wordcount ) %>%
  filter(ratio_length > 0.75) %>%
  inner_join(wh_unesco_danger, by = c("Site" = "name_en")) %>%
  select(Site)

#--------------------------------------------------------------------
#
# ## Talk page topic modelling
#
# library(tidytext)
#
# tidy_talk_raw <-
#   enframe(unlist(wh_wiki_talk_pages)) %>%
#   bind_cols(wh_wiki_table %>%
#               filter(category  == "Cultural"))
#
# write.csv(tidy_talk_raw$value , "data/wh_wiki_talk_page_text.txt")
#
# tidy_talk <-
#   tidy_talk_raw %>%
#   mutate(clean_text = str_remove_all(value, "\n|\t")) %>%
#   unnest_tokens(word, clean_text) %>%
#   anti_join(stop_words) %>%
#   filter(!word %in% c("article", "page", "talk", "utc",
#                       "class", "edit", "project's",
#                       "importance", "rated", "wikiproject",
#                       "project", "article", "articles", "article's",
#                       "wikipedia", "start", "scale",
#                       "world", "heritage", "sites",
#                       "links", "http", "parser",
#                       "output", "mw", "https",
#                       "unsigned", "comment")) %>%
#   filter(!str_detect(word, "[[:digit:]]+")) %>%
#   filter(!str_detect(word, ":")) %>%
#   filter(!str_detect(word, "articles")) %>%
#   filter(!str_detect(word, "jump")) %>%
#   filter(!str_detect(word, "\\."))
#
# # check which sites have some words
# tidy_talk %>%
#   filter(word == "unesco") %>%
#   group_by(Site) %>%
#   tally(sort = T)
#
#
# # top words
# tidy_talk %>%
#   count(word, sort = TRUE)
#
# # biggest documents
# tidy_talk %>%
#   group_by(name) %>%
#   tally(sort = TRUE)
#
#
# # The held-out likelihood is highest between 60 and 80, and the residuals are lowest around 50,
#
#
# library(quanteda)
# library(stm)
#
# talk_dfm <- tidy_talk %>%
#   count(name, word, sort = TRUE) %>%
#   cast_dfm(name, word, n)
#
# # library(slam)
# # talk_dfm_stm <- convert(talk_dfm, to = "stm")
#
# # very time consuming
# # k_search_output <-
# # searchK(talk_dfm_stm$documents,
# #         talk_dfm_stm$vocab,
# #         K = seq(10, 100, 20),
# #         cores = 1)
# #
# # plot(k_search_output)
#
# # takes a few minutes
# topic_model <- stm(talk_dfm, K = 60,
#                    verbose = FALSE,
#                    init.type = "Spectral")
#
# png(filename = "figures/wh_talk_pages_top_topics.png", width = 1800, height = 2600, res = 300)
# par(bty="n",col="grey40",lwd=10)
# plot(topic_model,  text.cex = 0.75)
# dev.off()
#
# td_beta <- tidy(topic_model)
#
# library(drlib) # devtools::install_github("dgrtwo/drlib")
#
# td_beta %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   mutate(topic = paste0("Topic ", topic),
#          term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(term, beta, fill = as.factor(topic))) +
#   geom_col(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free_y") +
#   coord_flip() +
#   scale_x_reordered() +
#   theme_minimal(base_size = 6) +
#   labs(x = NULL, y = expression(beta),
#        title = "Highest word probabilities for each topic",
#        subtitle = "Different words are associated with different topics")
#
# ## The probability that each document is generated from each topic.
#
# td_gamma <- tidy(topic_model, matrix = "gamma",
#                  document_names = rownames(talk_dfm))
#
# ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
#   geom_histogram(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~ topic, ncol = 3) +
#   labs(title = "Distribution of document probabilities for each topic",
#        y = "Number of stories", x = expression(gamma)) +
#   theme_minimal(base_size = 6)
#
# ## we may want to know which topics are associated with each talk page?
#
# topic_model_gamma <- tidy(topic_model, matrix = "gamma")
#
# topic_model_gamma %>%
#   filter(document %in% 1:10) %>%
#   mutate(title = reorder(document, gamma * topic)) %>%
#   ggplot(aes(factor(topic), gamma)) +
#   geom_boxplot() +
#   facet_wrap(~ title)
#
# ## Topic correlations
#
# mod.out.corr <- topicCorr(topic_model)
#
# topic_labels <- labelTopics(topic_model)
#
# topic_tag <- NULL
# for(i in 1:nrow(topic_labels$score)){
#   topic_tag[[i]] <-  paste0(as_tibble(topic_labels$score)[i, 1:3], collapse = "\n")
# }
#
# library(igraph)
# g <- graph.adjacency(mod.out.corr$posadj)
# V(g)$Degree <- degree(g, mode = 'in') # CALCULATE DEGREE
# V(g)$Name <- topic_tag # ADD NAMES
# E(g)$weight <- edge.betweenness(g)
#
# library(ggraph)
#
# ggraph(g, layout = "lgl") +
#   geom_edge_link0(aes(width = weight), alpha = 0.1) +
#   geom_edge_diagonal(label_colour = "blue", alpha = 0.1) +
#   geom_node_point(aes(size = Degree), alpha = 0.6, colour = "steelblue") +
#   geom_node_text(aes(label = Name), size = 3) +
#   theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
#
#
