library(tidyverse)

# all WH sites with WP pages (n = 584)
if(!exists("page_data_for_all_pages")){
  page_data_for_all_pages <-
    readRDS(here::here("analysis/data/raw_data/page_data_for_all_pages.rds"))
}

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size,
           base_family = base_family) %+replace%
    theme(panel.grid = element_blank() )
}


## Overall clusters

normalize = function(x) (x-min(x))/(max(x)-min(x))

#--------------------------------------------------------------------------
# take a look at the distribution of page variables
some_page_variables <-
  page_data_for_all_pages %>%
  select(page_wordcount,
         page_wikilinks_out,
         page_wikilinks_in,
         page_cited_items_on) %>%
  mutate(page_wikilinks_out_per_word = page_wikilinks_out / page_wordcount,
         page_cited_items_on_per_word = page_cited_items_on / page_wordcount)



# umap
library(uwot)
library("tmap")
data("World")

pages_umap_input <-
  some_page_variables  %>%
  mutate(page_wordcount_norm = normalize(page_wordcount),
         page_wikilinks_in_norm= normalize(page_wikilinks_in),
         page_wikilinks_out_per_word_norm = normalize(page_wikilinks_out_per_word),
         page_cited_items_on_per_word_norm = normalize(page_cited_items_on_per_word)) %>%
  select(-page_wordcount,
         -page_cited_items_on,
         -page_wikilinks_out,
         -page_wikilinks_in,
         -page_wikilinks_out_per_word,
         -page_cited_items_on_per_word) %>%
  bind_cols(., page_data_for_all_pages[ , 'country'] ) %>%
  filter_all(all_vars(!is.na(.))) %>%
  left_join(World %>%
              select(name, continent),
            by = c('country' = 'name'))

# compute umap

pages_umap_input_selected <-
  pages_umap_input %>%
  select(-country,
         -continent,
         -geometry
  )

pages_umap_output <-
  pages_umap_input_selected %>%
  umap(.,
       n_neighbors = 60,
       min_dist = 0.7,
       nn_method = "annoy",
       init = "spca") %>%
  as_tibble()

# compute hdbscan clusters
library(dbscan)
hdbscan_out <- hdbscan(pages_umap_output,
                       minPts = 5)

clus_tbl <-
tibble(clus = names(table(hdbscan_out$cluster)),
       n = table(hdbscan_out$cluster))

n_clusters <- nrow(clus_tbl) -1

n_no_cluster <- clus_tbl %>% filter(clus == 0) %>% pull(n) %>% unname
prop_no_cluster <- (n_no_cluster / sum(clus_tbl$n)) %>% unname

main_plot <-
  ggplot(pages_umap_output,
         aes(V1, V2)) +
  geom_point(
    aes(colour = factor(hdbscan_out$cluster),
      #  size =  pages_umap_input_selected$page_wordcount_norm  ,
        shape = pages_umap_input$continent,
      size = 3
    )) +
  scale_size(guide = FALSE) +
  scale_shape(name = "Continent") +
  scale_color_viridis_d(guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.8)) +
  xlab("") +
  ylab("")

# train a feature-selecting classificator like random forests on
# the cluster labels

rand_forest_input <-
  pages_umap_input_selected %>%
  mutate(clus = hdbscan_out$cluster) %>%
  filter(clus != 0)

library(caret)

fit <- train(
  clus ~ .,
  data = rand_forest_input,
  method = "ranger",
  trControl = trainControl(method="cv",
                           number = 10,
                           allowParallel = TRUE,
                           verbose = TRUE),
  importance = 'permutation')

var_imp_tbl <- tibble(var = row.names(varImp(fit)$importance),
                      imp = varImp(fit)$importance$Overall) %>%
  mutate(var = case_when(
    var == "page_wordcount_norm" ~ "Wordcount",
    var == "page_wikilinks_in_norm" ~ "Wikilinks in",
    var == "page_wikilinks_out_per_word_norm" ~ "Wikilinks out",
    var == "page_cited_items_on_per_word_norm" ~ "Reference list",
    TRUE ~ var
  ))



theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size,
           base_family = base_family) %+replace%
    theme(panel.grid = element_blank() )
}

sub_plot <-
  ggplot(var_imp_tbl,
         aes(reorder( var, -imp ),
             imp)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_nogrid(base_size = 6)

# plot plus subplot
main_plot +
  annotation_custom(ggplotGrob(sub_plot),
                    xmin = 6,
                    xmax = 4,
                    ymin= 5.5,
                    ymax= 3.5)

ggsave(here::here("analysis/figures/wh_country_umap_hdbscan.png"),
                  width = 10,
                  height = 6,
                  dpi = 300)

