library(tidyverse)
library(here)
library(furrr)

options(warn = 1)

# helper functions
clip01 <- function(x, eps = 1e-12) {
  pmin(pmax(x, eps), 1 - eps)
}

safelog <- function(p) {
  -log2(p)
}

# cross-entropy
xent <- function(p, q, eps = 1e-12) {
  p <- clip01(p, eps)
  q <- clip01(q, eps)
  p * safelog(q) + (1 - p) * safelog(1 - q)
}

# KL divergence
error <- function(s, l, eps = 1e-12) {
  xent(s, l, eps) - xent(s, s, eps)
}

# score a range of parameter values
p_range <- seq(0, 1, by = 0.02)

d_prob <- expand.grid(p_i = p_range, p_s = p_range) %>%
  mutate(p_u = 1 - p_i - p_s) %>%
  filter(p_u >= 0) %>%
  mutate(setting = row_number())

# load Leuven data
d <- read_csv(here("data", "d_leuven.csv"), show_col_types=FALSE)

# create unique combinations of two pairs:
unique_targets <- unique(d$concept)
all_x <- t(combn(unique_targets, 2)) %>%
  as.data.frame() %>%
  rename(b1 = V1, b2 = V2)

pairs_base <- all_x %>%
  # add prior need:
  left_join(d %>% distinct(b1=concept, p1=p, s1=domain), by = "b1") %>%
  left_join(d %>% distinct(b2=concept, p2=p, s2=domain), by = "b2") %>%
  left_join(d %>% filter(concept!=category, !(category %in% c("animals", "artifacts"))) %>%
              distinct(b1=concept, i1=category), by = "b1", relationship = "many-to-many") %>%
  left_join(d %>% filter(concept!=category, !(category %in% c("animals", "artifacts"))) %>%
              distinct(b2=concept, i2=category), by = "b2", relationship = "many-to-many") %>%
  # estimate normalized p(b1) * p(b2)
  mutate(p1_p2 = p1 * p2,
         p1_p2 = p1_p2 / sum(p1_p2)) %>%
  mutate(s_same = ifelse(s1 == s2 & i1 != i2, 1, 0),
         i_same = ifelse(s1 == s2 & i1 == i2, 1, 0),
         s_diff = ifelse(s1 != s2 & i1 != i2, 1, 0)) %>%
  # estimate normalized p(b1) * p(b2) given the same starting or terminal
  mutate(p_i_same = p1_p2 * i_same,
         p_i_same = p_i_same / sum(p_i_same),
         p_s_same = p1_p2 * s_same,
         p_s_same = p_s_same / sum(p_s_same),
         p_s_diff = p1_p2 * s_diff,
         p_s_diff = p_s_diff / sum(p_s_diff)) %>%
  # estimate context dependent probability
  mutate(p1_x = p1 / (p1 + p2),
         p2_x = p2 / (p1 + p2))

# parallel run
no_cores <- (availableCores() - 1) %/% 2
plan(multicore, workers = no_cores)

message(" Running identity gain estimation ")

all_categories <- unique(d$category)
all_settings <- unique(d_prob$setting)

per_category <- future_map(
  all_categories,
  function(cat) {

    d_small <- d %>%
      filter(category == cat) %>%
      distinct(category, level, domain)

    map_dfr(
      all_settings,
      function(set) {

        pairs <- pairs_base %>%
          # add range of probabilities
          cross_join(d_prob %>% filter(setting == set)) %>%
          # estimate final probability of context x given the ratio
          mutate(p_x = p_i_same * p_i + p_s_same * p_s + p_s_diff * p_u) %>%
          select(setting, p_i, p_s, p_u, s1, s2, i1, i2, b1, b2, p_x, p1_x, p2_x)

        gains <- d_small %>%
          cross_join(pairs) %>%
          mutate(
            error_without_label = p1_x * safelog(p1_x) + p2_x * safelog(p2_x),
            e1 = case_when(
              level == "terminal"         ~ ifelse(b1 != b2 & b1 == category, 0, safelog(p1_x)),
              level == "intermediate"  ~ ifelse(i1 != i2 & i1 == category, 0, safelog(p1_x)),
              level == "starting" ~ ifelse(s1 != s2 & s1 == category, 0, safelog(p1_x))),
            e2 = case_when(
              level == "terminal"         ~ ifelse(b1 != b2 & b2 == category, 0, safelog(p2_x)),
              level == "intermediate"  ~ ifelse(i1 != i2 & i2 == category, 0, safelog(p2_x)),
              level == "starting" ~ ifelse(s1 != s2 & s2 == category, 0, safelog(p2_x))),
            error_with_label = p1_x * e1 + p2_x * e2,
            gain = p_x * (error_without_label - error_with_label)
          ) %>%
          group_by(category, level, domain, setting, p_i, p_s, p_u) %>%
          summarise(gain = sum(gain), .groups = "drop")

        gains
      }
    )
  },
  .progress = TRUE
)

all <- list_rbind(per_category)

out_path <- here("output", "results", "nonling_context_leuven_identity.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(all, out_path)

message(" Finished IDENTITY. Results written to: ", out_path)

# Feature-based model:
features <- d %>%
  select(concept, starts_with("f_")) %>%
  distinct() %>%
  pivot_longer(cols=-concept, names_to = "feature_id", values_to = "value")

message(" Running feature gain estimation ")

# process pairs by chunks
chunk_size <- 500
pair_chunks <- split(
  pairs_base,
  (seq_len(nrow(pairs_base)) - 1) %/% chunk_size
)

# compute context-dependent errors
pairs_base_f_list <- lapply(
  pair_chunks,
  function(pb) {
    pb %>%
      left_join(features %>% rename(b1=concept, f1=value), by = "b1", relationship = "many-to-many") %>%
      left_join(features %>% rename(b2=concept, f2=value), by = c("b2", "feature_id"), relationship = "many-to-many") %>%
      mutate(fnone = p1_x * f1 + p2_x * f2,
             e1_x = error(f1, fnone),
             e2_x = error(f2, fnone)) %>%
      group_by(s1, s2, i1, i2, b1, b2, p1_x, p2_x, p_i_same, p_s_same, p_s_diff) %>%
      # sum across features
      summarise(e1_x = sum(e1_x, na.rm=TRUE),
                e2_x = sum(e2_x, na.rm=TRUE),
                .groups="drop")
  }
)
pairs_base_f <- list_rbind(pairs_base_f_list)

per_category_feature <- future_map(
  all_categories,
  function(cat) {

    d_small <- d %>%
      filter(category == cat) %>%
      distinct(category, level, domain)

    map_dfr(
      all_settings,
      function(set) {

        pairs <- pairs_base_f %>%
          # add range of probabilities
          cross_join(d_prob %>% filter(setting == set)) %>%
          # estimate final probability of context x given the ratio
          mutate(p_x = p_i_same * p_i + p_s_same * p_s + p_s_diff * p_u) %>%
          select(setting, p_i, p_s, p_u, s1, s2, i1, i2, b1, b2,
                 p_x, p1_x, p2_x, e1_x, e2_x)

        gains <- d_small %>%
          cross_join(pairs) %>%
          mutate(
            error_without_label = p1_x * e1_x + p2_x * e2_x,
            e1 = case_when(
              level == "terminal"         ~ ifelse(b1 != b2 & b1 == category, 0, e1_x),
              level == "intermediate"  ~ ifelse(i1 != i2 & i1 == category, 0, e1_x),
              level == "starting" ~ ifelse(s1 != s2 & s1 == category, 0, e1_x)),
            e2 = case_when(
              level == "terminal"         ~ ifelse(b1 != b2 & b2 == category, 0, e2_x),
              level == "intermediate"  ~ ifelse(i1 != i2 & i2 == category, 0, e2_x),
              level == "starting" ~ ifelse(s1 != s2 & s2 == category, 0, e2_x)),
            error_with_label = p1_x * e1 + p2_x * e2,
            gain = p_x * (error_without_label - error_with_label)
          ) %>%
          group_by(category, level, domain, setting, p_i, p_s, p_u) %>%
          summarise(gain = sum(gain), .groups = "drop")

        gains
      }
    )
  },
  .progress = TRUE
)

all_feature <- list_rbind(per_category_feature)

out_path_feature <- here("output", "results", "nonling_context_leuven_features.csv")
dir.create(dirname(out_path_feature), recursive = TRUE, showWarnings = FALSE)
write_csv(all_feature, out_path_feature)

message(" Finished FEATURE. Results written to: ", out_path_feature)

warnings()
