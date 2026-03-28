library(tidyverse)
library(here)
library(furrr)
library(future)

options(warn = 1)

# safe log
safelog <- function(p) {
  ifelse(is.na(p) | p <= 0 | is.infinite(p), 0, log2(p))
}

# gain estimation function
gain_feature <- function(p_t, m_t, gain_tx) {
  gain <- p_t / m_t * gain_tx
}

# Gain estimation for combined spoken and written texts

# prepare prior need probability
d <- read_csv(here("data", "d_leuven.csv"), show_col_types = FALSE)
p_c <- d %>%
  group_by(category, level, domain) %>%
  summarise(p_c = sum(p), .groups = "drop")

# load context-dependent probabilities
probs <- read_csv(here("data/probs", "all_probs.csv"), show_col_types = FALSE)

probs <- probs %>%
  # create unique sentence identifier
  mutate(sent_id = paste0(domain, "_", row_id, "_", target_word)) %>%
  select(sent_id, target_word, probe_word, p=prob_norm)

mts <- probs %>%
  distinct(sent_id, target_word) %>%
  group_by(target_word) %>%
  summarise(m_t = n_distinct(sent_id), .groups = "drop") %>%
  left_join(d %>% filter(level=="terminal") %>% distinct(target_word, category), by = "target_word") %>%
  left_join(p_c %>% distinct(category, p_t=p_c), by = "category") %>%
  select(category, p_t, m_t)

all_ids <- unique(probs$sent_id)

# parallel run
no_cores <- (availableCores() - 1) %/% 2
plan(multicore, workers = no_cores)

# run and save both
message("Running feature gain estimation: COMBINED")
per_sentence <- future_map(
  all_ids,
  function(id) {

    d_small <- probs %>%
      filter(sent_id == id) %>%
      left_join(d %>%
                  select(probe_word=target_word, concept, category, starts_with("f_")) %>%
                  unique(), by = "probe_word")

    target_small <- unique(d_small$target_word)
    category_small <- d %>% filter(target_word == target_small, level == "terminal") %>% distinct(category) %>% pull()
    p_t_small <- mts %>% filter(category == category_small) %>% distinct(p_t) %>% pull()
    m_t_small <- mts %>% filter(category == category_small) %>% distinct(m_t) %>% pull()

    f_none <- d_small %>%
      filter(concept == category) %>%
      summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
      pivot_longer(cols = everything(), names_to = "feature", values_to = "f_none")

    f_c <- d_small %>%
      group_by(category) %>%
      summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
      ungroup() %>%
      pivot_longer(cols = -category, names_to = "feature", values_to = "f_c")

    f_t <- d_small %>%
      filter(category == category_small) %>%
      group_by(category) %>%
      summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
      ungroup() %>%
      pivot_longer(cols = -category, names_to = "feature", values_to = "f_t") %>%
      select(-category)

    d_gain_small <- d_small %>%
      select(sent_id, target_word, probe_word, concept, category) %>%
      filter(target_word == probe_word) %>%
      left_join(p_c, by = "category") %>%
      left_join(f_c, by = "category") %>%
      left_join(f_none, by = "feature") %>%
      left_join(f_t, by = "feature") %>%
      mutate(gain_tx = f_t * safelog(f_c / f_none) + (1-f_t) * safelog( (1-f_c) / (1-f_none)) ) %>%
      group_by(domain, level, category, target_word) %>%
      # sum across all features for a given category
      summarise(gain_tx = sum(gain_tx), .groups = "drop") %>%
      mutate(sent_id=id,
             p_t = p_t_small,
             m_t = m_t_small)
  },
  .progress = TRUE
)

all <- list_rbind(per_sentence) %>%
  # apply gain estimation function
  mutate(gain = gain_feature(p_t, m_t, gain_tx)) %>%
  # sum across all targets that belong to category c
  group_by(category, level, domain) %>%
  summarise(gain = sum(gain), .groups = "drop")

out_path <- here("output", "results", "llm_analysis_features.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(all, out_path)
message(" Finished COMBINED. Results written to: ", out_path)

warnings()

# Gain estimation for spoken and written texts separately

d <- read_csv(here("data", "d_leuven.csv"), show_col_types = FALSE)
probs_raw <- read_csv(here("data/probs", "all_probs.csv"), show_col_types = FALSE)

spoken_types <- c("OTHERSP", "CONVRSN")

probs_raw <- probs_raw %>%
  mutate(modality = if_else(doc_type %in% spoken_types, "spoken", "written"))

# we need to normalize across 199 targets for spoken and 253 for written texts
# so we set up helper function as below
prepare_modality_inputs <- function(modality_label, probs_raw, d) {

  probs_mod <- probs_raw %>%
    filter(modality == modality_label) %>%
    # create unique sentence identifier
    mutate(sent_id = paste0(domain, "_", row_id, "_", target_word)) %>%
    select(sent_id, target_word, probe_word, prob_norm)

  targets_mod <- unique(probs_mod$target_word)

  # normalize contextual probabilities
  probs_mod <- probs_mod %>%
    filter(probe_word %in% targets_mod) %>%
    group_by(sent_id, target_word) %>%
    mutate(total = sum(prob_norm)) %>%
    ungroup() %>%
    mutate(prob_norm = prob_norm / total) %>%
    select(-total)

  # normalize prior need probabilities
  d_mod <- d %>%
    filter(target_word %in% targets_mod)

  total_p <- d_mod %>%
    distinct(concept, p) %>%
    summarise(total_p = sum(p)) %>%
    pull(total_p)

  p_c_mod <- d_mod %>%
    mutate(p = p / total_p) %>%
    group_by(category, level, domain) %>%
    summarise(p_c = sum(p), .groups = "drop")

  # prepare m_t and p_t using these renormalized data
  mts_mod <- probs_mod %>%
    distinct(sent_id, target_word) %>%
    group_by(target_word) %>%
    summarise(m_t = n_distinct(sent_id), .groups = "drop") %>%
    left_join(d %>% filter(level == "terminal") %>% distinct(target_word, category),
              by = "target_word") %>%
    left_join(p_c_mod %>% distinct(category, p_t = p_c),
              by = "category") %>%
    select(category, p_t, m_t)

  list(
    probs = probs_mod,
    p_c   = p_c_mod,
    mts   = mts_mod
  )
}

run_feature_gain <- function(probs_data, p_c_data, mts_data, d) {

  all_ids <- unique(probs_data$sent_id)

  per_sentence <- future_map(
    all_ids,
    function(id) {

      d_small <- probs_data %>%
        filter(sent_id == id) %>%
        transmute(sent_id, target_word, probe_word, p = prob_norm) %>%
        left_join(
          d %>%
            select(probe_word = target_word, concept, category, starts_with("f_")) %>%
            distinct(),
          by = "probe_word"
        )

      target_small   <- unique(d_small$target_word)
      category_small <- d %>% filter(target_word == target_small, level == "terminal") %>% distinct(category) %>% pull()
      p_t_small <- mts_data %>% filter(category == category_small) %>% distinct(p_t) %>% pull()
      m_t_small <- mts_data %>% filter(category == category_small) %>% distinct(m_t) %>% pull()

      f_none <- d_small %>%
        filter(concept == category) %>%
        summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
        pivot_longer(cols = everything(), names_to = "feature", values_to = "f_none")

      f_c <- d_small %>%
        group_by(category) %>%
        summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
        ungroup() %>%
        pivot_longer(cols = -category, names_to = "feature", values_to = "f_c")

      f_t <- d_small %>%
        filter(category == category_small) %>%
        group_by(category) %>%
        summarise(across(starts_with("f_"), ~ sum(.x * p) / sum(p))) %>%
        ungroup() %>%
        pivot_longer(cols = -category, names_to = "feature", values_to = "f_t") %>%
        select(-category)

      d_gain_small <- d_small %>%
        select(sent_id, target_word, probe_word, concept, category) %>%
        filter(target_word == probe_word) %>%
        left_join(p_c_data, by = "category") %>%
        left_join(f_c, by = "category") %>%
        left_join(f_none, by = "feature") %>%
        left_join(f_t, by = "feature") %>%
        mutate(gain_tx = f_t * safelog(f_c / f_none) + (1-f_t) * safelog( (1-f_c) / (1-f_none)) ) %>%
        group_by(domain, level, category, target_word) %>%
        # sum across all features for a given category
        summarise(gain_tx = sum(gain_tx), .groups = "drop") %>%
        mutate(sent_id=id,
               p_t = p_t_small,
               m_t = m_t_small)
    },
    .progress = TRUE
  )

  list_rbind(per_sentence) %>%
    # apply gain estimation function
    mutate(gain = gain_feature(p_t, m_t, gain_tx)) %>%
    # sum across all targets that belong to category c
    group_by(category, level, domain) %>%
    summarise(gain = sum(gain), .groups = "drop")
}

# run and save spoken
message(" Running feature gain estimation: SPOKEN")
inp_spoken <- prepare_modality_inputs("spoken", probs_raw, d)
features_spoken <- run_feature_gain(inp_spoken$probs, inp_spoken$p_c, inp_spoken$mts, d)

out_spoken <- here("output", "results", "llm_analysis_features_spoken.csv")
dir.create(dirname(out_spoken), recursive = TRUE, showWarnings = FALSE)
write_csv(features_spoken, out_spoken)
message(" Finished SPOKEN. Results written to: ", out_spoken)

warnings()

# run and save written
message(" Running feature gain estimation: WRITTEN")
inp_written <- prepare_modality_inputs("written", probs_raw, d)
features_written <- run_feature_gain(inp_written$probs, inp_written$p_c, inp_written$mts, d)

out_written <- here("output", "results", "llm_analysis_features_written.csv")
dir.create(dirname(out_written), recursive = TRUE, showWarnings = FALSE)
write_csv(features_written, out_written)
message(" Finished WRITTEN. Results written to: ", out_written)

warnings()
