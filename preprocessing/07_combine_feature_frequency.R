# This R file combines feature and frequency information for Rosch and Leuven concepts

library(tidyverse)
library(here)
library(testthat)

# A) Rosch data

# load features data
fe <- read_csv(here("data", "rosch_feat.csv"), show_col_types = FALSE)

# load frequency data
fq <- read_csv(here("data", "rosch_freq.csv"), show_col_types = FALSE) %>%
  select(concept, p)
expect_equal(sum(fq$p), 1)

# combine feature and frequency datasets
d <- fe %>%
  inner_join(fq, by = "concept") %>%
  select(concept, category, level, domain, p, everything()) %>%
  write_csv(here("data", "d_rosch.csv"))

# B) Leuven data

# load features data
fe <- read_csv(here("data", "dedeyne_feat.csv"), show_col_types = FALSE) %>%
  # fix typo for flea
  mutate(concept = ifelse(concept == "flee", "flea", concept),
         category = ifelse(category == "flee", "flea", category))
expect_equal(length(unique(fe$concept)), 291)

# load frequency data
ani <- read_csv(here("data/bnc_matches", "animals_sampled.csv"), show_col_types = FALSE) %>%
  bind_rows(read_csv(here("data/bnc_matches", "animals_sampled_flea.csv"), show_col_types = FALSE)) %>%
  rename(total = n) %>%
  group_by(target_word, concept, total) %>%
  summarise(sampled = n()) %>%
  ungroup() %>%
  # add across spoken and written corpora
  group_by(target_word, concept) %>%
  summarise(total = sum(total),
            sampled = sum(sampled)) %>%
  ungroup()

ani_wsd <- read_csv(here("data/wsd", "animals_wsd.csv"), show_col_types = FALSE) %>%
  bind_rows(read_csv(here("data/wsd", "animals_wsd_flea.csv"), show_col_types = FALSE)) %>%
  filter(label == "ANIMAL", confidence >= 0.95) %>%
  group_by(target_word, concept) %>%
  summarise(literal = n()) %>%
  ungroup()

animal <- ani %>%
  inner_join(ani_wsd, by = c("target_word", "concept")) %>%
  mutate(literal_prop_in_sampled = literal / sampled,
         literal_in_total = round(total * literal_prop_in_sampled, 0))

art <- read_csv(here("data/bnc_matches", "artifacts_sampled.csv"), show_col_types = FALSE) %>%
  rename(total = n) %>%
  group_by(target_word, concept, total) %>%
  summarise(sampled = n()) %>%
  ungroup() %>%
  # add across spoken and written
  group_by(target_word, concept) %>%
  summarise(total = sum(total),
            sampled = sum(sampled)) %>%
  ungroup()

art_wsd <- read_csv(here("data/wsd", "artifacts_wsd.csv"), show_col_types = FALSE) %>%
  filter(label == "ARTIFACT", confidence >= 0.95) %>%
  group_by(target_word, concept) %>%
  summarise(literal = n()) %>%
  ungroup()

artifact <- art %>%
  inner_join(art_wsd, by = c("target_word", "concept")) %>%
  mutate(literal_prop_in_sampled = literal / sampled,
         literal_in_total = round(total * literal_prop_in_sampled, 0))

fq <- bind_rows(animal, artifact) %>%
  distinct(concept, target_word, count = literal_in_total) %>%
  filter(!is.na(count)) %>%
  mutate(total = sum(count),
         p = count / total) %>%
  select(concept, target_word, p)
expect_equal(length(unique(fq$concept)), 255)
expect_equal(sum(fq$p), 1)

# combine feature and frequency datasets
d <- fe %>%
  inner_join(fq, by = "concept") %>%
  select(concept, target_word, category, level, domain, p, everything())  %>%
  write_csv(here("data", "d_leuven.csv"))
expect_equal(length(unique(d$concept)), 255)
