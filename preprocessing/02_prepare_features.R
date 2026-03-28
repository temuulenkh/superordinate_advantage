# This R file prepares feature data sets.

library(tidyverse)
library(here)
library(testthat)


# A) Prepare Rosch feature data

a <- read_csv(here("rawdata/rosch1976.csv"), show_col_types = FALSE)

# create distinct feature IDs
a_features <- a %>%
  distinct(feature) %>%
  mutate(feature_id = paste0("f_", row_number()))

# add these feature IDs to the dataframe
a <- a %>%
  left_join(a_features, by = "feature")
a %>%
  distinct(superordinate, feature_id) %>%
  write_csv(here("data", "rosch_feat_id.csv"))

# create category and domain at different levels
b <- a %>%
  select(category=subordinate, domain=superordinate) %>%
  unique()

c <- a %>%
  select(category=basic, domain=superordinate) %>%
  unique()

d <- a %>%
  select(domain=superordinate) %>%
  unique() %>%
  mutate(category = domain)

# combine all
all <- bind_rows(b, c, d)

# now prepare final data
rosch_feat <- a %>%
  # if a feature is present, we assign 1, otherwise 0
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = c(subordinate, basic, superordinate),
    names_from = feature_id,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(concept = subordinate) %>%
  pivot_longer(cols = c(subordinate, basic, superordinate),
               names_to = "level",
               values_to = "category") %>%
  left_join(all, by = "category") %>%
  select(concept, category, level, domain, everything()) %>%
  write_csv(here("data", "rosch_feat.csv"))


# B) Prepare Leuven feature data

animal_exemplar <- read_csv(
  unz(here("rawdata/dedeyne2008/cvsdata/exemplar feature judgments/TypeIIAnimalExemplarFeatureMatrix.zip"),
      "TypeIIAnimalExemplarFeatureMatrix-sum.CSV"), show_col_types = FALSE)

animal_exemplar <- animal_exemplar[-1, ] %>%
  rename(feature_dutch = `...1`, feature_english = `feature/ exemplar ENGLISH`, freq = `...3`) %>%
  # fix typos to match names with animal_category (cockchafer is chosen over maybug due to higher frequency in COCA)
  rename(canary=cabary, chicken=kip, viper=viger, cockchafer=maybug)

animal_category <- read_csv(
  unz(here("rawdata/dedeyne2008/cvsdata/exemplar feature judgments/TypeIVAnimalCategoryFeatureMatrix.zip"),
      "TypeIVAnimalCategoryFeatureMatrix-sum.CSV"), show_col_types = FALSE)

animal_category <- animal_category[-1, ] %>%
  rename(feature_dutch = `...1`, feature_english = `feature / exemplar ENGLISH`, freq = `...3`)

# we will use both exemplar and category features
animal <- bind_rows(animal_exemplar, animal_category) %>%
  # some features are mentioned for both exemplar and category features, so we combine verification frequencies across them
  mutate(across(monkey:turtle, ~ as.numeric(.x))) %>%
  # we will use Dutch features
  group_by(feature_dutch) %>%
  summarise(across(monkey:turtle, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  ungroup()

animal_long <- animal %>%
  pivot_longer(
    cols = -c(feature_dutch),
    names_to = "concept",
    values_to = "value"
  ) %>%
  # replace spaces with underscores for compound words
  mutate(concept = str_replace_all(concept, " ", "_")) %>%
  # if it is verified by at least one participant, then we assume that feature exists for a given concept
  mutate(value = ifelse(value >= 1, 1, 0)) %>%
  mutate(superordinate = "animals")

artifact_exemplar <- read_csv(
  unz(here("rawdata/dedeyne2008/cvsdata/exemplar feature judgments/TypeIIArtifactsExemplarFeatureMatrix.zip"),
      "TypeIIArtifactsExemplarFeatureMatrix-sum.CSV"), show_col_types = FALSE)

artifact_exemplar <- artifact_exemplar[-1, ] %>%
  rename(feature_dutch = `...1`, feature_english = `feature/ exemplar ENGLISH`, freq = `...3`)  %>%
  # fix typos to match names with artifact_category
  rename("flute...73"=flute, "flute...75"=recorder, "oil can"=oilcan, cannon=canon)

artifact_category <- suppressWarnings(
  read_csv(unz(here("rawdata/dedeyne2008/cvsdata/exemplar feature judgments/TypeIVArtifactsCategoryFeaturesMatrix.zip"),
      "TypeIVArtifactsCategoryFeaturesMatrix-sum.CSV"), show_col_types = FALSE))

artifact_category <- artifact_category[-1, ] %>%
  rename(feature_dutch = `...1`, feature_english = `feature / exemplar ENGLISH`, freq = `...3`)  %>%
  # fix typo
  rename(cannon=canon) %>%
  select(where(~ !all(is.na(.))))

# we will use both exemplar and category features
artifact <- bind_rows(artifact_exemplar, artifact_category) %>%
  # some features are mentioned for both exemplar and category features, so we combine verification frequencies across them
  mutate(across(`can opener`:saw, ~ as.numeric(.x))) %>%
  # we will use Dutch features
  group_by(feature_dutch) %>%
  summarise(across(`can opener`:saw, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  ungroup()

artifact_long <- artifact %>%
  pivot_longer(
    cols = -c(feature_dutch),
    names_to = "concept",
    values_to = "value"
  ) %>%
  # replace spaces with underscores for compound words
  mutate(concept = str_replace_all(concept, " ", "_")) %>%
  # if it is verified by at least one participant, then we assume that feature exists for a given concept
  mutate(value = ifelse(value >= 1, 1, 0)) %>%
  # let's combine verification frequency across English lemmas for which Dutch has two distinct words
  mutate(concept = case_when(
    concept %in% c("motorbike...95", "motorbike...110") ~ "motorbike",
    concept %in% c("crowbar...150", "crowbar...144") ~ "crowbar",
    concept %in% c("truck...120", "truck...97") ~ "truck",
    concept %in% c("flute...73", "flute...75") ~ "flute",
    TRUE ~ concept
  )) %>%
  group_by(feature_dutch, concept) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(value = if_else(value >= 1, 1, 0)) %>%
  ungroup() %>%
  mutate(superordinate = "artifacts")

# combine animal and artifact domains, and add category information
category <- read_csv(here("data", "dedeyne_freq.csv"), show_col_types = FALSE) %>%
  select(category, concept) %>%
  unique()

check <- category %>% group_by(concept) %>% summarise(n = n_distinct(category)) %>%
  ungroup() %>% filter(n > 1)
# there are 8 concepts that have two different intermediate level categories

# we decided to drop amphibians category all together because all their concepts belong to the reptiles category
category <- category %>%
  filter(category != "amphibians")

check <- category %>% group_by(concept) %>% summarise(n = n_distinct(category)) %>%
  ungroup() %>% filter(n > 1)
# there are still 3 concepts that have two different intermediate level categories, we will leave them as they are

dedeyne_all <- bind_rows(animal_long, artifact_long)

dedeyne_withid <- dedeyne_all %>%
  # assign unique feature IDs
  mutate(feature_id = paste0("f_", match(feature_dutch, unique(feature_dutch)))) %>%
  select(-feature_dutch)

dedeyne_withid %>%
  distinct(starting=superordinate, feature_id) %>%
  write_csv(here("data", "dedeyne_feat_id.csv"))

dedeyne_clean <- dedeyne_withid %>%
  select(-superordinate) %>%
  pivot_wider(names_from = feature_id, values_from = value, values_fill = 0) %>%
  mutate(concept = str_to_lower(concept),
         concept = str_replace_all(concept, "[()]", ""))

check <- dedeyne_clean %>% select(concept) %>%
  anti_join(category, by = "concept") %>%
  nrow()
# all concepts should have frequency infromation
expect_equal(check, 0)

categories <- dedeyne_clean %>%
  select(concept) %>%
  inner_join(category, by = "concept") %>%
  mutate(superordinate = case_when(
    category %in% c("mammals", "birds", "fish", "insects", "reptiles") ~ "animals",
    TRUE ~ "artifacts"
  )) %>%
  rename(intermediate=category, starting=superordinate) %>%
  mutate(terminal=concept, domain=starting) %>%
  pivot_longer(cols = c(terminal, intermediate, starting),
               names_to = "level",
               values_to = "category")

dedeyne <- dedeyne_clean %>%
  inner_join(categories, by = "concept") %>%
  select(concept, category, level, domain, everything()) %>%
  distinct() %>%
  write_csv(here("data", "dedeyne_feat.csv"))

expect_equal(dedeyne %>% select(concept) %>% unique() %>% nrow(), 291)
# there are unique set of 291 concepts
expect_equal(dedeyne %>% select(concept) %>% unique() %>% filter(str_detect(concept, "_")) %>% nrow(), 28)
# out of which, 28 are compounds
expect_equal(dedeyne %>% filter(domain == "animals") %>% select(concept) %>% unique() %>% nrow(), 129)
expect_equal(dedeyne %>% filter(domain == "artifacts") %>% select(concept) %>% unique() %>% nrow(), 162)
# out of which, 129 belong to animal domain and 162 to artifact domain

