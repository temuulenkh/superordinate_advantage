# This R file prepares frequency information for Rosch and Leuven concepts

library(tidyverse)
library(here)
library(janitor)


# A) Prepare COCA frequency for Rosch concepts

# On Oct 10, 2025, we obtained the following frequency data from COCA website. We combined frequencies for singular and plural cases.

concepts <- c("delicious apple", "macintosh apple", "freestone peach", "cling peach", "concord grape", "green seedless grape",
             "kitchen table", "dining room table", "kitchen chair", "living room chair", "floor lamp", "desk lamp",
             "classic guitar", "folk guitar", "upright piano", "grand piano", "kettle drum", "bass drum")

#counts <- c(31+77, 5+8, 0+5, 1+12, 37+39, 0+6,
#            4215+92, 1068+22, 237+101, 27+7, 165+65, 211+41,
#           7+2, 15+1, 138+9, 474+50, 13+20, 218+27)

counts <- c(108, 13, 5, 13, 76, 6,
            4307, 1090, 338, 34, 230, 252,
            9, 16, 147, 524, 33, 245)

rosch_freq <- data.frame(
  concept = concepts,
  count = counts,
  stringsAsFactors = FALSE
) %>%
  mutate(total = sum(count),
         p = count / total) %>%
  write_csv(here("data", "rosch_freq.csv"))


# B) Prepare Dutch frequency file Leuven data
# This file will be used to assign categories later, but we used BNC frequency for Leuven data

categories <- c(
  "amphibians", "birds", "fish", "insects", "mammals", "reptiles",
  "clothing", "weapons", "tools", "kitchen utensils", "musical instruments", "vehicles"
)

zip_path <- here("rawdata/dedeyne2008/cvsdata/exemplar judgments/exemplarWFAoAPercKnownRatings.zip")

read_and_clean <- function(category) {
  print(category)
  suppressWarnings(
    read_csv(
      unz(zip_path, paste0("exemplarWFAoAPercKnownRatings-", category, ".CSV")),
      show_col_types = FALSE
    ) %>%
      # deselects columns that have only NA values
      select(where(~ !all(is.na(.)))) %>%
      clean_names() %>%
      # filter out cases where frequency information is not available
      filter(!is.na(log_lemma_freq)) %>%
      rename(lemma_dutch = x1, lemma_english = x2) %>%
      mutate(category = category) %>%
      mutate(
        log_lemma_freq = as.double(log_lemma_freq),
        age_of_acquisition = as.double(age_of_acquisition),
        percent_known = as.double(percent_known)
      )
  )
}

dedeyne_freq_all <- map_dfr(categories, read_and_clean)

dedeyne_freq <- dedeyne_freq_all %>%
  select(category, lemma_dutch, concept=lemma_english, log_freq = log_lemma_freq) %>%
  # insert underscore for compounds
  mutate(concept = str_replace_all(concept, " ", "_")) %>%
  mutate(concept = case_when(
    # fix typo
    concept == "canon"~ "cannon",
    # need to change these to match with feature data
    concept == "double_barreled_shotgun"~ "double_barrelled_shotgun",
    concept == "filling-knife"~ "filling_knife",
    concept == "oilcan"~ "oil_can",
    TRUE ~ concept
  )) %>%
  mutate(concept = str_to_lower(concept),
         concept = str_replace_all(concept, "[()]", "")) %>%
  unique() %>%
  drop_na() %>%
  # transform logarithmic values to raw counts
  mutate(count = round(10^(log_freq))) %>%
  group_by(category, concept) %>%
  # combine frequencies across English lemmas
  summarise(count = sum(count),
            total = 42380000, # this corpus size was given in De Deyne et al. 2008
            p = count / total) %>% # note that these probabilities are not yet normalized
  ungroup() %>%
  write_csv(here("data", "dedeyne_freq.csv"))

