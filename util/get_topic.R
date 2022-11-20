script.dir <- this.path::this.dir()
setwd(script.dir)
args <- commandArgs(trailingOnly=TRUE)

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(yaml)
library(here)

text <- clipr::read_clip(allow_non_interactive = T)

known_entities <- yaml::read_yaml(here("input", "nlp_input.yaml")) %>%
  pluck("known_entities") %>%
  map_dfr(flatten_dfc) %>%
  {set_names(.$token, regex(.$regex, ignore_case = T))}
custom_stopwords <- yaml::read_yaml(here("input", "nlp_input.yaml")) %>%
  pluck("custom_stopwords")
compounds <- read_lines(here("output", "compounds.txt"))
kxl_model <- read_rds(here("output", "topic_model.rds"))
betas <- tidytext:::tidy.LDA(kxl_model, matrix = "beta")
topics <- yaml::read_yaml(here("input", "topics.yaml")) %>%
  map_dfr(flatten)

source(here("util", "clean_document.R"))
source(here("util", "fix_collocations.R"))
source(here("util", "tokenize.R"))
tibble(document = "doc", content = text,
               order = "1", person = "-") %>%
  clean_document(known_entities) %>%
  fix_collocations(compounds) %>%
  tokenize(custom_stopwords) %>%
  inner_join(betas, by = c("token" = "term")) %>%
  mutate(beta = beta * n) %>%
  group_by(topic) %>%
  summarize(beta = sum(beta),  .groups = "drop") %>%
  mutate(total_beta = sum(beta)) %>%
  mutate(gamma = beta / total_beta) %>%
  select(-c(total_beta, beta)) %>%
  arrange(desc(gamma)) %>%
  left_join(select(topics, topic, short), by = "topic") %>%
  rename("topic_n" = "topic", "topic" = "short")
