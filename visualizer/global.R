library(tidyverse)
library(here)
library(glue)
library(shiny)
library(ggsci)
library(yaml)
library(tidytext)
library(reshape2)

## Raw data

kxl_data <- read_rds(here("output", "data_betas.rds"))
topics <- yaml::read_yaml(here("input", "topics.yaml")) %>%
  map_dfr(flatten)
topics_c <- set_names(glue("{topics$topic}: {topics$short}"),
                      topics$topic)
betas <- read_rds(here("output", "topic_model.rds")) %>%
  tidytext:::tidy.LDA(matrix = "beta")

## Useful for referncing

get_tokens <- function(entry){
  entry$tokens %>%
    mutate(source = entry$title) %>%
    select(source, order, person, token, n) %>%
    mutate(order = as.integer(order))
}

arenas <- kxl_data %>%
  keep( ~.x$type == "arena") %>%
  map_dfr(~ keep(.x, names(.x) %in% c("description", "title"))) %>%
  {set_names(.$title, .$description)}
tokens <- kxl_data %>%
  map_dfr(get_tokens)
unique_tokens <- unique(tokens$token) %>%
  sort()

better_colnames <- c(
  "Token" = "token",
  "Topic" = "topic",
  "Gamma" = "gamma",
  "Loading" = "beta",
  "Remark #" = "order",
  "Person" = "person",
  "Content" = "content",
  "Source" = "source",
  "Count" = "n"
)

## Functions

table_styling <- function(df){
  matching_colnames <- better_colnames[better_colnames %in% names(df)]
  df %>%
    rename(!!! matching_colnames)
}

column <- partial(column, style = "border: 1px solid")

plot_topic_bars <- function(entry, na_to_zero = F){
  if (na_to_zero == T){
    treat_na <- function(df){mutate(df, gamma = replace_na(gamma, 0))}
  } else {
    treat_na <- function(df){df}
  }
  entry %>%
    pluck("gammas") %>%
    treat_na() %>%
    mutate(description = recode(topic, !!! topics_c)) %>%
    mutate(description = fct_reorder(description, topic)) %>%
    ggplot(aes(topic, gamma, fill = description)) +
      geom_col() +
    scale_x_continuous(breaks = seq(1, 13, 1)) +
      labs(x = "Topic", y = "Prevalence") +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      guides(fill=guide_legend(ncol=3)) +
      scale_fill_d3(palette = "category20c")

}

