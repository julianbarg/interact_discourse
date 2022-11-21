show_terms <- function(input_model, input_topic) {
  betas <- tidytext:::tidy.LDA(input_model, matrix = "beta")
  betas %>%
    filter(topic == input_topic) %>%
    slice_max(beta, n = 40) %>%
    mutate(term = str_trunc(term, 22)) %>%
    print()
}

get_examples <- function(input_data, input_k, long_length, min_length){
  get_examples_k <- function(entry, input_k){
    suitable_remarkes <- filter(entry$token_counts,
                                total_tokens >= min_length)
    entry$gammas %>%
      filter(topic == input_k) %>%
      inner_join(suitable_remarkes, by = "order") %>%
      mutate(length = if_else(total_tokens >= long_length,
             "long", "short")) %>%
      group_by(length) %>%
      slice_max(gamma, n = 10) %>%
      mutate(title = entry$title) %>%
      left_join(entry$document, by = c("order"))
  }
  map_dfr(input_data, ~ get_examples_k(.x, input_k)) %>%
    group_by(length) %>%
    slice_max(gamma, n = 10) %>%
    ungroup() %>%
    arrange(desc(gamma)) %>%
    relocate(title, .after = last_col()) %>%
    print()
}

show_topic <- function(model, topic, data, long_length, min_length){
  show_terms(model, topic)
  get_examples(data, topic, long_length, min_length)
}
