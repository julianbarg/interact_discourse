get_flow <- function(entry){
  topics <- entry$gamma %>%
    pivot_wider(id_cols = order,
                names_from = topic,
                values_from = gamma,
                names_prefix = "topic_") %>%
    rowwise() %>%
    mutate(topics = list(c_across(starts_with("topic_")))) %>%
    ungroup %>%
    select(-starts_with("topic_"))

  entry$document %>%
    filter(! str_detect(person, regex("^Senator"))) %>%
    mutate(words = str_count(content, " ")) %>%
    select(-content) %>%
    arrange(order) %>%
    mutate(change = person != lag(person),
           change = ifelse(is.na(change), F, change),
           group = cumsum(change)) %>%
    select(-change) %>%
    group_by(group) %>%
    arrange(desc(order)) %>%
    mutate(remaining_words = cumsum(words) - words,
           remaining_remarks = 1:n() - 1) %>%
    ungroup() %>%
    arrange(order) %>%
    left_join(topics, by = "order") %>%
    select(order, person, group, words, starts_with("remaining_"), topics)
}
