source(here("util", "get_remark_topic.R"))

count_convo <- function(df){
  df %>%
    group_by(title, id) %>%
    arrange(desc(order)) %>%
    mutate(remaining_words = cumsum(words) - words,
           remaining_remarks = cumsum(n()) - 1) %>%
    arrange(order) %>%
    filter(words >= 50) %>%
    filter(order == first(order)) %>%
    ungroup() %>%
    mutate(topic = get_remark_topic(topics)) %>%
    filter(!topic %in% c(11, 6)) %>%
    mutate(description = recode(topic, !!! topics_c)) %>%
    mutate(description = fct_reorder(description, topic))
}
