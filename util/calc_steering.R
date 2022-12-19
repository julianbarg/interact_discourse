source(here("util", "euclidean.R"))

calc_steering <- function(df, cutoff = 50){
  df %>%
    filter(words >= cutoff) %>%
    # filter(group %in% c("anti", "science")) %>%
    group_by(title, id) %>%
    filter(!is.na(get_remark_topic(topics))) %>%
    arrange(order) %>%
    filter(! max(n()) == 1) %>%
    mutate(dist = euclidean(topics, list(first(topics))),
           topic = get_remark_topic(list(first(topics))),
           n = row_number() - 1) %>%
    mutate(n = if_else(n == 0, 0, n/(n() - 1))) %>%
    ungroup() %>%
    filter(!topic %in% c(11, 6)) %>%
    mutate(description = recode(topic, !!! topics_c)) %>%
    mutate(description = fct_reorder(description, topic)) %>%
    select(title, id, n, dist, topic, group, everything()) %>%
    arrange(title, id, n)
}
