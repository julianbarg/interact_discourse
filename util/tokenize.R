tokenize <- function(df, custom_stopwords){
  stopwords <- tidytext::get_stopwords() %>%
    filter(! word %in% c("no")) %>%
    mutate(word = str_replace_all(word, "'", "[']?")) %>%
    add_row(word = ".{1,2}", .before = 1) %>%
    bind_rows(tibble(word = custom_stopwords)) %>%
    {str_c("^", .$word, "$", collapse = "|")}

  df_clean <- df %>%
    mutate(content = str_remove_all(content, regex("[^'_\\- [:alpha:]]"))) %>%
    tidytext::unnest_tokens("token", "content") %>%
    # tidytext::unnest_ngrams("token", "content", n = 3, n_min = 1) %>%
    filter(! str_detect(token, regex(stopwords))) %>%
    mutate(token = str_replace_all(token, "_", " ")) %>%
    mutate(token = tm::stemDocument(token)) %>%
    count(order, person, token) %>%
    mutate(n = if_else(str_detect(token, " "),
                       as.integer(n * 2),
                       n))
    # count(order, person, token) %>%
    # mutate(n = if_else(str_detect(token, " "), as.integer(3 * n), n))
}
