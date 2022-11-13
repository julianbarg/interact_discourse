tokenize <- function(df, custom_stopwords, known_entities){
  stopwords <- tidytext::get_stopwords() %>%
    filter(! word %in% c("no")) %>%
    mutate(word = str_replace_all(word, "'", "[']?")) %>%
    add_row(word = "[a-z]{1,2}", .before = 1) %>%
    bind_rows(tibble(word = custom_stopwords)) %>%
    {str_c(" ", .$word, " ", collapse = "|")}

  df_clean <- df %>%
    mutate(content = str_remove_all(
      content, regex("\\([^\\)]*\\)|\\[[^\\]]*\\]"))) %>%
    mutate(content = str_remove_all(content, regex(
"Senator [A-Z][a-z]*|[Cc]hairman [A-Z][a-z]*|[Ss]peaker [A-Z][a-z]*"))) %>%
    mutate(content = str_remove_all(content, regex("[^-.,' [:alpha:]]"))) %>%
    mutate(content = str_to_lower(content)) %>%
    mutate(content = str_replace_all(content, known_entities)) %>%
    tidytext::unnest_ngrams("token", "content", n = 3, n_min = 2) %>%
    # tidytext::unnest_ngrams("token", "content", n = 3, n_min = 1) %>%
    mutate(token = str_glue(" {token} ")) %>%
    filter(! str_detect(token, regex(stopwords, ignore_case = T))) %>%
    mutate(token = str_trim(token)) %>%
    mutate(token = str_replace(token, "_", " ")) %>%
    mutate(token = str_remove_all(token, regex("[^ [:alpha:]]"))) %>%
    mutate(token = tm::stemDocument(token)) %>%
    count(order, person, token)
    # count(order, person, token) %>%
    # mutate(n = if_else(str_detect(token, " "), as.integer(3 * n), n))
}
