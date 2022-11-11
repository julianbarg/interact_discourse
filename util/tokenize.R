tokenize <- function(df, known_entities, compounds){
  stopwords <- tidytext::get_stopwords() %>%
    mutate(word = str_remove_all(word, regex("[^ [:alpha:]]"))) %>%
    filter(! word == "no") %>%
    {.$word}

  count_compounds <- function(df, compounds){
    df %>%
      tidytext::unnest_ngrams("token", "content", n = 2) %>%
      filter(token %in% compounds) %>%
      count(order, person, token)
  }

  remove_compounds <- function(df, compounds){
    compound_regex <- regex(str_c(compounds, collapse = "|"),
                            ignore_case = T)
    df %>%
      mutate(content = str_remove_all(content, compound_regex))
  }

  document_clean <- df %>%
    mutate(content = str_remove_all(content, "[^ -[[:alpha:]]]")) %>%
    mutate(content = str_replace_all(content, known_entities))
  compound_counts <- count_compounds(document_clean, compounds)

  document_clean %>%
    remove_compounds(compounds) %>%
    tidytext::unnest_tokens(token, content) %>%
    count(order, person, token) %>%
    filter(! token %in% stopwords) %>%
    mutate(token = SnowballC::wordStem(token)) %>%
    filter(str_length(token) > 4) %>%
    mutate(token = str_replace_all(token, "_", " ")) %>%
    bind_rows(compound_counts)
}
