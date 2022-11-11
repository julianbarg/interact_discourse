get_compound_candidates <- function(documents, known_entities){
  stopwords <- tidytext::get_stopwords() %>%
    mutate(word = str_remove_all(word, regex("[^ [:alpha:]]"))) %>%
    filter(! word == "no") %>%
    {.$word}

  documents %>%
    str_remove_all(regex("[^ [:alpha:]]")) %>%
    str_replace_all(regex(" .{1} "), " ") %>%
    str_replace_all(known_entities) %>%
    str_to_lower() %>%
    quanteda.textstats::textstat_collocations(size = 3) %>%
    separate(collocation,
             into = c("word_1", "word_2", "word_3"),
             sep = " ") %>%
    filter(!(word_1 %in% stopwords | word_2 %in% stopwords
             | word_3 %in% stopwords)) %>%
    unite(word_1, word_2, word_2, col = "collocation", sep = " ")
}
