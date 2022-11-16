get_compound_candidates <- function(content,
                                    known_entities,
                                    custom_stopwords,
                                    min_count,
                                    size){
  source(here("util", "clean_document.R"))
  stopwords <- tidytext::get_stopwords() %>%
    filter(! word %in% c("no")) %>%
    mutate(word = str_replace_all(word, "'", "[']?")) %>%
    add_row(word = "[a-z]{1,2}", .before = 1) %>%
    bind_rows(tibble(word = custom_stopwords)) %>%
    {str_c(" ", .$word, " ", collapse = "|")}

  content %>%
    quanteda.textstats::textstat_collocations(size = size,
                                              min_count = min_count) %>%
    mutate(collocation = str_glue(" {collocation} ")) %>%
    filter(! str_detect(collocation, regex(stopwords, ignore_case = T))) %>%
    mutate(collocation = str_trim(collocation))
}
