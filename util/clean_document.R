clean_document <- function(entry, known_entities){
  entry %>%
    mutate(content = str_remove_all(content,
                                    regex("\\([^\\)]*\\)|\\[[^\\]]*\\]"))) %>%
    mutate(content = str_remove_all(content, regex(
      "Senator [A-Z][a-z]*|[Cc]hairman [A-Z][a-z]*|[Ss]peaker [A-Z][a-z]*"
      ))) %>%
    mutate(content = str_remove_all(content, regex("[^-.,'_ [:alpha:]]"))) %>%
    mutate(content = str_replace_all(content, regex(" {2,}"), " ")) %>%
    mutate(content = str_to_lower(content)) %>%
    mutate(content = str_replace_all(content, known_entities))
 }
