read_data <- function(entry){
  entry$document <- read_lines(here("input", "topics", entry$file))
  if (entry$type == "arena"){
    entry$document <- tibble(content = entry$document) %>%
      filter(str_detect(content,
                        regex("^[A-Z]*\\.? ?[A-Z]{1,}\\.? [A-Z]{2,}:"))) %>%
      mutate(person = str_extract(content, regex("^[^:]*")),
             content = str_remove(content, regex("^[^:]*:")),
             order = row_number()) %>%
      mutate(person = str_to_title(person)) %>%
      select(order, person, content)
  } else if (entry$type == "report"){
    entry$document <- tibble(order = 1,
                             person = "-",
                             content = str_c(entry$document, collapse = " "))
  }
  entry
}
