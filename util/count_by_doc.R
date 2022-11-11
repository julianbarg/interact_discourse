count_by_doc <- function(entry){
  entry$tokens %>%
    group_by(person, token) %>%
    summarize(n = sum(n), .groups = "drop") %>%
    mutate(id = str_c(person, entry$title, sep = "@")) %>%
    select(id, token, n)
}
