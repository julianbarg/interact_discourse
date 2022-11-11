get_frequency <- function(df){
  n_documents = n_distinct(df$id)
  df %>%
    group_by(token) %>%
    summarize(frequency = n(),
              share = frequency / n_documents,
              .groups = "drop") %>%
    select(token, frequency, share)
}
