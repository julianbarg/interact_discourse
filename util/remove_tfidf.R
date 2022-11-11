remove_tfidf <- function(entry, tfidf_df, cutoff){
  tfidf_specific <- tfidf_df %>%
    separate(id, into = c("person", "title"), sep = "@") %>%
    filter(title == entry$title)
  entry %>%
    modify_at("tokens",
              ~ anti_join(.x, tfidf_specific, by = c("token", "person")))
}
