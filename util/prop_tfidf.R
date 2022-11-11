prop_tfidf <- function(tokens, prop){
  tokens %>%
    tidytext::bind_tf_idf(token, id, n) %>%
    slice_min(tf_idf, prop = prop) %>%
    arrange(desc(tf_idf)) %>%
    mutate(token = str_trunc(token, 25)) %>%
    mutate(
      tf = round(tf, 5),
      tf_idf = round(tf_idf, 7)) %>%
    select(token, tf, tf_idf, id)
}
