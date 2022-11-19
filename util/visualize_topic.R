visualize_topic <- function(model, topic_k, n, recode = c("a" = "a")){
  plot <- model %>%
    tidytext:::tidy.LDA(matrix = "beta") %>%
    filter(topic == topic_k) %>%
    mutate(term = recode(term, !!! recode)) %>%
    slice_max(beta, n = n) %>%
    mutate(term = as_factor(term)) %>%
    ggplot(aes(term, beta)) +
    geom_col() +
    xlab("Term") +
    ylab("Weight (beta)") +
    theme_bw() +
    theme(
      axis.text.x =
        element_text(angle = 90, vjust = 0.4, hjust=1),
      text = element_text(size = 12))
  plot
}
