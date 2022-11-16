fix_collocations <- function(entry, collocations){
  collocations <- set_names(str_replace_all(collocations, " ", "_"),
                            collocations)
  entry %>%
    mutate(content =  str_replace_all(content, collocations))
}
