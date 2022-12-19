get_remark_topic <- function(column){
  map_int(column, ~ {if (any(is.na(unlist(.x)))) {
      as.integer(NA)
    } else {
      which(unlist(.x) == max(unlist(.x)))
    }
  })
}
