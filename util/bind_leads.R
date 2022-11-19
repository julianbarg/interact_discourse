bind_leads <- function(df, max_lead = 40){
  leads <- as.double(1:max_lead)
  lead_one <- function(df, lead_n){
    rename_col <- set_names("temp_lead", glue("lead_person_{lead_n}"))
    df %>%
      summarize(temp_lead = person == lead(person, lead_n),
                "count_{{lead_n}}" := if_else(temp_lead, n_words, 0L)) %>%
      rename(all_of(rename_col))
  }
  map_dfc(leads, ~lead_one(df, .x)) %>%
    mutate(order = 1:n())%>%
    summarize(
      remain_remarks = rowSums(across(starts_with("lead_person_")), na.rm = T),
      remain_words = rowSums(across(starts_with("count_")), na.rm = T))
}
