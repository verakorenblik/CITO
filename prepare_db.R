create_scoring_rules <- function(information, responses = c("A", "B", "C")) {
  
  scoring_rules <- information %>%
    dplyr::select(item_id, key) %>%
    dplyr::distinct() %>%  # removes duplicates across booklet versions
    tidyr::crossing(response = responses) %>%
    dplyr::mutate(
      item_score = ifelse(response == key, 1, 0)
    ) %>%
    dplyr::select(item_id, response, item_score) %>%
    dplyr::mutate(
      item_id = factor(item_id),
      item_score = as.integer(item_score)
    )
  
  return(scoring_rules)
}

