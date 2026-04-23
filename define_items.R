get_booklet_item_sets <- function(information) {
  
  items <- information %>%
    dplyr::pull(item_id) %>%
    unique()
  
  items_1 <- information %>%
    dplyr::filter(booklet_id == "1") %>%
    dplyr::pull(item_id) %>%
    unique()
  
  items_2 <- information %>%
    dplyr::filter(booklet_id == "2") %>%
    dplyr::pull(item_id) %>%
    unique()
  
  return(list(
    items   = items,
    items_1 = items_1,
    items_2 = items_2,
    items_overlap = intersect(items_1, items_2),
    items_1_only = setdiff(items_1, items_2),
    items_2_only = setdiff(items_2, items_1)
  ))
}