#' Remove layout information from each entity
#'
#' @param df the dataframe of parsed response
#'
#' @return the same dataframe with layout information removed in the values
#' @export
#'
#' @importFrom dplyr mutate accross where
remove_layout <- function(df) {
  df %>%
    mutate(across(where(is.character), ~stringr::str_replace(.x, "\\s\\d+\\|\\d+$", ""))) %>% 
    mutate(across(where(is.data.frame), ~remove_layout(.x)))
}

decode_all_sample <- function(response, taxonomy) {
  removed_trailer <- purrr::map_chr(response, ~stringr::str_remove(.x, "[^}]+$"))
  response_layout_df <- purrr::map_dfr(removed_trailer, ~jsonlite::fromJSON(.x))
  remove_layout(response_layout_df)
}

majority_vote <- function(response_df) {
  response_df %>% dplyr::summarize_all(~names(which.max(table(.))))
}

