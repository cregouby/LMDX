#' Remove layout information from each entity
#'
#' @param df the dataframe of parsed response
#'
#' @return the same dataframe with layout information removed in the values
#' @export
#'
#' @importFrom dplyr mutate across where
remove_layout <- function(df) {
  df %>%
    mutate(across(where(is.character), ~stringr::str_replace(.x, "\\s\\d+\\|\\d+$", ""))) %>% 
    mutate(across(where(is.data.frame), ~remove_layout(.x)))
}

#' @importFrom purrr map_chr map_dfr
decode_all_sample <- function(response, taxonomy) {
  removed_trailer <- map_chr(response, ~stringr::str_remove(.x, "[^}]+$"))
  response_layout_df <- map_dfr(removed_trailer, ~jsonlite::fromJSON(.x))
  remove_layout(response_layout_df)
}

#' @importFrom dplyr summarize_all
majority_vote <- function(response_df) {
  response_df %>% summarize_all(~names(which.max(table(.))))
}

