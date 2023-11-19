#' Turn a json taxonomy into LMDX prompt <task> part
#'
#' @param taxonomy either 
#'   - a json representing the schema of the entity to extract
#'   - a value within `VRDU_Ad_Buy`, `VRDU_Registration` or `CORD` for the schema used in original paper.
#' @param into_yaml shall we include the taxonomy in json format (`FALSE`), the default or turn it into YAML encoding 
#'  in the prompt (`TRUE`).
#'
#' @return a character vector being the task part of the prompt
#' @export
lmdx_task <- function(taxonomy, into_yaml) {
  UseMethod("lmdx_task")
}

#' @export
lmdx_task.default <- function(taxonomy, into_yaml) {
  rlang::abort(paste0(taxonomy, " is not recognized as a supported taxonomy"))
}

#' @export
lmdx_task.json <- function(taxonomy, into_yaml = FALSE) {
  stopifnot(
    "taxonomy is not recognized as a valid json" = jsonlite::validate(taxonomy)
  )
  prompt <-
    "From the document, extract the following entities with Named Entity Recognition and format result into:"
  if (into_yaml) {
    entities <- glue::glue("```yaml\n{yaml::as.yaml(jsonlite::fromJSON(taxonomy))}\n```")
  } else {
    entities <- jsonlite::minify(taxonomy)
  }
  glue::glue("<Task>\n{prompt}\n{entities}\n</Task>\n<Extraction>\n")
}

#' @export
lmdx_task.character <- function(taxonomy, into_yaml = FALSE) {
  stopifnot(
    "taxonomy is not recognized within ['CORD', 'VRDU_Ad_Buy', 'VRDU_Registration']" = taxonomy %in% c("CORD", "VRDU_Ad_Buy", "VRDU_Registration")
  )
  prompt <-
    "From the document, extract the text values and tags of the following entities:"
  json <- switch (
    taxonomy,
    "CORD" = jsonlite::minify(cord_json),
    "VRDU_Ad_Buy" = jsonlite::minify(vrdu_ad_buy_json),
    "VRDU_Registration" = jsonlite::minify(vrdu_registration_json),
  )
  if (into_yaml) {
    entities <- glue::glue("```yaml\n{yaml::as.yaml(jsonlite::fromJSON(json))}\n```")
  } else {
    entities <- json
  }
    
  glue::glue("<Task>\n{prompt}\n{entities}\n</Task>\n<Extraction>\n")
}

cord_json <- '{
  "line_item": [
    {
      "discount_price": "",
      "identifier": "",
      "name": "",
      "other": "",
      "quantity": "",
      "sub_name": [],
      "sub_price": [],
      "sub_quantity": [],
      "subtotal_price": "",
      "total_price": "",
      "unit_price": ""
    }
  ],
  "subtotal": {
    "discount_price": "",
    "other": [],
    "service_price": "",
    "subtotal_price": [],
    "tax_price": []
  },
  "total": {
    "cash_price": [],
    "change_price": "",
    "credit_card_price": "",
    "emoney_price": "",
    "line_item_quantity_count": "",
    "line_item_type_count": "",
    "other": "",
    "total_price": []
  }
}'

vrdu_registration_json <- '{
  "file_date": "",
  "foreign_principle_name": "",
  "registrant_name": "",
  "registration_num": "",
  "signer_name": "",
  "signer_title": ""
}'

vrdu_ad_buy_json <- '{
  "advertiser": "",
  "agency": "",
  "contract_num": "",
  "flight_from": "",
  "flight_to": "",
  "gross_amount": "",
  "line_item": [
    {
      "channel": "",
      "program_desc": "",
      "program_end_date": "",
      "program_start_date": "",
      "sub_amount": ""
    }
  ],
  "product": "",
  "property": "",
  "tv_address": ""
}'
