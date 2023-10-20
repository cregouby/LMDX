response <- c('{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "$200.00 47|67"}], "product": "", "property": "", "tv_address": ""}</Extraction>',
              '{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "200.00 47|67"}], "product": "", "property": "", "tv_address": ""}
</Extraction>',
              '{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "14086 29|09", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "200.00 47|67"}], "product": "", "property": "", "tv_address": ""}
</Extraction>',
              '{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "$200.00 47|67"}], "product": "", "property": "", "tv_address": ""}
</Extraction>',
              '{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "14086 29|09", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "200.00 47|67"}], "product": "", "property": "", "tv_address": ""}
</Extraction>',
              '{"advertiser": "Mike Carr for Jackson Co States Atty 38|13", "agency": "", "contract_num": "14086 29|09", "flight_from": "03/03/20 33|17", "flight_to": "03/03/20 33|17", "gross_amount": "$600.00 94|69", "line_item": [{"channel": "WSIL-TV 73|15", "program_desc": "", "program_end_date": "03/08/20 15|67", "program_start_date": "03/02/20 15|67", "sub_amount": "$200.00 47|67"}], "product": "", "property": "", "tv_address": ""}
</Extraction>')

removed_trailer <- purrr::map_chr(response, ~stringr::str_remove(.x, "[^}]+$"))
response_layout_df <- purrr::map_dfr(removed_trailer, ~jsonlite::fromJSON(.x))

test_that("remove_layout works", {
  expect_error(removed_layout <- remove_layout(response_layout_df), 
               NA)
  expect_true(is.data.frame(removed_layout))
})

test_that("decode_all_sample works", {
  expect_error(decoded <- decode_all_sample(response), 
               NA)
  expect_true(is.data.frame(decoded ))
})

test_that("majority_vote works", {
  expect_error(majority_vote(decode_all_sample(response)), 
               NA)
})