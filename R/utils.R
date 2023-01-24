#' @importFrom httr2 request req_method req_headers req_body_json req_perform resp_body_json
#' @importFrom dplyr mutate
#' @importFrom lubridate as_datetime
#' @importFrom broman hex2dec
#' @importFrom stringr str_remove str_length str_sub
#' @importFrom base as.numeric as.integer paste0
#' @importFrom purrr discard map_df pluck
#'
#' @describeParams v_data a string of data, usually in hex format
#' @describeParams interpretation a string indicating how the data should be interpreted
#' "hex" for converting to decimal, "numeric" for converting to numeric, "none" for no conversion
#' @export
#'
v_parse_long_data_string <- function(v_data, interpretation = "hex") {
  long_char <- v_data %>% str_remove("0x")
  len <- str_length(long_char)
  count <- len / 64
  start_vec <- 0:(count-1) * 64 + 1
  stop_vec <- 1:count * 64

  # check for interpretation and perform appropriate conversion
  if (interpretation == "hex") {
    str_sub(string = long_char, start = start_vec, end = stop_vec) %>%
      broman::hex2dec(.)
  } else if (interpretation == "numeric") {
    str_sub(string = long_char, start = start_vec, end = stop_vec) %>%
      paste0("0x",.) %>%
      as.numeric()
  } else if (interpretation == "none") {
    str_sub(string = long_char, start = start_vec, end = stop_vec)
  }
}

#' Convert blockchain hex to Vet Values
#'
#' @param x A hex value representing a Vet token amount
#' @return A numeric value representing the Vet value
#' @examples
#' v_convert_vet("0x13370000000000000000") # returns 13.37
#'
v_convert_vet <- function(x) {
  num <- as.numeric(x)/(10^18)
  round(num,3)
}

#' Get Token ID
#'
#' @param x A hex value representing a Token ID
#' @return The Token ID as a numeric value
#' @examples
#' v_get_token_id("0x0000000000000000000000000000dead") # returns 57005
#'
v_get_token_id <- function(x) {
  str_sub(x,63,66) %>% broman::hex2dec(.)
}

#' Parse Date Time
#'
#' @param x A hex value representing a timestamp
#' @return The timestamp as a datetime object
#' @examples
#' v_get_time("0x5f5e100") # returns 2022-08-01 00:00:00
#'
v_get_time <- function(x) {
  as.numeric(x) %>% lubridate::as_datetime()
}

#' Removes unnecessary 000s
#'
#' @param x An Ethereum address
#' @return The address with any unnecessary 000s removed
#' @examples
#' v_clean_addresses("0x0000000000000000000000000000dead") # returns "0x0000dead"
#'
v_clean_addresses <- function(x) {
  str_remove_all(x,"000000000000000000000000")
}

#' Try Catch Function
#'
#' @param df A dataframe
#' @param error_df A dataframe to return in case of error
#' @return df if no error is thrown, otherwise error_df
#' @examples
#' try_catch(df, error_df = data.frame())
#'
try_catch <- function(df, error_df = data.frame()) {
  tryCatch({
    df
  }, error = function(e) {
    error_df
  })
}
#' Convert a decimal number to unsigned hexadecimal
#'
#' @param num A decimal number to convert
#' @param prefix logical, whether to include the 0x prefix (default: TRUE)
#' @return Returns the converted number as a string
#' @examples
#' v_num_as_uint(123)
#' v_num_as_uint(123, prefix = FALSE)
v_num_as_uint <- function(num, prefix = TRUE) {
  num <- as.integer(num)
  if (prefix) {
    paste0("0x", str_pad(broman::dec2hex(num), width = 64, side = "left", pad = "0"))
  } else {
    str_pad(broman::dec2hex(num), width = 64, side = "left", pad = "0")
  }
}

#' Convert an address to unsigned hexadecimal
#'
#' @param addr An Ethereum address
#' @param prefix logical, whether to include the 0x prefix (default: TRUE)
#' @return Returns the converted address as a string
#' @examples
#' v_addr_as_uint("0x714E34AD16D78eF503Cff5C686975031ebaEce8d")
#' v_addr_as_uint("0x714E34AD16D78eF503Cff5C686975031ebaEce8d", prefix = FALSE)
v_addr_as_uint <- function(addr, prefix = TRUE) {
  if (prefix) {
    str_replace(addr, "0x", "0x000000000000000000000000")
  } else {
    str_replace(addr, "0x", "000000000000000000000000")
  }
}

#' Convert a decimal number to hexadecimal
#'
#' @param x A decimal number to convert
#' @return Returns the converted number as a string
#' @examples
#' v_num_to_hex(123)
v_num_to_hex <- function(x) {
  as.character(as.bigz(x), b = 16)
}


#' Get the latest block number from VeChainThor blockchain
#'
#' @return The latest block number as an integer
#' @export

v_get_best_block <- function() {
  request("https://mainnet.veblocks.net/blocks/best") %>%
    req_perform() %>%
    resp_body_json() %>%
    pluck("number")
}

#' Convert a number to a hexadecimal string
#'
#' @param x A numeric value
#' @return The hexadecimal representation of the input number
#' @export
to_hex <- function(x) {
  as.character(x, b = 16)
}
