#' @importFrom httr2 request req_method req_headers req_body_json req_perform resp_body_json
#' @importFrom dplyr mutate
#' @importFrom lubridate as_datetime
#' @importFrom broman hex2dec
#' @importFrom stringr str_remove str_length str_sub
#' @importFrom base as.numeric as.integer paste0
#' @importFrom purrr discard map_df pluck

#' Build Log
#'
#' This function builds a log based on the provided parameters. The type of log can be specified as "event" or "transfer".
#'
#' @param v_from The starting block number for the log.
#' @param v_to The ending block number for the log.
#' @param v_offset The offset for the log.
#' @param v_limit The limit for the log.
#' @param v_address The address for the log.
#' @param v_topic0 The topic0 for the log.
#' @param v_topic1 The topic1 for the log.
#' @param v_topic2 The topic2 for the log.
#' @param v_topic3 The topic3 for the log.
#' @param v_topic4 The topic4 for the log.
#' @param v_sender The sender for the log.
#' @param v_recipient The recipient for the log.
#' @param v_order The order of the log.
#' @param type The type of log to build. Can be "event" or "transfer".
#' @return A list containing the range, options, criteriaSet, and order of the log.
#' @export
#'
#'

build_log <- function(v_from=0, v_to=20000000, v_offset=0, v_limit=100000,
                      v_address="", v_topic0="", v_topic1="", v_topic2="", v_topic3="", v_topic4="",
                      v_sender="", v_recipient="", v_order="asc", type="event") {
  range <- list(unit="block", from=v_from, to=v_to)
  range <- range[!(is.na(range) | range == "")]
  options <- list(offset=v_offset, limit=v_limit)
  options <- options[!(is.na(options) | options == "")]

  if (type == "event") {
    criteriaSet <- data.frame(address=v_address, topic0=v_topic0, topic1=v_topic1, topic2=v_topic2, topic3=v_topic3, topic4=v_topic4)
  } else if (type == "transfer") {
    criteriaSet <- data.frame(txOrigin=v_address, sender=v_sender, recipient=v_recipient)
  }
  criteriaSet <- subset(criteriaSet, select = c(which(colSums(is.na(criteriaSet) | criteriaSet == "") == 0)))
  order <- v_order

  return(list(range=range, options=options, criteriaSet=criteriaSet, order=order))
}


#' @title Perform an API call to retrieve log data
#' @param search_api A list containing the range, options, criteriaSet and order of the search
#' @param type The type of log data to retrieve. Can be "event" or "transfer"
#' @param event_url The URL of the event log API endpoint
#' @param transfer_url The URL of the transfer log API endpoint
#' @return A data frame containing the retrieved log data
#' @examples
#' search_api <- list(range=range, options=options, criteriaSet=criteriaSet, order=order)
#' v_log_api_call(search_api, type = "event")
#' v_log_api_call(search_api, type = "transfer")

v_log_api_call <- function(search_api, type = "event",
                           event_url = "https://mainnet.veblocks.net/logs/event",
                           transfer_url = "https://mainnet.veblocks.net/logs/transfer") {
  if (type == "event") {
    url <- event_url
  } else if (type == "transfer") {
    url <- transfer_url
  }

  request(url) %>%
    req_method("POST") %>%
    req_headers(accept = "application/json") %>%
    req_body_json(search_api) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = F) %>%
    map_df(~as.data.frame(t(unlist(.x)))) %>%
    mutate(meta.blockTimestamp=lubridate::as_datetime(as.numeric(meta.blockTimestamp)))
}


