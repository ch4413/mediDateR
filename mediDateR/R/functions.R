#' Roll Up Contiguous Data
#'
#' Rolls up contiguous data time windows based on a set \code{start_end_log}
#'
#' @param data a dataframe with attributes:id, dtstart, dtend
#' @param start_end_lag defined lag between the end time and the following start
#' time in days. Set to 1 (day) as default.
#'
#' @return data is returned in a tibble
#'
#' @examples
#' \dontrun{data_roll_up(data)}
#'
#' @export
data_roll_up <- function(data, start_end_lag = 1) {
  data %>%
    dplyr::mutate( flag = ifelse( dtstart != lag(dtend) + start_end_lag  | is.na(lag(dtend)) , 1 , 0)) %>%
    dplyr::mutate( group = cumsum(flag)) %>%
    dplyr::group_by(id, group) %>%
    dplyr::summarise( dtstart = min(dtstart) , dtend = max(dtend)) %>%
    dplyr::select(-group)
}

#' Read in Data
#'
#' Reads in rds or csv data
#'
#' @param filepath the filepath to the raw data
#' @param filetype a character for the file extension ("csv" or "RDS")
#'
#' @return tibble of data or an error for invalid file extensions
#'
#' @examples
#'
#' \dontrun{read_data(filepath, filetype)}
#'
#' @export
read_data <- function(filepath, filetype) {

  if (tolower(filetype) == "rds") {
    readr::read_rds(filepath)
  }
  else if (tolower(filetype) == "csv") {
    readr::read_csv(filepath)
  }
  else{
    message("Error: invalid filetype chosen")
  }

}
