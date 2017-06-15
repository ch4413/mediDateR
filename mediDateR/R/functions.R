#' Roll Up Contiguous Data
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' \dontrun{data_roll_up(data)}
#'
#' @export
data_roll_up <- function(data) {
  data %>%
    dplyr::mutate( FLAG = ifelse( dtstart != lag(dtend) + 1  | is.na(lag(dtend)) , 1 , 0)) %>%
    dplyr::mutate( GROUP = cumsum(FLAG)) %>%
    dplyr::group_by(id, GROUP) %>%
    dplyr::summarise( START = min(dtstart) , FINISH = max(dtend)) %>%
    dplyr::select(-GROUP)
}

#' Read in Data
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
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

}
