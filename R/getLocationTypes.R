#' @title getLocationTypes:
#'
#' @description
#'
#' @param datastoreID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getLocationTypes(cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite dplyr
#' @export
#' @return NULL
#'
#'
getLocationTypes <- function(cdms_host = 'https://cdms.nptfisheries.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/location/getlocationtypes')

  # GET request with query parameters
  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query location types from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  return(df)

}
