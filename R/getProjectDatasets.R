#' @title getProjectDatasets:
#'
#' @description
#'
#' @param datastoreID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getProjectDatasets(projectID, cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite dplyr
#' @export
#' @return NULL
#'
getProjectDatasets <- function(projectID, cdms_host = 'https://cdms.nptfisheries.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/project/getprojectdatasets')

  query_list <- list(id = projectID)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = query_list)

  httr::stop_for_status(req,
                        task = paste0('query projects from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  df <- dplyr::select(df, DatasetID = Id, ProjectID = ProjectId, DatastoreID = DatastoreId, Name, Description, CreateDateTime)

  return(df)
}
