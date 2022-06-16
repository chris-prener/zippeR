#' Load Crosswalk Files
#'
#' @description Since USPS ZIP Codes are not identical to Census Bureau ZCTAs,
#'     data containing ZIP Codes should be crosswalked to identify the proper
#'     ZCTA. The American Academy of Family Physicians publishes open-source
#'     crosswalk files on their UDS Mapper website (\url{https://udsmapper.org/}).
#'     This function loads the crosswalk files
#'     (\url{https://udsmapper.org/zip-code-to-zcta-crosswalk/}) so that they
#'     can be previewed from within R.
#'
#' @usage zi_load_crosswalk(year)
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data for from 2010 to 2020.
#'
#' @return A tibble containing the UDS Mapper crosswalk file for a given year.
#'
#' @export
zi_load_crosswalk <- function(year){

  # global variables
  CityName = PO_NAME = StateAbbr = StateName = ZCTA_USE = ZIP =
    ZIPType = ZIP_TYPE = NULL

  # check inputs
  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between 2010 and 2021.")
  }

  if (year %in% c(2010:2021) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a year between 2010 and 2021.")
  }

  # load data
  if (year == 2021){
    out <- rio::import(file = "https://udsmapper.org/wp-content/uploads/2021/09/ZiptoZcta_Crosswalk_2021.xlsx")
  } else if (year == 2016){
    out <- rio::import(file = "https://udsmapper.org/wp-content/uploads/2021/12/ZipCodetoZCTACrosswalk2016.xlsx")
  } else if (year %in% c(2010, 2011) == TRUE) {
    out <- rio::import(file = paste0("https://udsmapper.org/wp-content/uploads/2021/12/ZIPCodetoZCTACrosswalk", year , ".xls"))
  } else {
    out <- rio::import(file = paste0("https://udsmapper.org/wp-content/uploads/2021/12/ZIPCodetoZCTACrosswalk", year , ".xlsx"))
  }

  # tidy
  if (year %in% c(2010:2014, 2016) == TRUE){

    ## preliminary fixes
    if (year %in% c(2012, 2013) == TRUE){
      out <- dplyr::mutate(out, StateAbbr = ifelse(is.na(StateAbbr) == TRUE, StateName, StateAbbr))
    }

    ## address columns
    out <- dplyr::select(out, ZIP, PO_NAME = CityName, STATE = StateAbbr, ZIP_TYPE = ZIPType, ZCTA = ZCTA_USE)

    ## additional fixes
    if (year == 2014){

      out <- dplyr::mutate(out, ZIP = ifelse(ZIP == "4691", "04691", ZIP))

    } else if (year %in% c(2010:2013) == TRUE){

      # remove military ZIPs
      out <- dplyr::filter(out, ZIP_TYPE != "M")

      # fix capitalization
      out <- dplyr::mutate(out, PO_NAME = stringr::str_to_title(PO_NAME))

      # address ZIP_TYPE
      out <- dplyr::mutate(out, ZIP_TYPE = dplyr::case_when(
        ZIP_TYPE == "U" ~ "Post Office or large volume customer",
        ZIP_TYPE == "P" ~ "Post Office or large volume customer",
        ZIP_TYPE == "L-PY" ~ "L-PY",
        ZIP_TYPE == "S" ~ "ZIP Code area"
      ))

      # fix ZIPs in 2020
      if (year == 2010){
        out <- dplyr::filter(out, ZCTA != "N/A")
      }

    }

  } else if (year %in% c(2017:2021)){

    ## fix ZIP code name
    out <- dplyr::rename(out, ZIP = ZIP_CODE)

    ## remove non-ZCTA geometries
    if (year %in% c(2019:2020)){
      out <- dplyr::filter(out, ZCTA != "No ZCTA")
    }
  }

  # re-order output
  out <- dplyr::arrange(out, ZIP)

  # convert to tibble
  out <- tibble::as_tibble(out)

  # return output
  return(out)

}
