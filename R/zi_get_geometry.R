#' Download and Optionally Geoprocess ZCTAs
#'
#' @description This function returns geometric data for ZIP Code Tabulation
#'     Areas (ZCTAs), which are rough approximations of many (but not all)
#'     USPS ZIP codes. Downloading and processing these data will be heavily
#'     affected by your internet connection, your choice for the \code{cb}
#'     argument, and the processing power of your computer (if you select
#'     specific counties).
#'
#' @usage zi_get_geometry (year, style = "zcta5", return = "GEOID",
#'     state = NULL, county = NULL, cb = FALSE, starts_with = NULL,
#'     includes = NULL, excludes = NULL, method, shift_geo = FALSE)
#'
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data between 2010 and 2021.
#' @param style A character scalar - either \code{"zcta5"} or \code{"zcta3"}.
#'     See Details below.
#' @param return A character scalar; if \code{"id"} (default), only the five-digit
#'     number of each ZCTA (or three-digit if \code{style = "zcta3"}) is returned.
#'     This is the only valid option for  \code{style = "zcta3"}. For
#'     \code{style = "zcta5"}, if \code{return = "full"}, all TIGER/Line columns
#'     are returned.
#' @param state A scalar or vector with character state abbreviations
#'     (e.x. \code{"MO"}) or numeric FIPS codes (e.x. \code{29}). ZCTAs that
#'     are within the given states (determined based on a combination of
#'     \code{year} and \code{method}) will be returned. See Details below for
#'     more information. This argument is optional unless a argument is also
#'     specified for \code{county}.
#' @param county A character scalar or vector with character GEOIDs (e.x.
#'     \code{"29510"}). ZCTAs that are within the given states (determined based
#'     on a combination of \code{year} and \code{method}) will be returned. See
#'     Details below for more information. This argument is optional.
#' @param cb A logical scalar; if \code{FALSE}, the most detailed TIGER/Line
#'     data will be used for \code{style = "zcta5"}. If \code{TRUE}, a
#'     generalized (1:500k) version of the data will be used. The generalized
#'     data will download significantly faster, though they show less detail.
#'     According to the \code{tigris::zctas()} documentation, the download size
#'     if \code{TRUE} is ~65MB while it is ~500MB if \code{cb = FALSE}. This
#'     argument does not apply to \code{style = "zcta3"}, which only returns
#'     generalized data.
#' @param starts_with A character scalar or vector containing the first two
#'     digits of a GEOID or ZCTA3 value to return. It defaults to \code{NULL},
#'     which will return all ZCTAs in the US. For example, supplying the argument
#'     \code{starts_with = c("63", "64")} will return only those ZCTAs or ZCTA3s
#'     that begin with 63 or 64. If you supply a state or a county, that will limit
#'     the data this argument is applied to, potentially leading to missed ZCTAs.
#' @param includes A character scalar or vector containing GEOID's or ZCTA3 values
#'     to include when finalizing output. This may be necessary depending on what
#'     is identified with the \code{method} argument.
#' @param excludes A character scalar or vector containing GEOID's or ZCTA3 values
#'     to exclude when finalizing output. This may be necessary depending on what
#'     is identified with the \code{method} argument.
#' @param method A character scalar - either \code{"intersect"} or \code{"centroid"}.
#'     See Details below.
#' @param shift_geo A logical scalar; if \code{TRUE}, Alaska, Hawaii, and Puerto Rico
#'     will be re-positioned so that the lie to the southwest of the continental
#'     United States. This defaults to \code{FALSE}, and can only be used when
#'     states are not listed for the \code{state} argument.
#'
#' @details This function contains options for both the type of ZCTA and,
#'     optionally, for how state and county data are identified. For type,
#'     either five-digit or three-digit ZCTA geometries are available. The
#'     three-digit ZCTAs were created by geoprocessing the five-digit boundaries
#'     for each year, and then applying a modest amount of simplification
#'     (with \code{sf::st_simplify()}) to reduce file size. The source files
#'     are available on GitHub at \url{https://github.com/chris-prener/zcta3}.
#'
#'     Since ZCTAs cross state lines, two methods are used to create these
#'     geometry data for years 2012 and beyond for states and all years for counties.
#'     The \code{"intersect"} method  will return ZCTAs that border the states or
#'     counties selected. In most  cases, this will result in more ZCTAs being
#'     returned than are actually within the states or counties selected.
#'     Conversely, the \code{"centroid"} method will return only ZCTAs whose
#'     centroids (geographical centers) lie within the states or counties named.
#'     In most cases, this will return fewer ZCTAs than actually lie within the
#'     states or counties selected. Users will need to review their data carefully
#'     and will likely need to use the \code{include} and \code{exclude} arguments
#'     to finalize the geographies returned.
#'
#'     For state-level data in 2010 and 2011, the Census Bureau published individual
#'     state files that will be utilized automatically by \code{zippeR}. If
#'     county-level data are requested for these years, the state-specific file
#'     will be used as a base before identifying ZCTAs within counties using
#'     either the \code{"intersect"} or \code{"centroid"} method described above.
#'
#' @return A \code{sf} object with ZCTAs matching the parameters specified above
#'     (either a nationwide file, a specific state or states, or a specific
#'     county or counties).
#'
#' @export
zi_get_geometry <- function(year, style = "zcta5", return = "id",
                            state = NULL, county = NULL, cb = FALSE,
                            starts_with = NULL, includes = NULL, excludes = NULL,
                            method, shift_geo = FALSE){

  # check inputs
  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2021.")
  }

  if (year %in% c(2010:2021) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value between years 2010 and 2021.")
  }

  if (style %in% c("zcta5", "zcta3") == FALSE){
    stop("The 'style' value provided is invalid. Please select either 'zcta5' or 'zcta3'.")
  }

  if (return %in% c("id", "full") == FALSE){
    stop("The 'return' value provided is invalid. Please select either 'id' or 'full'.")
  }

  if (style == "zcta3" & year %in% c(2010, 2020, 2021) == FALSE){
    stop(paste0("ZCTA3 data for ", year, " are not yet available."))
  }

  if (style == "zcta3" & return == "full"){
    warning("The 'full' option for 'return' is not available for 'zcta3' data. Please use 'id' instead.")
  }

  if (style == "zcta3" & cb == TRUE){
    warning("The 'cb' argument does not apply to 'zcta3' data.")
  }

  if (shift_geo == TRUE & is.null(state) == FALSE){
    stop("The 'shift_geo' functionality can only be used when you are returning data for all states.")
  }

  ## validate state (using tigris workflow)
  if (is.null(state) == FALSE){
    state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))
  }

  if (is.null(county) == FALSE & is.null(state) == TRUE){
    stop("Please provide at least one state abbreviation or FIPS code for the 'state' argument that corresponds to data passed to the 'county' argument.")
  }

  if (is.null(state) == FALSE){
    if (missing(method) == TRUE){
      stop("Please select a valid method for returning ZCTA values. Your choices are 'centroid' and 'intersect'. See documentation for details.")
    }

    if (method %in% c("centroid", "intersect") == FALSE){
      stop("The two valid methods for returning ZCTA values are 'centroid' and 'intersect'. See documentation for details.")
    }
  }

  ## check year
  if (year == 2011){
    year <- 2010
  }

  # includes and excludes vectors
  # validate counties

  # call sub functions
  if (style == "zcta5"){

    out <- zi_get_zcta5(year = year, return = return, state = state,
                        county = county, cb = cb, starts_with = starts_with,
                        includes = includes, excludes = excludes,
                        method = method)

  } else if (style == "zcta3"){

    out <- zi_get_zcta3(year = year, state = state,
                        county = county, cb = cb, starts_with = starts_with,
                        includes = includes, excludes = excludes,
                        method = method)

  }

  # shift geometry
  if (shift_geo == TRUE){
    out <- tigris::shift_geometry(out, position = "below")
  }

  # return output
  return(out)

}

## Sub Function for ZCTA5
zi_get_zcta5 <- function(year, return = "id", state, county, cb, starts_with,
                         includes, excludes, method){

  # global variables
  GEOID10 = GEOID20 = NULL

  # download geometry
  if (is.null(state) == FALSE & year == 2010){

    ## tigris call
    out <- tigris::zctas(year = 2010, state = state, cb = cb)

  } else if (is.null(state) == FALSE & is.null(county) == TRUE) {

    ## tigris call
    out <- tigris::zctas(year = year, cb = cb)

    ## generate vector of requested state ZCTAs
    zcta_vec <- zi_list_zctas(year = year, state = state, method = method)

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

    ## subset based on year
    if (year < 2020){
      out <- dplyr::filter(out, GEOID10 %in% zcta_vec == TRUE)
    } else if (year >= 2020){
      out <- dplyr::filter(out, GEOID20 %in% zcta_vec == TRUE)
    }

  } else if (is.null(state) == FALSE & is.null(county) == FALSE){

    ## tigris call
    out <- tigris::zctas(year = year, cb = cb)

    ## geoprocess based on county to produced vector of ZTAs
    zcta_vec <- zi_process_county(cb = cb, state = state, county = county,
                                  year = year, zcta = out, method = method,
                                  style = "zcta5")

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

    ## subset based on year
    if (year < 2020){
      out <- dplyr::filter(out, GEOID10 %in% zcta_vec == TRUE)
    } else if (year >= 2020){
      out <- dplyr::filter(out, GEOID20 %in% zcta_vec == TRUE)
    }

  } else if (is.null(state) == TRUE & is.null(county) == TRUE){

    ## tigirs call
    out <- tigris::zctas(year = year, cb = cb)

    ## subset based on year
    if (is.null(excludes) == FALSE){
      if (year < 2020){
        out <- dplyr::filter(out, GEOID10 %in% excludes == FALSE)
      } else if (year >= 2020){
        out <- dplyr::filter(out, GEOID20 %in% excludes == FALSE)
      }
    }
  }

  # subset based on starts with
  if (is.null(starts_with) == FALSE){
    if (year < 2020){
      out <- dplyr::filter(out, substr(GEOID10, 1, 2) %in% starts_with == TRUE)
    } else if (year >= 2020){
      out <- dplyr::filter(out, substr(GEOID20, 1, 2) %in% starts_with == TRUE)
    }
  }

  # subset columns based on return
  if (return == "id"){
    if (year < 2020){
      out <- dplyr::select(out, GEOID10)
    } else if (year >= 2020){
      out <- dplyr::select(out, GEOID20)
    }
  }

  # return output
  return(out)

}

## Sub Function for Processing County-level Data
zi_process_county <- function(cb, state, county, year, zcta, method, style){

  # global variables
  GEOID = GEOID10 = GEOID20 = NULL

  # download and prep geometry
  counties <- suppressMessages(tigris::counties(cb = cb, state = state, year = year))
  counties <- dplyr::select(counties, GEOID)
  counties <- dplyr::filter(counties, GEOID %in% county)

  # calculate centroids
  if (method == "centroid"){
    zcta <- sf::st_centroid(zcta)
  }

  # create simplified data
  if (style == "zcta5"){
    if (year < 2020){
      zcta <- dplyr::select(zcta, GEOID10)
    } else if (year >= 2020) {
      zcta <- dplyr::select(zcta, GEOID20)
    }
  }

  # geoprocess
  intersect <- suppressWarnings(sf::st_intersection(zcta, counties))

  # create output
  if (style == "zcta5"){
    if (year < 2020){
      out <- intersect$GEOID10
    } else if (year >= 2020) {
      out <- intersect$GEOID20
    }
  } else if (style == "zcta3"){
    out <- intersect$ZCTA3
  }

  # return output
  return(out)

}

## Sub Function for ZCTA3
zi_get_zcta3 <- function(year, state, county, cb, starts_with,
                         includes, excludes, method){

  # global variables
  ZCTA3 = GEOID10 = GEOID20 = NULL

  # create value
  val <- paste0("zcta3_", year)

  # download geometry
  if (is.null(state) == FALSE & is.null(county) == TRUE) {

    ## github download
    out <- sf::st_read(zcta3_url[[val]])

    ## generate vector of requested state ZCTAs
    zcta_vec <- zi_list_zctas(year = year, state = state, method = method)
    zcta_vec <- unique(substr(zcta_vec, 1, 3))

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

    ## subset based on year
    if (year < 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)
    } else if (year >= 2020){
      out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)
    }

  } else if (is.null(state) == FALSE & is.null(county) == FALSE){

    ## github download
    out <- sf::st_read(zcta3_url[[val]])

    ## geoprocess based on county to produced vector of ZTAs
    zcta_vec <- zi_process_county(cb = cb, state = state, county = county,
                                  year = year, zcta = out, method = method,
                                  style = "zcta3")

    ## add inclusions, remove exclusions
    zcta_vec <- unique(c(zcta_vec, includes))
    zcta_vec <- zcta_vec[zcta_vec %in% excludes == FALSE]

    ## subset based on year
    out <- dplyr::filter(out, ZCTA3 %in% zcta_vec == TRUE)

  } else if (is.null(state) == TRUE & is.null(county) == TRUE){

    ## github download
    out <- sf::st_read(zcta3_url[[val]])

    ## subset based on year
    if (is.null(excludes) == FALSE){
      out <- dplyr::filter(out, ZCTA3 %in% excludes == FALSE)
    }
  }

  # subset based on starts with
  if (is.null(starts_with) == FALSE){
    out <- dplyr::filter(out, substr(ZCTA3, 1, 2) %in% starts_with == TRUE)
  }

  # return output
  return(out)

}
