#' Aggregate ZCTAs to Three-digit ZCTAs
#'
#' @description This function takes input ZCTA data and aggregates it to three-digit
#'    areas, which are considerably larger. These regions are sometimes used in
#'    American health care contexts for publishing geographic identifiers.
#'
#' @usage zi_aggregate(.data, year, extensive = NULL, intensive = NULL,
#'     intensive_method = "mean", survey, output = "tidy", zcta = NULL,
#'     debug = NULL)
#'
#' @param .data A tidy set of demographic data containing one or more variables
#'     that should be aggregated to three-digit ZCTAs. This data frame or tibble
#'     should contain all five-digit ZCTAs within the three digit ZCTAs that you
#'     plan to use for aggregating data. See Details below for formatting
#'     requirements.
#' @param year A four-digit numeric scalar for year. \code{zippeR} currently
#'     supports data for from 2010 to 2020. Different \code{survey} products
#'     are available for different years. See the \code{survey} parameter
#'     for more details.
#' @param extensive A character scalar or vector listing all extensive (i.e.
#'     count data) variables you wish to aggregate. These will be summed. For
#'     American Community Survey data, the margin of error will be calculated by
#'     taking the square root of the summed, squared margins of error for each
#'     five-digit ZCTA within a given three-digit ZCTA.
#' @param intensive A character scalar or vector listing all intensive (i.e.
#'     ratio, percent, or median data) variables you wish to aggregate. These
#'     will be combined using the approach listed for \code{intensive_method}.
#' @param intensive_method A character scalar; either \code{"mean"} (default)
#'     or \code{"median"}. In either case, a weighted approach is used where
#'     total population for each five-digit ZCTA is used to calculate individual
#'     ZCTAs' weights. For American Community Survey Data, this is applied to
#'     the margin of error as well.
#' @param survey A character scalar representing the Census product. It can
#'     be either a Decennial Census product (either \code{"sf1"} or \code{"sf3"})
#'     or an American Community Survey product (either \code{"acs1"},
#'     \code{"acs3"}, or \code{"acs5"}). For Decennial Census calls, only the 2010
#'     Census is available. In addition, if a variable cannot be found in \code{"sf1"},
#'     the function will look in \code{"sf3"}. Also note that \code{"acs3"} was
#'     discontinued after 2013.
#' @param output A character scalar; one of \code{"tidy"} (long output) or
#'     \code{"wide"} depending on the type of data format you want. If you are
#'     planning to join these data with geometric data, \code{"wide"} is the
#'     strongly encouraged format.
#' @param zcta An optional vector of ZCTAs that demographic data are requested
#'     for. If this is \code{NULL}, data will be returned for all ZCTAs. If a
#'     vector is supplied, only data for those requested ZCTAs will be returned.
#'     The vector can be created with \code{zi_get_geometry()}. If
#'     \code{style = "zcta5"}, this vector should be made up of five-digit
#'     \code{GEOID} values. If \code{style = "zcta3"}, this vector should be
#'     made up of three-digital \code{ZCTA3} values.
#' @param debug A logical scalar; if \code{TRUE}, the call made to the
#'     Census API will be returned. This can be very useful in debugging and
#'     determining if error messages returned are due to \code{tidycensus},
#'     \code{zippeR}, or the Census API. Copy to the API call into a browser
#'     and see what is returned by the API directly.
#'
#' @return A tibble containing all aggregated data requested in either
#'     \code{"tidy"} or \code{"wide"} format.
#'
#' @export
zi_aggregate <- function(.data, year, extensive = NULL, intensive = NULL,
                         intensive_method = "mean", survey,
                         output = "tidy", zcta = NULL, debug = NULL){

  # global variables
  GEOID = ZCTA3 = key = variable = NULL

  # evaluate inputs
  if (is.numeric(year) == FALSE){
    stop("The 'year' value provided is invalid. Please provide a numeric value for 2010 or 2020.")
  }

  if (length(survey) > 1){
    stop("One only 'survey' product may be requested at a time.")
  }

  if (survey %in% c("sf1", "sf3", "acs1", "acs3", "acs5") == FALSE){
    stop("The 'survey' requested is invalid. Please choose one of 'sf1', 'sf3', 'acs1', 'acs3', or 'acs5'.")
  }

  if (survey %in% c("sf1", "sf3") == TRUE & year != 2010){
    stop("The 'year' value provided is invalid for Decennial Census data. Only 2010 may be requested currently.")
  }

  if (survey %in% c("acs1", "acs5") == TRUE & year %in% c(2010:2020) == FALSE){
    stop("The 'year' value provided is invalid for 1- or 5-year American Community Survey data. Please provide a year between 2010 and 2020.")
  }

  if (survey == "acs3" & year %in% c(2010:2013) == FALSE){
    stop("The 'year' value provided is invalid for 3-year American Community Survey data. Please provide a year between 2010 and 2013.")
  }

  if (output %in% c("tidy", "wide") == FALSE){
    stop("The 'output' requested is invalid. Please choose one of 'tidy' or 'wide'.")
  }

  if (survey %in% c("sf1", "sf3") == TRUE){
    error <- "Input data appear to be malformed - there should be three columns for Decennial Census data: 'GEOID', 'variable', and 'value'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 3){
      stop(error)
    }

    if (all(names(.data) == c("GEOID", "variable", "value")) == FALSE){
      stop(error)
    }
  } else if (survey %in% c("acs1", "acs3", "acs5") == TRUE){
    error <- "Input data appear to be malformed - there should be four columns for ACS data: 'GEOID', 'variable', 'estimate', and 'moe'. Note that zi_aggregate() only accepts 'tidy' data."

    if (length(names(.data)) != 4){
      stop(error)
    }

    if (all(names(.data) == c("GEOID", "variable", "estimate", "moe")) == FALSE){
      stop(error)
    }
  }

  # set additional arguments
  ## call type
  if (is.null(extensive) == FALSE){
    extensive_id <- TRUE
  } else {
    extensive_id <- FALSE
  }

  if (is.null(intensive) == FALSE){
    intensive_id <- TRUE
  } else {
    intensive_id <- FALSE
  }

  ## debugging to pass to tidycensus
  if (is.null(debug) == FALSE){

    if (debug %in% c("call") == FALSE){
      stop("The 'debug' argument is invalid. Please choose one of 'call', ")
    }

    if (debug == "call"){
      call <- TRUE
    } else {
      call <- FALSE
    }
  } else if (is.null(debug) == TRUE) {
    call <- FALSE
  }

  # prep data
  .data <- dplyr::mutate(.data, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
  .data <- dplyr::arrange(.data, ZCTA3)

  # call underlying tidycensus data
  if (survey %in% c("sf1", "sf3") == TRUE){

    ## summarize data
    if (extensive_id == TRUE & intensive_id == FALSE){

      ## aggregate
      out <- zi_census_extensive(.data)

    } else if (extensive_id == FALSE & intensive_id == TRUE){

      ## calculate weights
      weights <- zi_census_weights(year = year, key = key, call = call)

      ## aggregate
      out <- zi_census_intensive(.data, weights = weights, method = intensive_method)

    } else if (extensive_id == TRUE & intensive_id == TRUE){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive == TRUE)
      intensive_df <- dplyr::filter(.data, variable %in% intensive == TRUE)

      ## calculate weights
      weights <- zi_census_weights(year = year, key = key, call = call)

      ## aggregate
      extensive_df <- zi_census_extensive(extensive_df)
      intensive_df <- zi_census_intensive(intensive_df, weights = weights, method = intensive_method)

      ## combine
      out <- dplyr::bind_rows(extensive_df, intensive_df)
      out <- dplyr::arrange(out, ZCTA3, variable)

    }

  } else if (survey %in% c("acs1", "acs3", "acs5") == TRUE){

    ## summarize data
    if (extensive_id == TRUE & intensive_id == FALSE){

      ## aggregate
      out <- zi_acs_extensive(.data)

    } else if (extensive_id == FALSE & intensive_id == TRUE){

      ## calculate weights
      weights <- zi_acs_weights(year = year, survey = survey, key = key, call = call)

      ## aggregate
      out <- zi_acs_intensive(.data, weights = weights, method = intensive_method)

    } else if (extensive_id == TRUE & intensive_id == TRUE){

      ## subset data
      extensive_df <- dplyr::filter(.data, variable %in% extensive == TRUE)
      intensive_df <- dplyr::filter(.data, variable %in% intensive == TRUE)

      ## calculate weights
      weights <- zi_acs_weights(year = year, survey = survey, key = key, call = call)

      ## aggregate
      extensive_df <- zi_acs_extensive(extensive_df)
      intensive_df <- zi_acs_intensive(intensive_df, weights = weights, method = intensive_method)

      ## combine
      out <- dplyr::bind_rows(extensive_df, intensive_df)
      out <- dplyr::arrange(out, ZCTA3, variable)

    }

  }

  # optionally subset
  if (is.null(zcta) == FALSE){
    out <- dplyr::filter(out, ZCTA3 %in% zcta == TRUE)
  }

  # optionally pivot

  # return output
  return(out)

}


## Extensive Decennial Census
zi_census_extensive <- function(.data){

  # global variables
  ZCTA3 = variable = value = NULL

  ## group by and sum
  .data <- dplyr::group_by(.data, ZCTA3, variable)
  .data <- dplyr::summarise(.data, value = sum(value, na.rm = TRUE))

  ## return output
  return(.data)

}

## Intensive Decennial Census
zi_census_intensive <- function(.data, weights, method){

  # global variables
  ZCTA3 = variable = value = weight = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = "ZCTA3")

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data, value = stats::weighted.mean(value, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(.data, value = spatstat.geom::weighted.median(value, weight, na.rm = TRUE))
  }

  ## return output
  return(.data)

}

## Intensive Census Weights
zi_census_weights <- function(year, key, call){

  # global variables
  GEOID = NAME = ZCTA3 = total_pop = value = weight = NULL

  ## call get_acs
  out <- tidycensus::get_decennial(geography = "zcta", variables = "P001001",
                                   year = year, output = "tidy",
                                   key = key, show_call = call)

  ## prep data
  out <- dplyr::mutate(out, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
  out <- dplyr::select(out, -NAME)
  out <- dplyr::arrange(out, ZCTA3)

  ## group by and sum
  totals <- dplyr::group_by(out, ZCTA3)
  totals <- dplyr::summarise(totals, total_pop = sum(value, na.rm = TRUE))

  ## join
  out <- dplyr::left_join(out, totals, by = "ZCTA3")

  ## calculate proportions
  out <- dplyr::mutate(out, weight = value/total_pop)

  ## subset
  out <- dplyr::select(out, ZCTA3, weight)

  ## return output
  return(out)

}

## Extensive ACS
zi_acs_extensive <- function(.data){

  # global variables
  ZCTA3 = variable = estimate = moe = NULL

  ## square MOEs
  .data <- dplyr::mutate(.data, moe = moe^2)

  ## group by and sum
  .data <- dplyr::group_by(.data, ZCTA3, variable)
  .data <- dplyr::summarise(.data,
                            estimate = sum(estimate, na.rm = TRUE),
                            moe = sum(moe, na.rm = TRUE))

  ## square root of MOE
  .data <- dplyr::mutate(.data, moe = sqrt(moe))

  ## return output
  return(.data)

}

## Intensive ACS
zi_acs_intensive <- function(.data, weights, method){

  # global variables
  ZCTA3 = variable = estimate = weight = moe = NULL

  ## join
  .data <- dplyr::left_join(.data, weights, by = "ZCTA3")

  ## group_by
  .data <- dplyr::group_by(.data, ZCTA3, variable)

  ## summarise (method dependent)
  if (method == "mean"){
    .data <- dplyr::summarise(.data,
                              estimate = stats::weighted.mean(estimate, weight, na.rm = TRUE),
                              moe = stats::weighted.mean(moe, weight, na.rm = TRUE))
  } else if (method == "median"){
    .data <- dplyr::summarise(.data,
                              estimate = spatstat.geom::weighted.median(estimate, weight, na.rm = TRUE),
                              moe = spatstat.geom::weighted.median(moe, weight, na.rm = TRUE))
  }

  ## return output
  return(.data)

}

## Intensive ACS Weights
zi_acs_weights <- function(year, survey, key, call){

  # global variables
  GEOID = NAME = ZCTA3 = total_pop = estimate = weight = NULL

  ## call get_acs
  out <- tidycensus::get_acs(geography = "zcta", variables = "B01003_001",
                             year = year, output = "tidy",
                             survey = survey, key = key, show_call = call)

  ## prep data
  out <- dplyr::mutate(out, GEOID = stringr::word(NAME, 2))
  out <- dplyr::mutate(out, ZCTA3 = substr(GEOID, 1, 3), .before = GEOID)
  out <- dplyr::select(out, -NAME)
  out <- dplyr::arrange(out, ZCTA3)

  ## group by and sum
  totals <- dplyr::group_by(out, ZCTA3)
  totals <- dplyr::summarise(totals, total_pop = sum(estimate, na.rm = TRUE))

  ## join
  out <- dplyr::left_join(out, totals, by = "ZCTA3")

  ## calculate proportions
  out <- dplyr::mutate(out, weight = estimate/total_pop)

  ## subset
  out <- dplyr::select(out, ZCTA3, weight)

  ## return output
  return(out)

}
