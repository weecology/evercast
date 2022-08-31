#' @title Functions for Forecasting Everglades Wading Birds
#'
#' @description This package contains the functions used for forecasting everglades wading birds.
#'
#' @name evercast
#'
#' @docType package
#'
#' @keywords package
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom scoringRules crps_nbinom crps_pois crps_sample logs_nbinom logs_pois logs_sample
#' @importFrom tscount tsglm 
#' @importFrom utils packageDescription read.csv sessionInfo write.csv
#' @importFrom wader download_observations load_datafile max_counts
#' @importFrom yaml read_yaml write_yaml
#'
NULL