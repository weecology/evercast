#' @title Provide the Names or Controls for the Prefab Models
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) models or a \code{list} of their controls.
#'
#' @return \code{prefab_models}: \code{character} vector of model names. \cr
#'         \code{prefab_model_controls}: \code{list} of model controls. \cr
#'
#' @examples
#'  prefab_models()
#'  prefab_model_controls()
#'
#' @name prefabricated_model_datasets
#'
NULL

#' @rdname prefabricated_model_datasets
#'
#' @export
#'
prefab_models <- function( ) {

  names(prefab_model_controls())

}


#' @rdname prefabricated_model_datasets
#'
#' @export
#'
prefab_model_controls <- function ( ) {

  prefab_model_file <- system.file("extdata", "prefab_model_controls.yaml", package = "evercast")

  read_yaml(prefab_model_file, eval.expr = TRUE)

}
