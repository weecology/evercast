#' @title Fill an Everglades Forecasting Directory with Basic Components
#'
#' @description Fill the directory with components.
#'             
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param models \code{character} vector of name(s) of model(s) to include.
#'
#' @param datasets \code{character} vector of name(s) of wading bird dataset(s) to be created. 
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @return \code{NULL}, \code{\link[base]{invisible}}-ly.
#'
#' @name directory filling
#'
#'
#' @export
#'
fill_dir <- function (main     = ".",
                      models   = prefab_models(), 
                      datasets = prefab_datasets(),
                      settings = directory_settings(), 
                      quiet    = FALSE, 
                      verbose  = FALSE) {

  messageq("Filling directory with content: \n", quiet = quiet)

  fill_resources(main     = main, 
                 settings = settings, 
                 quiet    = quiet, 
                 verbose  = verbose)

  fill_data(main     = main, 
            datasets = datasets,
            models   = models,
            settings = settings, 
            quiet    = quiet, 
            verbose  = verbose)
            

  messageq("\nDirectory filling complete.", quiet = quiet)

  invisible()

}



#' @rdname directory-filling
#'
#' @export
#'
fill_data <- function (main     = ".",
                       models   = prefab_models(),
                       datasets = prefab_datasets(),
                       settings = directory_settings(), 
                       quiet    = FALSE,
                       verbose  = FALSE) {

  messageq(" Writing data files ... ", quiet = quiet)

  write_dataset_controls(main     = main, 
                         settings = settings, 
                         datasets = datasets, 
                         quiet    = FALSE)



  messageq("  ... data preparing complete.", quiet = quiet)

  invisible()

}

#' @rdname directory-filling
#'
#' @export
#'
fill_resources <- function (main     = ".",
                            settings = directory_settings(),
                            quiet    = FALSE,
                            verbose  = FALSE) {

  messageq("Downloading resources ... ", quiet = quiet)

  download_observations(path        = file.path(main, settings$subs$resources), 
                        version     = settings$resources$EvergladesWadingBird$version,
                        from_zenodo = settings$resources$EvergladesWadingBird$source == "zenodo",
                        quiet       = quiet,
                        verbose     = verbose)

  messageq("  ... downloads complete. ", quiet = quiet)
  


  invisible()

}
