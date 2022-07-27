#' @title Read and Write Dataset Control Lists
#'
#' @description Input/Output functions for dataset control lists.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param datasets \code{character} vector of name(s) of rodent dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param new_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.
#'
#' @return \code{list} of \code{datasets}' control \code{list}s, \code{\link[base]{invisible}}-ly for \code{write_dataset_controls}.
#'  
#' @name read and write rodent dataset controls
#'
#' @export
#'
read_dataset_controls <- function (main     = ".",
                                   settings = directory_settings()) {

  read_yaml(file.path(main, settings$subs$data, settings$files$dataset_controls))

}

#' @rdname read-and-write-rodent-dataset-controls
#'
#' @export
#'
dataset_controls <- function (main     = ".",
                              datasets = prefab_datasets(),
                              settings = directory_settings()) {

  read_dataset_controls(main     = main,
                        settings = settings)[datasets]

}

#' @rdname read-and-write-rodent-dataset-controls
#'
#' @export
#'
write_dataset_controls <- function (main                 = ".",
                                    new_dataset_controls = NULL,
                                    datasets             = prefab_datasets(),
                                    settings             = directory_settings(),
                                    quiet                = FALSE) {

  dataset_controls <- prefab_dataset_controls()
  ndatasets        <- length(dataset_controls)
  nnew_datasets    <- length(new_dataset_controls)

  if (nnew_datasets > 0) {

    for (i in 1:nnew_datasets) {

      dataset_controls <- update_list(dataset_controls, 
                                             x = new_dataset_controls[[i]])

      names(dataset_controls)[ndatasets + i] <- names(new_dataset_controls)[i]

    }

  }

  write_yaml(x    = dataset_controls,
             file = file.path(main, settings$subs$data, settings$files$dataset_controls))

  invisible(dataset_controls)

}

#' @title Provide the Names or Controls for the Prefab Datasets
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) datasets or a \code{list} of their controls.
#'
#' @return \code{prefab_datasets}: \code{character} vector of dataset names. \cr
#'         \code{prefab_dataset_controls}: \code{list} vector of dataset controls. \cr
#'
#' @examples
#'  prefab_datasets()
#'  prefab_dataset_controls()
#'
#' @name prefabricated_datasets
#'
NULL

#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_datasets <- function(){

  names(prefab_dataset_controls())

}


#' @rdname prefabricated_datasets
#'
#' @export
#'
prefab_dataset_controls <- function () {

  prefab_controls_file <- system.file("extdata", "prefab_dataset_controls.yaml", package = "evercast")

  read_yaml(prefab_controls_file)

}


#' @title Prepare Data Tables for Forecasting
#'
#' @description Workhorse function for creating datasets using existing functions. 
#'
#' @param name \code{character} name of the data set.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @param verbose \code{logical} indicator if detailed messages should be shown.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing files should be updated (most users should leave as \code{TRUE}).
#'
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param filename \code{character} value of the file for saving the output.
#'
#' @param species \code{character} vector of species to include.
#'
#' @param regions \code{character} vector of regions to include.
#'
#' @inheritParams wader::load_datafile
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @name prepare dataset
#'
#' @export
#'

prepare_dataset <- function (name        = "all_total",
                             main        = ".",
                             settings    = directory_settings(),
                             datafile    = "Indicators/max_count_all.csv",
                             filename    = "counts_all_total.csv",
                             na.strings  = "",
                             regions     = "all",
                             species     = "total",
                             save        = TRUE,
                             overwrite   = TRUE,
                             quiet       = FALSE,
                             verbose     = FALSE) {


  return_if_null(name)

  messageq("    - ", name, quiet = quiet)


  dataset <- load_datafile(datafile            = datafile,
                           na.strings          = na.strings,
                           path                = file.path(main, settings$subs$resources),
                           download_if_missing = FALSE,
                           quiet               = quiet)

  out <- dataset[dataset$region %in% regions & dataset$species %in% species, ]

  write_data(x         = out, 
             main      = main, 
             data_sub  = settings$subs$data,
             save      = save, 
             filename  = filename, 
             overwrite = overwrite, 
             quiet     = !verbose)

  out

}









#' @title Prepare Data for the Evercast Repository
#'
#' @description Create specified \code{datasets} using their associated function and arguments.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param datasets \code{character} vector of name(s) of dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{list} of prepared \code{datasets}.
#'  
#' @name prepare rodents
#'
#' @export
#'
prepare_data  <- function (main     = ".",
                           datasets = prefab_datasets(),
                           settings = directory_settings(), 
                           quiet    = FALSE,
                           verbose  = FALSE) {

  return_if_null(datasets)

  dataset_controls_list <- dataset_controls(main     = main, 
                                            settings = settings, 
                                            datasets = datasets)

  messageq("  - rodents", quiet = quiet)

  out <- named_null_list(element_names = datasets)

  for (i in 1:length(dataset_controls_list)) {

    out[[i]] <- do.call(what = dataset_controls_list[[i]]$fun, 
                        args = update_list(list      = dataset_controls_list[[i]]$args, 
                                           main      = main, 
                                           settings  = settings, 
                                           quiet     = quiet, 
                                           verbose   = verbose))
  
  }

  invisible(out)

}

