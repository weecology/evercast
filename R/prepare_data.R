#' @title Read and Write Dataset Count Control Lists
#'
#' @description Input/Output functions for count dataset control lists.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param count_datasets \code{character} vector of name(s) of rodent dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param new_dataset_controls \code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.
#'
#' @return \code{list} of \code{datasets}' control \code{list}s, \code{\link[base]{invisible}}-ly for \code{write_dataset_controls}.
#'  
#' @name read and write count dataset controls
#'
#' @export
#'
read_count_dataset_controls <- function (main     = ".",
                                         settings = directory_settings()) {

  read_yaml(file.path(main, settings$subs$data, settings$files$count_dataset_controls), eval.expr = TRUE)

}

#' @rdname read-and-write-count-dataset-controls
#'
#' @export
#'
count_dataset_controls <- function (main           = ".",
                                    count_datasets = prefab_count_datasets(),
                                    settings       = directory_settings()) {

  read_count_dataset_controls(main     = main,
                              settings = settings)[count_datasets]

}

#' @rdname read-and-write-count-dataset-controls
#'
#' @export
#'
write_count_dataset_controls <- function (main                       = ".",
                                          new_count_dataset_controls = NULL,
                                          count_datasets             = prefab_count_datasets(),
                                          settings                   = directory_settings(),
                                          quiet                      = FALSE) {

  count_dataset_controls <- prefab_count_dataset_controls()
  ncount_datasets        <- length(count_dataset_controls)
  nnew_count_datasets    <- length(new_count_dataset_controls)

  if (nnew_count_datasets > 0) {

    for (i in 1:nnew_count_datasets) {

      count_dataset_controls <- update_list(count_dataset_controls, 
                                            x = new_count_dataset_controls[[i]])

      names(count_dataset_controls)[ncount_datasets + i] <- names(new_count_dataset_controls)[i]

    }

  }

  write_yaml(x    = count_dataset_controls,
             file = file.path(main, settings$subs$data, settings$files$count_dataset_controls))

  invisible(count_dataset_controls)

}

#' @title Provide the Names or Controls for the Prefab Count Datasets
#'
#' @description Create a \code{character} vector of the names of the pre-fabricated (prefab) count datasets or a \code{list} of their controls.
#'
#' @return \code{prefab_count_datasets}: \code{character} vector of count dataset names. \cr
#'         \code{prefab_count_dataset_controls}: \code{list} vector of count dataset controls. \cr
#'
#' @examples
#'  prefab_count_datasets()
#'  prefab_count_dataset_controls()
#'
#' @name prefabricated_count_datasets
#'
NULL

#' @rdname prefabricated_count_datasets
#'
#' @export
#'
prefab_count_datasets <- function( ) {

  names(prefab_count_dataset_controls())

}


#' @rdname prefabricated_count_datasets
#'
#' @export
#'
prefab_count_dataset_controls <- function ( ) {

  prefab_controls_file <- system.file("extdata", "prefab_count_dataset_controls.yaml", package = "evercast")

  read_yaml(prefab_controls_file, eval.expr = TRUE)

}


#' @title Prepare Max Counts Data Tables for Forecasting
#'
#' @description Generate max count annual datasets using existing functions. 
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
#' @inheritParams wader::max_counts
#'
#' @return \code{data.frame} for the specified data set.
#'
#' @name prepare dataset
#'
#' @export
#'
prepare_max_counts <- function (name        = "all_total",
                                main        = ".",
                                settings    = directory_settings(),
                                species     = "total",
                                datafile    = "Indicators/max_count_all_total.csv",
                                filename    = "counts_all_total.csv",
                                minyear     = 1986,
                                maxyear     = this_year(),
                                level       = "all",
                                save        = TRUE,
                                overwrite   = TRUE,
                                quiet       = FALSE,
                                verbose     = FALSE) {


  return_if_null(name)

  messageq("    - ", name, quiet = quiet)


  out <- max_counts(minyear = minyear,
                    maxyear = maxyear,
                    level   = level,
                    path    = file.path(main, settings$subs$resources))

  out <- out[out$species %in% species, ]

  write_data(x         = out, 
             main      = main, 
             data_sub  = settings$subs$data,
             save      = save, 
             filename  = filename, 
             overwrite = overwrite, 
             quiet     = !verbose)

  out

}









#' @title Prepare Counts Data for the Evercast Repository
#'
#' @description Create specified \code{datasets} using their associated function and arguments.
#'
#' @param quiet \code{logical} indicator controlling if messages are printed.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information or not (and thus just the tidy messages). 
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param count_datasets \code{character} vector of name(s) of colony count dataset(s) to include.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return \code{list} of prepared \code{datasets}.
#'  
#' @name prepare rodents
#'
#' @export
#'
prepare_counts  <- function (main           = ".",
                             count_datasets = prefab_count_datasets(),
                             settings       = directory_settings(), 
                             quiet          = FALSE,
                             verbose        = FALSE) {

  return_if_null(datasets)

  count_dataset_controls_list <- count_dataset_controls(main           = main, 
                                                        settings       = settings, 
                                                        count_datasets = count_datasets)

  messageq("  - counts", quiet = quiet)

  out <- named_null_list(element_names = count_datasets)

  for (i in 1:length(dataset_controls_list)) {

    out[[i]] <- do.call(what = count_dataset_controls_list[[i]]$fun, 
                        args = update_list(list      = count_dataset_controls_list[[i]]$args, 
                                           main      = main, 
                                           settings  = settings, 
                                           quiet     = quiet, 
                                           verbose   = verbose))
  
  }

  invisible(out)

}

