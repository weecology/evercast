#' @title Create the Structure of a Forecasting Directory
#'
#' @description Instantiates the necessary folder structure for a forecasting directory and writes a YAML file that tracks the setup configurations (see \code{\link{write_directory_config}}). 
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) puts the forecasting directory in the present locations. Nesting the forecasting directory in a folder can be done by simply adding to the \code{main} input (see \code{Examples}).
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
#'
#' @name directory creation
#'
#' @export
#'
create_dir <- function(main     = ".", 
                       settings = directory_settings(), 
                       quiet    = FALSE){

  messageq(message_break(), "\nEstablishing forecasting directory at\n ", normalizePath(file.path(main = main), mustWork = FALSE), "\n", message_break(), quiet = quiet)

  mapply(FUN          = dir.create, 
         path         = file.path(main, settings$subs),
         recursive    = TRUE,
         showWarnings = FALSE)


  write_directory_config(main     = main, 
                         settings = settings, 
                         quiet    = quiet)


}


#' @title Create and Fill a Forecasting Directory
#'
#' @description Combines \code{\link{create_dir}} and \code{\link{fill_dir}} to create a ready-to-run (via \code{\link{evercast}}) directory where indicated.
#'
#' @param main \code{character} value of the name of the main component of the directory tree.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}}.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param verbose \code{logical} indicator of whether or not to print out all of the information (and thus just the tidy messages).
#'
#' @return The \code{list} of directory settings \code{\link[base]{invisible}}-ly.
#'
#' @name directory setup
#'
#'
#' @export
#'
setup_dir <- function (main     = ".",
                       settings = directory_settings(), 
                       quiet    = FALSE, 
                       verbose  = FALSE) {

  messageq(message_break(), "\nThis is evercast v", packageDescription("evercast", fields = "Version"), "\n", message_break(), quiet = quiet)

  create_dir(main     = main, 
             settings = settings,
             quiet    = quiet)

  fill_dir(main     = main,
           settings = settings,
           quiet    = quiet,
           verbose  = verbose)

  messageq(message_break(), "\nDirectory successfully instantiated\n", message_break(), quiet = quiet)

  read_directory_config(main     = main,
                        settings = settings,
                        quiet    = quiet)

}



#' @title Create a Directory Settings List
#'
#' @description Most users will not want or need to change the directory folders and file names, but it is helpful to have them be flexible for certain circumstances, and this function gathers them into a list for pipeline functionality.
#'
#' @param files named \code{character} list of the file names (with extensions). Default includes \code{directory_config}, \code{count_dataset_controls}.
#'
#' @param subdirectories named \code{character} list of the subdirectory names. Default includes \code{resources}, \code{data}, \code{models}, \code{fits}, and \code{forecasts}. 
#'
#' @param EvergladesWadingBird \code{list} with \code{source} and \code{version} elements that are \code{character} values for the source and version of the Everglades Wading Bird to download. Default values retrieve the latest data from github. \cr \cr
#'                   See \code{\link[wader]{download_observations}}.
#'
#' @param save \code{logical} indicator controlling if the output should be saved out.
#'
#' @param overwrite \code{logical} indicator of whether or not the existing files should be updated (most users should leave as \code{TRUE}).
#'
#' @return Named \code{list} of settings for the directory.
#'
#' @export
#'
directory_settings <- function (files                 = list(directory_config       = "dir_config.yaml", 
                                                             count_dataset_controls = "count_dataset_controls.yaml"), 
                                subdirectories        = list(forecasts = "forecasts", 
                                                             fits      = "fits", 
                                                             models    = "models", 
                                                             resources = "resources", 
                                                             data      = "data"),
                                EvergladesWadingBird  = list(source  = "github", 
                                                             version = "latest"),
                                save                  = TRUE,
                                overwrite             = TRUE) {

  list(files     = files,
       subs      = subdirectories,
       resources = list(EvergladesWadingBird = EvergladesWadingBird),
       save      = save, 
       overwrite = overwrite)

}





#' @title Create, Read the Directory Configuration File
#' 
#' @description The directory configuration file is a special file within the forecasting directory setup and has its own set of functions. \cr \cr
#'              \code{write_directory_config} creates the YAML metadata configuration file. It is (and should only be) called from within \code{\link{setup_dir}}, as it captures information about the compute environment used to instantiate the directory.
#'
#' @param quiet \code{logical} indicator if progress messages should be quieted.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. Default value (\code{"."}) puts the forecasting directory in the present locations. Nesting the forecasting directory in a folder can be done by simply adding to the \code{main} input.
#'
#' @param settings \code{list} of controls for the directory, with defaults set in \code{\link{directory_settings}} that should generally not need to be altered.
#'
#' @return \code{list} of directory configurations, \code{\link[base]{invisible}}-ly.
#'
#' @name directory_configuration_file
#'
NULL

#' @rdname directory_configuration_file
#'
#' @export
#'
write_directory_config <- function (main     = ".", 
                                    settings = directory_settings(), 
                                    quiet    = FALSE){

  config <- list(setup = list(date             = as.character(Sys.Date()),
                              R_version        = sessionInfo()$R.version,
                              evercast_version = packageDescription("evercast", fields = "Version")),
                 tree  = list(main             = main, 
                              subs             = settings$subs))

  write_yaml(x    = config, 
             file = file.path(main, settings$files$directory_config))

  invisible(config)

}


#' @rdname directory_configuration_file
#'
#' @export
#'
read_directory_config <- function (main     = ".", 
                                   settings = directory_settings(), 
                                   quiet    = FALSE){

  invisible(read_yaml(file.path(main, settings$files$directory_config)))

}


