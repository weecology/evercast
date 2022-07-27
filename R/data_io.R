
#' @title Save Data Out to a File and Return It	Invisibly
#'
#' @description Save inputted data out to a data file if requested and return it to the console, \code{\link[base]{invisible}}-ly..
#'
#' @param x \code{data.frame} or YAML \code{list} to be written out.
#'
#' @param main \code{character} value of the name of the main component of the directory tree. 
#'
#' @param data_sub \code{character} value defining the data subdirectory of the portalcasting directory tree. 
#'
#' @param save \code{logical} indicator controlling if \code{x} should be saved out.
#'
#' @param filename \code{character} name of the file for saving \code{x}.
#'
#' @param overwrite \code{logical} indicator of if the file should be overwritten if it exists.
#'
#' @param quiet \code{logical} indicator if messages should be quieted.
#'
#' @return \code{x} as input, \code{\link[base]{invisible}}-ly.
#'
#' @export
#'
write_data <- function (x         = NULL, 
                        main      = ".", 
                        data_sub  = "data",
                        save      = TRUE, 
                        filename  = NULL, 
                        overwrite = TRUE, 
                        quiet     = FALSE) {
  
  return_if_null(x)

  return_if_null(filename)

  save_it <- FALSE

  if (save) {

    full_path <- file.path(main, data_sub, filename)

    if (file.exists(full_path)) {

      if (overwrite) {

        save_it <- TRUE

        messageq("    **", filename, " exists and overwrite = TRUE; file saved**", quiet = quiet)

      } else {

        messageq("    **", filename, " exists and overwrite = FALSE; not saved***", quiet = quiet) 
      }

    } else {

      save_it <- TRUE

      messageq("    **", filename, " saved**", quiet = quiet)

    }

    if (save_it) {

      if (file_ext(filename) == "csv") {

        write.csv(x, full_path, row.names = FALSE)

      } else if (file_ext(filename) == "yaml"){

        write_yaml(x, file = full_path)

      } else {

        stop("file type not supported")

      }

    }
   
  }

  invisible(x)

}