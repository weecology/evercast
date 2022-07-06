
#' @title Optionally generate a message based on a logical input
#'
#' @description A wrapper on \code{\link[base]{message}} that, given the input to \code{quiet}, generates the message(s) in \code{...} or not.
#'
#' @param ... zero or more objects that can be coerced to \code{character} and are concatenated with no separator added, or a single condition object. See \code{\link[base]{message}}.
#'
#' @param quiet \code{logical} indicator if the message should be generated. 
#'
#' @param domain The domain for the translation. If \code{NA}, messages will not be translated. See \code{\link[base]{message}} and \code{\link[base]{gettext}}.
#'
#' @param appendLF \code{logical} indicator if messages given as a \code{character} string should have a newline appended. See \code{\link[base]{message}}.
#'
#' @return A message is given, and \code{NULL} returned.
#'
#' @export
#'
messageq <- function (..., 
                      quiet    = FALSE, 
                      domain   = NULL, 
                      appendLF = TRUE) {

  if (!quiet) {

    message(...,
            domain   = domain,
            appendLF = appendLF)

  }

  invisible()

}



#' @title Produce a Horizontal Break Line for Messaging
#'
#' @description Creates a horizontal line of characters for messages.
#'
#' @param char \code{character} value to repeated \code{reps} times to form the break. 
#'
#' @param reps \code{integer}-conformable value for number of times \code{char} is replicated.
#' 
#' @return \code{NULL} (message is put out to console).
#'
#' @examples
#'  message_break()
#'
#' @export
#'
message_break <- function(char = "-",
                          reps = 60){
  
  paste(rep(char, reps), collapse = "") 

}