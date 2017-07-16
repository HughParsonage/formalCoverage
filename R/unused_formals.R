#' Is every function argument used in its body?
#' @param fn A function.
#' @param ns If \code{fn} is a character vector, the namespace to which \code{fn} belongs.
#' @return The arguments of \code{fn} that are not used or if all are indeed used \code{NULL} invisibly.
#' 
#' @examples 
#' add3 <- function(x, y, z = 0) {
#'   x + y
#' }
#' 
#' unused_formals(add3)
#' @importFrom utils getFromNamespace
#' @export

unused_formals <- function(fn, ns = NULL) {
  if (is.character(fn)) {
    if (is.null(ns)) {
      stop("ns must be used if fn is a character vector.")
    } else {
      fn <- getFromNamespace(fn, ns = ns)
    }
  }
  
  formalz <- formals(fn)
  bod <- as.character(body(fn))
  
  formal_present <- function(formal) {
    formal_dots_escaped <- gsub(".", "\\.", formal, fixed = TRUE)
    
    any(grepl(pattern = paste0("(?:\\b|[\\s\\(])", formal_dots_escaped, "(?:\\b|[\\s\\)])"),
              x = bod,
              perl = TRUE))
  }
  formals_present <- vapply(names(formalz), formal_present, FALSE)
  
  if (!all(formals_present) && !any(grepl("UseMethod", bod, fixed = TRUE))) {
    names(formals_present)[!formals_present]
  }
}
