#' Formal coverage of package
#' @param pkg Top level of package.
#' @return If no formals uncovered, returns \code{NULL}. Otherwise, a named list: each list element is a function from
#' the package namespace and each value of the list is the unused argument.
#' @details N.B. The exported functions in the \code{NAMESPACE} file
#' must be available in the namespace of the environment in which the function is run. That is,
#' if you make an changes to your exported functions, 
#' you must have build and installed the package to be tested prior to running this function.
#' @export

formalPackageCoverage <- function(pkg = ".") {
  NAMESPACE <- readLines(file.path(pkg, "NAMESPACE"))
  DESCRIPTION <- readLines(file.path(pkg, "DESCRIPTION"))
  stopifnot(grepl("Package: ", DESCRIPTION[1]))
  ns <- gsub("Package: ", "", DESCRIPTION[1])
  funs <- gsub("^export\\((.*)\\)$",
               "\\1", 
               grep("^export", NAMESPACE, value = TRUE),
               perl = TRUE)
  funs <- gsub("'", "", funs, fixed = TRUE)
  funs <- gsub('"', "", funs, fixed = TRUE)
  
  unused_by_files <- lapply(funs, unused_formals, ns = ns)
  
  if (!is.null(unlist(unused_by_files))) {
    names(unused_by_files) <- funs
    are_null <- vapply(unused_by_files, is.null, FALSE)
    unused_by_files[!are_null]
  }
}


