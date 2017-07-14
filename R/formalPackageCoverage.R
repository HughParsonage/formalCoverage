#' Formal coverage of package
#' @param pkg Top level of package.
#' @return If no formals uncovered, returns \code{NULL}.
#' @export

formalPackageCoverage <- function(pkg = ".") {
  unused_by_files <-
    lapply(list.files(path = file.path(pkg, "R"),
                      pattern = "\\.R$",
                      full.names = TRUE),
           unused_formals)
  
  if (!is.null(unlist(unused_by_files))) {
    unused_by_files
  }
}
