#' Produce a data codebook
#'
#' Make a data codebook that summarizes the contents of a dataset.
#' The result is saved to an R markdown file which can
#' rendered into an easy-to-read codebook in pdf, html or word formats. 
#'
#' @param data The dataset to be checked. This dataset should be of class \code{data.frame},
#' \code{tibble} or \code{matrix}. If it is of classs \code{matrix}, it will be converted to a
#' \code{data.frame}.
#' 
#' @param vol Extra text string or numeric that is appended on the end of the output
#' file name(s). For example, if the dataset is called "myData", no file argument is
#'  supplied and \code{vol=2}, the output file will be called "dataMaid_myData2.Rmd"
#' 
#' @param reportTitle A text string. If supplied, this will be the printed title of the
#' report. If left unspecified, the title with the name of the supplied dataset.
#' 
#' @export
makeCodebook <- function(data, vol="", reportTitle=NULL, ...) {
  
  dfname <- deparse(substitute(data))
  
  makeDataReport(data,
                 reportTitle=paste0("Codebook for ", dfname),
                 file=normalizeFileName(paste0("codebook_", dfname, vol, ".Rmd")),
                 ...)
}
