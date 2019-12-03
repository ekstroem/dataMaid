
#' Find path to the pandoc binary by checking the \code{PATH} and the \code{RSTUDIO_PANDOC} env vars
#' @return logical that is TRUE is pandoc can be found
#' @export
pandoc_exists <- function() {

    ## check if pandoc can be found on the path
    bin <- as.character(Sys.which('pandoc'))

    ## check RStudio env var pointing to the bundled pandoc
    if (Sys.getenv('RSTUDIO_PANDOC') != '') {
        bin <- Sys.getenv('RSTUDIO_PANDOC')
        bin <- file.path(bin, ifelse(
            grepl('w|W', .Platform$OS.type),
            ## we are on Windows
            'pandoc.exe',
            ## no extension on Mac and Linux
            'pandoc'))
    }

    ## return whatever found
    bin != ''

}


#' Find path to the pandoc binary by checking the \code{PATH} and the \code{RSTUDIO_PANDOC} env vars
#' @return logical that is TRUE is pandoc can be found
#' @export
whoami_exists <- function() {

    ## check if pandoc can be found on the path
    bin <- as.character(Sys.which('whoami'))

    ## return whatever found
    bin != ''

}


