#' Find out if the whoami package binaries is installed (git + whoami)
#' @return logical that is TRUE if whoami and git can be found
#' @export
whoami_available <- function() {

    ## check if pandoc can be found on the path
    whoami_bin <- as.character(Sys.which('whoami'))
    git_bin <- as.character(Sys.which('git'))

    ## return whatever found
    (whoami_bin != '') & (git_bin != '')

}


