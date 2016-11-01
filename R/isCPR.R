#Check if v contains only (except for NAs) values that look like Danish
#civil registration numbers
#works until 2036...
#' @export
isCPR <- function(v) {
  out <- list(problem=FALSE, message="")
  m <- "Warning: The variable seems to consist of Danish civil regristration (CPR) numbers."
  v <- as.character(na.omit(v))
  if (length(v) == 0) return(out) #if v consists only of NAs
  posCPR <- FALSE
  chars <- nchar(v)
  
  if (!all(chars %in% c(10,11))) return(out)
  
  if (all(chars == 10)) {
    posCPR <- grepl("[0-9]{10}", v)
  }
  if (all(chars== 11)) {
    posCPR <- grepl("[0-9]{6}-[0-9]{4}", v)
  }
  
  if (!all(posCPR)) return(out)
  
  if (!all(isDanishDate(substring(v, 1, 6)))) return(out)
  
  v <- gsub("-", "", v)
  
  year <- as.numeric(substring(v, 5, 6))
  digit7 <- substring(v, 7, 7)
  
  noCheckPl <- year<36 & year>=7 & digit7 >= 4 #is this right?
  
  if (!all(noCheckPl)) {
    check <- function(x) {
      x <- as.numeric(strsplit(x, "")[[1]])
      a <- c(4, 3, 2, 7, 6, 5, 4, 3, 2, 1)
      (x %*% a) %% 11 == 0 #note: x %*% a = a %*% x for 1 x n vectors in R
    }
    res <- sapply(v[!noCheckPl], check)
    if (!all(res)) return(out)
  } else if (!all(digit7[noCheckPl]>3)) return(out)
  
  out$problem <- TRUE
  out$message <- m
  out
}

##########################################Not exported below#########################################



#Checks whether strs contains only entries on the form DDMMYY
isDanishDate <- function(strs) {
  if (!(all(nchar(strs) == 6) & all(grepl("[0-9]{6}", strs)))) return(FALSE)
  
  ds <- as.numeric(substring(strs, 1, 2))
  ms <- as.numeric(substring(strs, 3, 4))
  
  if (any(ms > 13)) return(FALSE)
  
  mds <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  maxDs <- mds[ms]
  
  if (any(ds > maxDs)) return(FALSE)
  
  TRUE
}
