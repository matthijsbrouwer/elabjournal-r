library(keyring)

#' The elabjournal class
#'
#' R6 class to access the eLABJournal \code{API}.
api <- function(key=NULL) {
  return(.api$new(key))
}

reset_key <- function() {
  if(keyring::has_keyring_support()) {
    keyring::key_delete("elabjournal-r", "apikey")
  }
}

