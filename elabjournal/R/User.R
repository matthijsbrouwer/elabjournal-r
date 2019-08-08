.User <- R6Class(
  classname = "User",
  inherit = .eLABJournalObject,
  public = list(
    initialize = function(api, data) {
      if(inherits(data,"list") && exists("firstName",where=data) && exists("lastName",where=data)) {
        name = paste0(toString(data["firstName"])," ",toString(data["lastName"]))
        super$initialize(api, data, "userID", name)
      } else {
        stop("no (valid) User data")
      }
    }
  )
)
