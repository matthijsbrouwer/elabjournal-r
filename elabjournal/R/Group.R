.Group <- R6Class(
  classname = "Group",
  inherit = .eLABJournalObject,
  public = list(
    initialize = function(api, data) {
      if(inherits(data,"list") && exists("name",where=data)) {
        super$initialize(api, data, "groupID", toString(data["name"]))
      } else {
        stop("no (valid) Group data")
      }
    }
  )
)
