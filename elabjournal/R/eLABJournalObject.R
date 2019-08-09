.eLABJournalObject <- R6Class(
  classname="eLABJournalObject",
  private = list(
    .id = NULL,
    .id_key = NULL,
    .api = NULL,
    .name = NULL,
    .title = NULL,
    .data = NULL
  ),
  public = list(
    initialize = function(api, data, id_key, name, title=NULL) {
      if(inherits(data,"list") && exists(id_key,where=data)) {
        private$.id = data[[id_key]]
        private$.id_key = id_key
        private$.api = api
        private$.name = name
        private$.data = data
        if(is.null(title)) {
          private$.title = paste0(toString(class(self)[1])," '",toString(name),"' (",toString(data[id_key]),")")
        } else {
          private$.title = title
        }
      } else {
        stop("no (valid) data")
      }
    },
    print = function() {
      cat(private$.title)
    },
    name = function() {
      return(private$.name)
    },
    id = function() {
      return(private$.id)
    },
    title = function() {
      return(private$.title)
    },
    data = function() {
      return(private$.data)
    },
    visualize = function() {
      cat("Not implemented")
    },
    show = function() {
      cat("Not implemented")
    }
  )

)
