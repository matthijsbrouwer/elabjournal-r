library(httr)
library(rjson)
library(DiagrammeR)

.api <- R6Class(
  classname="api",
  private = list(
    .key = NULL,
    .url = "https://www.elabjournal.com",
    .user = NULL,
    .version = packageVersion("elabjournal"),
    check_and_set_key = function (key, throw_error, store_valid_key) {
      if(is.null(key)) {
        if(throw_error) {
          cat("Problem creating eLABJournal object: no key provided\n")
        } else {
          return(FALSE)
        }
      } else if (typeof(key)=="character" & (nchar(key)>0)) {
        rp <- private$request("/api/v1/auth/user","get",NULL,key)
        if(!is.null(rp) && !is.null(rp$user)) {
          private$.key = key
          private$.user = .User$new(self, rp$user)
          welcomeText <- paste0("Welcome ",private$.user$name(),"\nPackage 'elabjournal', version '",private$.version,"'")
          group = self$group()
          if(!is.null(group)) {
            welcomeText <- paste0(welcomeText,"\nYour active group is '", toString(group$name()), "' (", toString(group$id()),")")
          }
          cat(welcomeText)
          if(store_valid_key && keyring::has_keyring_support()) {
            keyring::key_set_with_value("elabjournal-r", username="apikey", password=key)
          }
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        if(throw_error) {
          cat("Problem creating eLABJournal object: no valid key provided\n")
        } else {
          return(FALSE)
        }
      }
    },


    request = function(location, method, request=NULL, key=NULL, show_messages=TRUE, stream=FALSE, headers=c()) {
      #define location
      if (!startsWith(location,"http")) {
        request_location <- paste0(private$.url,location)
      } else {
        request_location <- location
      }
      #define headers
      if(!is.null(key)) {
        headers["Authorization"] <- key
      } else {
        headers["Authorization"] <- private$.key
      }
      headers["Accept"] <- "application/json"
      # GET-method
      if(method=="get") {
        data = tryCatch({
          if (is.null(request) || inherits(request,"list")) {
            httr::GET(url=request_location, httr::add_headers(headers), query=request)
          } else {
            stop("unsupported type of request")
          }
        }, warning = function(w) {
          stop(paste0("Warning: ",w$message))
        }, error = function(e) {
          stop(paste0("Error: ",e$message))
        }, finally = {
        })
      } else if(method=="put") {
        data = tryCatch({
          if (is.character(request)) {
            httr::PUT(url=request_location, httr::add_headers(headers), body=request)
          } else {
            stop("unsupported type of request")
          }
        }, warning = function(w) {
          stop(paste0("Warning: ",w$message))
        }, error = function(e) {
          stop(paste0("Error: ",e$message))
        }, finally = {
        })
      } else {
        stop("unsupported method")
      }
      if(!is.null(data)) {
        if(httr::status_code(data)==200) {
          response = tryCatch({
            rjson::fromJSON(httr::content(data,type="text", encoding="UTF-8"))
          }, warning = function(w) {
            cat(paste0("Warning: ",w$message))
          }, error = function(e) {
            cat(paste0("Error: ",e$message))
          }, finally = {
          })
          return(response)
        } else if(httr::status_code(data)==204) {
          return(NULL)
        } else {
          if(show_messages) {
            cat(paste0("Http Error: ",(httr::http_status(data))$message))
          }
          return(NULL)
        }
      } else {
        return(NULL)
      }
    }
  ),
  public = list(
    initialize = function(key) {
      if(is.null(key)) {
        #try to get from keyring
        if(keyring::has_keyring_support()) {
          key = tryCatch({
            keyring::key_get("elabjournal-r",username="apikey")
          }, warning = function(w) {
            cat(w$message)
          }, error = function(e) {
            cat(e$message)
          }, finally = {
          })
        }
        if(!private$check_and_set_key(key, FALSE, FALSE)) {
          if(rstudioapi::isAvailable()) {
            key = rstudioapi::showPrompt(title = "eLABJournal API key", message = "Enter eLABJournal API key", default = "")
          } else {
            key = readline(prompt="eLABJournal API key: ")
          }
          key = private$check_and_set_key(key, TRUE, TRUE)
        }
      } else {
        private$check_and_set_key(key, TRUE, FALSE)
      }
    },
    print = function() {
      description <- paste0("eLABJournal API object - version ",private$.version)
      if(is.null(private$.key)) {
        description <- paste0(description," - authentication failed")
      } else {
        description <- paste0(description," - authenticated as ",private$.user$name())
      }
      cat(description)
    },
    group = function() {
      rp <- private$request("/api/v1/groups/active","get")
      return(.Group$new(self,rp))
    },
    set_group = function(id) {
      if(is.character(id) || is.numeric(id)) {
        rp <- private$request("/api/v1/groups/active","put", toString(id), headers=c("Content-Type"="application/json"))
      } else if(inherits(id,"Group")) {
        rp <- private$request("/api/v1/groups/active","put", toString(id$id()), headers=c("Content-Type"="application/json"))
      } else {
        stop("incorrect call")
      }
    },
    user = function() {
      return(private$.user)
    },
    projects = function() {
      request <- c()
      return(.Projects$new(self, "Projects", "/api/v1/projects", request, "projectID", 5, self$project))
    }
  ),
  active = list(

  )
)
