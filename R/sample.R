library(R6)

# location
Location <- R6Class("Location",
                    public = list(lon = NULL,
                                  lat = NULL,
                                  initialize = function(lon = NA,
                                                        lat = NA) {
                                    self$lon <- lon
                                    self$lat <- lat
                                  },
                                  distance = function(location) {
                                    return(distance(self$lon,
                                                    self$lat,
                                                    location$lon,
                                                    location$lat))
                                  }))

distance <- function(lon1,
                     lat1,
                     lon2,
                     lat2) {
  #暫定
  return(sqrt((lon2 - lon1) ^ 2 + (lat2 - lat1) ^ 2))
}

# mesh
Mesh <- R6Class("Mesh",
                public = list(location = NULL,
                              
                              initialize = function(location = NA) {
                                self$location <- location
                              }))

# user
User <- R6Class("User",
                public = list(sex = NULL,
                              age = NULL,
                              initialize = function(sex = NA, age = NA) {
                                self$sex = sex
                                self$age = age
                              }))

# rpois
