library(tidyverse)
library(magrittr)
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
                                    
                                    f <- function(lon1,
                                                  lat1,
                                                  lon2,
                                                  lat2) {
                                      #暫定
                                      return(sqrt((lon2 - lon1) ^ 2 + (lat2 - lat1) ^ 2))
                                    }
                                    
                                    return(f(self$lon,
                                             self$lat,
                                             location$lon,
                                             location$lat))
                                  }))

# mesh
# Mesh <- R6Class("Mesh",
#                 inherit = Location,
#                 public = list(lon = NULL,
#                               lat = NULL,
#                               
#                               initialize = function(lon = NA,
#                                                     lat = NA,
#                                                     ...) { # 暫定
#                                 super$initialize(lon = lon,
#                                                  lat = lat)
#                               },
#                               
#                               distance = function(location) {
#                                 return(super$distance(location))
#                               }))

# user
User <- R6Class("User",
                private = list(time_value = NULL,
                               
                               stay_time = NULL,
                               
                               passed_time = NULL),
                
                public = list(mesh = NULL,
                              facility = NULL,
                              distance = NULL,
                              
                              back = NULL,
                              
                              depart_time = NULL,
                              
                              initialize = function(time_value = NA,
                                                    mesh = NA, 
                                                    facility = NA) {
                                private$time_value <- time_value
                                
                                self$mesh <- mesh
                                self$facility <- facility
                                self$distance <- origin$distance(destination)
                                
                                self$back <- FALSE
                                
                                self$depart_time <- runif(1) # 暫定
                                private$stay_time <- runif(1) # 暫定
                                
                                private$passed_time <- 0
                                
                              },
                              
                              pass = function(time) {
                                private$passed_time <- private$passed_time + time
                              },
                              
                              get_generalized_cost = function(fare = NA) {
                                return(private$time_value * private$passed_time + fare)
                              }))

# self$location <- location
# self$user_df <- user_df
# 
# user_num <- rpois(user_df$facility_id %>% length(),
#                   lambda = user_df$user_avg)
# 
# self$users <- list()
# for (i in user_df$facility_id) {
#   self$users[[i]] <- list()
# 
#   if (user_num[i] != 0) {
#     for (j in 1:user_num[i]) {
#       self$users[[i]][[j]] <- User$new(origin = location,
#                                        destination = facs[[i]])
#     }
#   }
# }

# make meshs
# make_meshs <- function(mesh_df = NA,
#                        fac_df = NA,
#                        user_df = NA) {
#   facs <- list()
#   for (i in fac_df$facility_id) {
#     facs[[i]] <- Location$new(lon = lon[i],
#                               lat = lat[i])
#   }
#   
#   meshs <- list()
#   
#   lon <- mesh_df %>%
#     .$lon
#   lat <- mesh_df %>%
#     .$lat
#   
#   for (i in mesh_df$mesh_id) {
#     meshs[[i]] <- Mesh$new(location = Location$new(lon[i],
#                                                    lat[i]),
#                            facs = facs,
#                            user_df = user_df %>%
#                              filter(mesh_id == i) %>%
#                              select(-mesh_id))
#   }
#   
#   return(meshs)
# }


# car
# Car <- 

# 暫定データ
# facility
fac_df <- tribble(
  ~facility_id, ~lon, ~lat,
  1, 5, 4,
  2, 3, 6,
  3, 4, 5,
  4, 5, 6,
  5, 6, 6
)

lon <- fac_df %>%
  .$lon
lat <- fac_df %>%
  .$lat

facs <- list()

for (i in fac_df$facility_id) {
  facs[[i]] <- Location$new(lon[i], lat[i])
}



# mesh
mesh_df <- tibble(lon = 0:9,
                  lat = list(0:9)) %>%
  unnest() %>%
  mutate(mesh_id = 1:100) %>%
  select(mesh_id,
         lon,
         lat)

lon <- mesh_df %>%
  .$lon
lat <- mesh_df %>%
  .$lat

meshs <- list()

for (i in mesh_df$mesh_id) {
  meshs[[i]] <- Location$new(lon[i], lat[i])
}



# user
user_df <- tibble(mesh_id = 1:100,
                   facility_id = list(1:5)) %>%
  unnest() %>%
  mutate(user_avg = 5)

make_users <- function(user_df = NA,
                       meshs = NA,
                       facs = NA) {
  
}

