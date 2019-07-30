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
                                    distance <- function(lon1,
                                                         lat1,
                                                         lon2,
                                                         lat2) {
                                      #暫定
                                      return(sqrt((lon2 - lon1) ^ 2 + (lat2 - lat1) ^ 2))
                                    }
                                    
                                    return(distance(self$lon,
                                                    self$lat,
                                                    location$lon,
                                                    location$lat))
                                  }))

# mesh
Mesh <- R6Class("Mesh",
                public = list(location = NULL,
                              user_df = NULL,
                              users = NULL,
                              
                              initialize = function(location = NA,
                                                    facs = NA,
                                                    user_df = NA) {
                                self$location <- location
                                self$user_df <- user_df
                                
                                user_num <- rpois(user_df$facility_id %>% length(),
                                                  lambda = user_df$user_avg)
                                
                                self$users <- list()
                                for (i in user_df$facility_id) {
                                  self$users[[i]] <- list()
                                  
                                  if (user_num[i] != 0) {
                                    for (j in 1:user_num[i]) {
                                      self$users[[i]][[j]] <- User$new(origin = location,
                                                                       destination = facs[[i]])
                                    }
                                  }
                                }
                              }))

# make meshs
make_meshs <- function(mesh_df = NA,
                       fac_df = NA,
                       user_df = NA) {
  facs <- list()
  for (i in fac_df$facility_id) {
    facs[[i]] <- Location$new(lon = lon[i],
                              lat = lat[i])
  }
  
  meshs <- list()
  
  lon <- mesh_df %>%
    .$lon
  lat <- mesh_df %>%
    .$lat
  
  for (i in mesh_df$mesh_id) {
    meshs[[i]] <- Mesh$new(location = Location$new(lon[i],
                                                   lat[i]),
                           facs = facs,
                           user_df = user_df %>%
                             filter(mesh_id == i) %>%
                             select(-mesh_id))
  }
  
  return(meshs)
}

# user
User <- R6Class("User",
                public = list(origin = NULL,
                              destination = NULL,
                              
                              distance = NULL,
                              
                              depart_time_home = NULL,
                              stay_time = NULL,
                              depart_time_fac = NULL,
                              arrival_time = NULL,
                              
                              generalized_cost = NULL, # 暫定的にかかる時間
                              
                              initialize = function(origin = NA, 
                                                    destination = NA) {
                                self$origin <- origin
                                self$destination <- destination
                                
                                self$distance <- origin$distance(destination)
                                
                                self$depart_time_home <- runif(1) # 暫定
                                self$stay_time <- runif(1) # 暫定
                                
                              }))

# car
Car <- 

# 暫定データ
fac_df <- tribble(
  ~facility_id, ~lon, ~lat,
  1, 5, 4,
  2, 3, 6,
  3, 4 , 5,
  4, 5, 6,
  5, 6, 6
)

lon <- fac_df %>%
  .$lon
lat <- fac_df %>%
  .$lat

user_df <- tibble(mesh_id = 1:100,
                   facility_id = list(1:5)) %>%
  unnest() %>%
  mutate(user_avg = 5)

mesh_df <- tibble(lon = 0:9,
                  lat = list(0:9)) %>%
  unnest() %>%
  mutate(mesh_id = 1:100) %>%
  select(mesh_id,
         lon,
         lat)

meshs <- make_meshs(mesh_df,
                    fac_df,
                    user_df)

users <- list()

for (i in 1:length(meshs)) {
  for (j in 1:length(meshs[[i]])) {
    if (length(meshs[[i]]$users[[j]]) != 0) {
      for (k in 1:length(meshs[[i]]$users[[j]])) {
        meshs[[]]
      } 
    }
  }
  users %<>%
    append(users, meshs[[i]]$users)
}

         # mesh = Mesh$new(location = Location$new(lon = lon,
         #                                         lat = lat)))
#   select(mesh_id,
#          lon,
#          lat)


# rpois

