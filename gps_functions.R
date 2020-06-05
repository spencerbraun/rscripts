## Geospatial functions for use with StudentLife GPS dataset
## Spencer Braun
## 20200208


library(dplyr)
library(tidyr)
library(studentlife)
library(geodist)
library(geosphere)


#gps <- readRDS("yourfilepath")



filterGPS <- function(gps, t1, t2) {
  #' Filters StudentLife GPS tibble to specified timeframe between t1 and t2
  #' inclusive. If NULL arguments passed for t1 or t2, defaults to taking the
  #' min / max timestamp from gps tibble passed.
  #'
  #' Args:
  #'     gps(tibble): StudentLife GPS data
  #'     t1(int): Minimum timestamp desired in output
  #'     t2(int): Maximum timestamp desired in output
  #'
  #' Returns:
  #'    tibble of filtered GPS data

  if (is.null(t1)) {
    t1 <- select(gps, timestamp) %>% min
  }
  if (is.null(t2)) {
    t2 <- select(gps, timestamp) %>% max
  }

  gps %>%
    dplyr::filter(timestamp >= t1) %>%
    dplyr::filter(timestamp <= t2)

}

totalDistanceCovered <- function(gps, t1=NULL, t2=NULL) {
  #' Calculates the total distance in meters covered per uid for specified time period
  #' between t1 and t2 inclusive. If no time specified, returns for entire gps
  #' tibble. For more information on this statistic, see
  #' https://studentlife.cs.dartmouth.edu/
  #'
  #' Args:
  #'     gps(tibble): StudentLife GPS data
  #'     t1(int): Minimum timestamp desired in output
  #'     t2(int): Maximum timestamp desired in output
  #' Returns:
  #'    tibble: uid, totalDistance


  gpsSlice <- filterGPS(gps, t1, t2)

  distByUser <- gpsSlice %>%
    group_by(uid) %>%
    summarize(
      totalDistance=sum(geodist(cbind(latitude, longitude), sequential = TRUE))
    ) %>%
    ungroup

  return(distByUser)

}


gyrationRadius <- function(gps, t1=NULL, t2=NULL) {
  #' Calculates the radius of gyration in meters per uid for specified time period
  #' between t1 and t2 inclusive. If no time specified, returns for entire gps
  #' tibble. For more information on this statistic, see
  #' https://studentlife.cs.dartmouth.edu/
  #'
  #' Args:
  #'     gps(tibble): StudentLife GPS data
  #'     t1(int): Minimum timestamp desired in output
  #'     t2(int): Maximum timestamp desired in output
  #' Returns:
  #'    tibble: uid, gyrationRadius


  gpsSlice <- filterGPS(gps, t1, t2)

  # generate centroids by uid
  centroids <- gpsSlice %>%
    group_by(uid) %>%
    summarise(longCentr = centroid(as.matrix(cbind(longitude, latitude)))[1],
              latCentr = centroid(as.matrix(cbind(longitude, latitude)))[2])

  userCentroids <- left_join(gpsSlice, centroids, by="uid")

  # Function to calculate total distance. Coord Matrices with columns (Lat, Long)
  sumSquareDistance <- function(coordMatrix, centroidMatrix) return(
    sum(geodist(coordMatrix, centroidMatrix)^2)
  )

  # calculate distance from centroids to get gyration radius
  gyrDist <- userCentroids %>%
    group_by(uid,
             latGroup = cumsum(c(1, abs(diff(latitude)) > 0.1)),
             longGroup = cumsum(c(1, abs(diff(longitude)) > 0.1))) %>%
    summarize(
      timeSpent = max(timestamp) - min(timestamp),
      distance= sumSquareDistance(
        cbind(latitude, longitude), cbind(latCentr, longCentr)
      )
    ) %>%
    mutate(gyrationRadiusbyPlace = timeSpent * distance / sum(timeSpent)) %>%
    ungroup

  gyrDistByUser <-  gyrDist %>% group_by(uid) %>%
    summarise(gyrationRadius = sqrt(sum(gyrationRadiusbyPlace)))


  return(select(gyrDistByUser, uid, gyrationRadius))

}



stDevDisplacements <- function(gps, t1=NULL, t2=NULL) {
  #' Calculates the standard deviation of displacements per uid for specified
  #' time period between t1 and t2 inclusive. If no time specified,
  #' returns for entire gps tibble. For more information on this statistic, see
  #' https://studentlife.cs.dartmouth.edu/
  #'
  #' Args:
  #'     gps(tibble): StudentLife GPS data
  #'     t1(int): Minimum timestamp desired in output
  #'     t2(int): Maximum timestamp desired in output
  #' Returns:
  #'    tibble: uid, displaceSD


  gpsSlice <- filterGPS(gps, t1, t2)
  distTbl <- totalDistanceCovered(gpsSlice, t1, t2)
  timePeriod <- nrow(gpsSlice)

  # mean distance traveled per measurement
  dDist <- select(distTbl, totalDistance) / timePeriod

  squaredDistFromMean <- function(coordMatrix, mean) sum(
    geodist(coordMatrix, sequential = TRUE) - mean
  )^2

  sd <- gpsSlice %>% group_by(uid) %>%
    summarize(
      displaceSD=sqrt(
        squaredDistFromMean(cbind(latitude, longitude), dDist) / timePeriod)
    ) %>%
    ungroup

  return(sd)

}


t1 <- select(gps, timestamp) %>% min
t2 <- select(gps, timestamp) %>% pull %>% median(.)

stDevDisplacements(gps, t1, t2)

gyrationRadius(gps, t1, t2)

totalDistanceCovered(gps)

