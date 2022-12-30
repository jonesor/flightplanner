# Function for grid creation.
#
# litchiGrid is a function that creates a grid in Litchi format with points
# arranged in columns and rows. The user must select an origin point, and a
# length on the horizontal (west-east) and vertical (north-south) axis. They
# also must set the amount of overlap required and flight altitude of the drone.
# The angleDeg defines a rotation whereby the set of points is rotated around
# the origin.

# The script imageFootprintCalculations.R contains the necessarry functions. The
# angle of the FieldOfView (fov), and aspect ratio is assumed as for a DJI
# Phantom 4 Pro (fov = 94, imageHeight = 3, imageWidth = 4) I could alter this
# in a dropdown menu, if converted to a shiny app. e.g. I could pick a drone
# from the dropdown, and these values could be assigned automratically.


#' Compute a rotation matrix for a given angle
#'
#' @param angle_rad The angle in radians for which to compute the rotation matrix.
#' @return A rotation matrix.
#' @keywords internal
#' @export
#'
computeRotationMatrix <- function(angle_rad) {
  matrix(c(cos(angle_rad), -sin(angle_rad), sin(angle_rad), cos(angle_rad)), nrow = 2)
}

#' Convert a data frame to an sf object with WGS 84 CRS
#'
#' @param data A data frame containing latitude and longitude columns.
#' @return An sf object with WGS 84 CRS.
#' @keywords internal
#'
#' @importFrom sf st_as_sf
#'
#' @export
#' @examples
#' convertToSf(data.frame(latitude = 45.5236, longitude = -122.675))
#'
convertToSf <- function(data) {
  st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
}

#' Transform an sf object to a meter-based CRS
#'
#' @param data An sf object.
#' @return The input sf object transformed to a meter-based CRS.
#' @keywords internal
#'
#' @importFrom sf st_transform
#'
#' @export
#' @examples
#' data <- data.frame(latitude = 45.5236, longitude = -122.675)
#' data <- convertToSf(data)
#' transformSf(data)
transformSf <- function(data) {
  st_transform(data, 3857)
}

#' Generate a mission plan for a drone in Litchi app
#'
#' @param origin_lat The latitude of the starting point for the mission (in degrees).
#' @param origin_long The longitude of the starting point for the mission (in degrees).
#' @param altitude The altitude at which the photos will be taken (in meters).
#' @param overlap_width The width-wise overlap between photos (as a fraction between 0 and 1).
#' @param overlap_height The height-wise overlap between photos (as a fraction between 0 and 1).
#' @param survey_xaxis The width of the survey area (in meters).
#' @param survey_yaxis The height of the survey area (in meters).
#' @param angleDeg The angle at which the survey grid is rotated from North (in degrees). e.g. a value of 45 rotates the photo grid around the origin point (bottom-left) by 45 degrees.
#' @param fov numeric, optional. Field of view of the camera in degrees. Default is 94.
#' @param aspectRatio numeric vector, optional. Aspect ratio of the image in the form of a 2-element vector, with the width as the first element and the height as the second element. Default is c(4,3).
#' @param ... additional arguments to be passed to the function.
#' @return A data frame containing the mission plan for the drone, in a format suitable for use with the Litchi app.
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_coordinates
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#'
#' @export
#' @examples
#' litchiMission <- litchiGrid(origin_lat = 55.124818,
#' origin_long = 10.263445, altitude = 60, overlap_width = 0.4,
#' overlap_height = 0.1, survey_xaxis = 200, survey_yaxis = 600, angleDeg = 45)
#' \dontrun{
#' write.csv(x = litchiMission, file = "missionFiles/testGrid10.csv",
#' row.names = FALSE)
#' }
#'
litchiGrid <- function(origin_lat, origin_long, altitude = 100,
                       overlap_width = 0.75, overlap_height = 0.75, survey_xaxis = 200,
                       survey_yaxis = 300, angleDeg = 0, fov = 94, aspectRatio = c(4, 3), ...) {

  # Validation of inputs
  # Check that altitude is a positive number
  if (altitude <= 0) {
    stop("Altitude must be a positive number")
  }

  # Check that overlap width and overlap height are between 0 and 1
  if (overlap_width < 0 || overlap_width > 1) {
    stop("Overlap width must be between 0 and 1")
  }
  if (overlap_height < 0 || overlap_height > 1) {
    stop("Overlap height must be between 0 and 1")
  }

  # Check that angle is between 0 and 360 degrees
  if (angleDeg < 0 || angleDeg > 360) {
    stop("Angle must be between 0 and 360 degrees")
  }

  # Set projection as coordinate reference system (CRS) WGS 84 (crs 4326)
  origin_sf <- convertToSf(data.frame(latitude = origin_lat, longitude = origin_long))

  # Convert origin points to meter-based reference system
  origin1_sf_m <- transformSf(origin_sf)

  # Convert angle from degrees to radians
  angle_rad <- angleDeg * pi / 180

  # Compute the rotation matrix
  R <- computeRotationMatrix(angle_rad)

  # Create a photo grid, based on overlap, height and survey extent
  photo_grid_points <- photoGrid(altitude = altitude,
                                 overlap_width = overlap_width,
                                 overlap_height = overlap_height,
                                 survey_xaxis = survey_xaxis,
                                 survey_yaxis = survey_yaxis,
                                 fov = fov, aspectRatio = aspectRatio,
                                 plot = FALSE) |> select(horiz, vert)

  # Rotate each point in the data frame
  rotated_grid_points <- as.data.frame(t(apply(photo_grid_points, 1, function(p) R %*% as.matrix(p))))
  names(rotated_grid_points) <- c("x", "y")

  photo_grid_points$x_rot <- rotated_grid_points$x
  photo_grid_points$y_rot <- rotated_grid_points$y

  # Place the photogrid onto on lat long
  photo_grid_points <- photo_grid_points |>
    select(x_rot, y_rot) |>
    mutate(long = x_rot + st_coordinates(origin1_sf_m)[, "X"]) |>
    mutate(lat = y_rot + st_coordinates(origin1_sf_m)[, "Y"])

  photo_grid_points_sf <- st_as_sf(photo_grid_points,
    coords = c("long", "lat"),
    crs = 3857, agr = "constant"
  )

  photo_grid_points_sf_latlong <- st_transform(photo_grid_points_sf, 4326)

  # Add the X/Y coordinates to the dataframe
  photo_grid_points_sf_latlong$lng <- st_coordinates(photo_grid_points_sf_latlong)[, "X"]
  photo_grid_points_sf_latlong$lat <- st_coordinates(photo_grid_points_sf_latlong)[, "Y"]

  # Litchi file creation
  litchiMission <- data.frame(
    lat = photo_grid_points_sf_latlong$lat, lng = photo_grid_points_sf_latlong$lng,
    altitude = rep(altitude, nrow(photo_grid_points_sf_latlong)),
    heading = angleDeg,
    curve = 0, rotationdir = 0, gimbalmode = 2, gimbalangle = -90,
    actiontype1 = 0, actionparam1 = 1500,
    actiontype2 = 1, actionparam2 = 0,
    altitudemode = 0
  ) |> mutate(rowNumber = as.numeric(as.factor(lat)))

  # Some rearranging, necessary to ensure an efficient "lawnmower" flight path.
  litchiMission_oddRowID <- seq(1, max(litchiMission$rowNumber), 2)

  # The odd rows...
  litchiMission_odd <- litchiMission |>
    filter(rowNumber %in% litchiMission_oddRowID)

  # The even rows (sorted in decending longitude order)...
  litchiMission_even <- litchiMission |>
    filter(!rowNumber %in% litchiMission_oddRowID) |>
    arrange(desc(lng))

  # Recombine the two parts of the data frame
  litchiMission <- bind_rows(litchiMission_odd, litchiMission_even) |>
    arrange(lat)

  rownames(litchiMission) <- NULL

  return(litchiMission)
}
