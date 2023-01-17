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

#' Convert a data frame to an sf object with WGS 84 coordinate reference system (CRS)
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
#' @return The input sf object transformed to a meter-based coordinate reference system (CRS).
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
#' @param photo_grid A photo grid data frame created using the `fp_photo_grid` function.
#' @param origin_lat The latitude of the starting point for the mission (in degrees).
#' @param origin_long The longitude of the starting point for the mission (in degrees).
#' @param angleDeg The angle at which the survey grid is rotated from North (in degrees). e.g. a value of 45 rotates the photo grid around the origin point (bottom-left) by 45 degrees.
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
#' photo_grid <- fp_photo_grid(
#'   altitude = 60, overlap_width = 0.6,
#'   overlap_height = 0.2, survey_xaxis = 100, survey_yaxis = 200, plot = FALSE
#' )
#'
#' litchiMission <- fp_litchi_mission(photo_grid, pg,
#'   origin_lat = 55.125505,
#'   origin_long = 10.268467, angle = 38
#' )
#' \dontrun{
#' write.csv(
#'   x = litchiMission, file = "missionFiles/testGrid10.csv",
#'   row.names = FALSE
#' )
#' }
#'
fp_litchi_mission <- function(photo_grid, origin_lat, origin_long, angleDeg = 0, ...) {

  # Validation of inputs
  # photo_grid, should be a data frame containing columns for photoID, horiz, vert, and altitude
  if (!inherits(photo_grid, "data.frame")) {
    stop("photo_grid must be a data frame")
  }
  if (!is.numeric(photo_grid$horiz) || !is.numeric(photo_grid$vert)) {
    stop("photoID, horiz, and vert must be numeric columns in photo_grid")
  }
  if (length(unique(photo_grid$photoID)) != nrow(photo_grid)) {
    stop("photoID must be a unique identifier for each row in photo_grid")
  }
  if (min(photo_grid$altitude) <= 0) {
    stop("altitude must contain a positive value in photo_grid")
  }
  if (any(photo_grid$horiz < 0) || any(photo_grid$vert < 0)) {
    stop("horiz and vert cannot be negative in photo_grid")
  }


  # Check that angle is between 0 and 360 degrees
  if (angleDeg < 0 || angleDeg > 360) {
    stop("Angle must be between 0 and 360 degrees")
  }

  # Set projection of origin point as coordinate reference system (CRS) WGS 84 (crs 4326)
  origin_sf <- convertToSf(data.frame(latitude = origin_lat, longitude = origin_long))

  # Convert origin point to meter-based reference system
  origin1_sf_m <- transformSf(origin_sf)

  # Convert angle from degrees to radians
  angle_rad <- angleDeg * pi / 180

  # Compute the rotation matrix
  R <- computeRotationMatrix(angle_rad)

  # Create a photo grid, based on overlap, height and survey extent
  photo_grid_points <- photo_grid |> select(horiz, vert)

  photo_grid_altitude <- photo_grid$altitude

  # Rotate each point in the data frame
  rotated_grid_points <- as.data.frame(t(apply(photo_grid_points, 1, function(p) R %*% as.matrix(p))))
  names(rotated_grid_points) <- c("x", "y")

  photo_grid_points$x_rot <- rotated_grid_points$x
  photo_grid_points$y_rot <- rotated_grid_points$y

  # Place the photo grid onto on lat long
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
    altitude = photo_grid_altitude,
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
