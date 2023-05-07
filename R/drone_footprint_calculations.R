#' @title Drone Footprint Calculations
#'
#' @description Calculates various aspects of a drone's camera footprint at a given altitude, field of view, and image aspect ratio.
#'
#' @name footprint_
#' @param altitude numeric, required. Altitude of the drone in meters.
#' @param fov numeric, optional. Field of view of the camera in degrees. Default is 94.
#' @param aspectRatio numeric vector, optional. Aspect ratio of the image in the form of a 2-element vector, with the width as the first element and the height as the second element. Default is c(4,3).
#' @param ... additional arguments to be passed to the function.
#'
#' @return A single numeric value representing the calculated aspect of the camera footprint.
#'
#' @examples
#' # Calculate the width of the camera footprint at an altitude of 100 meters
#' footprint_width(altitude = 100)
#'
#' # Calculate the height of the camera footprint at an altitude of 50 meters
#' # and a field of view of 120 degrees
#' footprint_height(altitude = 50, fov = 120)
#'
#' # Calculate the diagonal length of the camera footprint at an altitude of
#' # 200 meters, a field of view of 90 degrees, and an image aspect ratio of
#' # 4:3
#' footprint_diagonal(altitude = 200, fov = 90, aspectRatio = c(4, 3))
#' @export

#' @rdname footprint_
footprint_diagonal <- function(altitude, fov = 94, aspectRatio = c(4, 3), ...) {
  imageWidth <- aspectRatio[1]
  imageHeight <- aspectRatio[2]

  # Check that altitude is a positive number
  if (altitude <= 0) {
    stop("Altitude must be a positive number")
  }

  # Check that fov is between 0 and 180 degrees
  if (fov < 0 || fov > 180) {
    stop("Field of view must be between 0 and 180 degrees")
  }

  # Check that image dimensions are positive numbers
  if (imageHeight <= 0 || imageWidth <= 0) {
    stop("Image aspect ratio dimensions must be positive numbers")
  }

  fov_radians <- fov * pi / 180
  return(2 * tan(fov_radians / 2) * altitude)
}

#' @rdname footprint_
footprint_height <- function(altitude, fov = 94, aspectRatio = c(4, 3), ...) {
  imageWidth <- aspectRatio[1]
  imageHeight <- aspectRatio[2]

  # Check that altitude is a positive number
  if (altitude <= 0) {
    stop("Altitude must be a positive number")
  }

  # Check that fov is between 0 and 180 degrees
  if (fov < 0 || fov > 180) {
    stop("Field of view must be between 0 and 180 degrees")
  }

  # Check that image dimensions are positive numbers
  if (imageHeight <= 0 || imageWidth <= 0) {
    stop("Image dimensions must be positive numbers")
  }

  fov_radians <- fov * pi / 180
  aspectRatio_fraction <- imageHeight / imageWidth
  d <- 2 * tan(fov_radians / 2) * altitude
  return(aspectRatio_fraction * d / sqrt(aspectRatio_fraction^2 + 1))
}

#' @rdname footprint_
footprint_width <- function(altitude, fov = 94, aspectRatio = c(4, 3), ...) {
  imageWidth <- aspectRatio[1]
  imageHeight <- aspectRatio[2]

  # Check that altitude is a positive number
  if (altitude <= 0) {
    stop("Altitude must be a positive number")
  }

  # Check that fov is between 0 and 180 degrees
  if (fov < 0 || fov > 180) {
    stop("Field of view must be between 0 and 180 degrees")
  }

  # Check that image dimensions are positive numbers
  if (imageHeight <= 0 || imageWidth <= 0) {
    stop("Image dimensions must be positive numbers")
  }

  fov_radians <- fov * pi / 180
  aspectRatio_fraction <- imageHeight / imageWidth
  d <- 2 * tan(fov_radians / 2) * altitude
  return(d / sqrt(aspectRatio_fraction^2 + 1))
}
