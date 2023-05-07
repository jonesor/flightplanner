#' Create a grid of points representing the centers of photos
#'
#' @param altitude The altitude at which the photos will be taken (in meters).
#' @param overlap_width The width-wise overlap between photos (as a fraction between 0 and 1).
#' @param overlap_height The height-wise overlap between photos (as a fraction between 0 and 1).
#' @param survey_xaxis The width of the survey area (in meters).
#' @param survey_yaxis The height of the survey area (in meters).
#' @param plot A boolean value indicating whether to plot the photo grid (TRUE) or return the data frame of points (FALSE).
#' @param fov numeric, optional. Field of view of the camera in degrees. Default is 94.
#' @param aspectRatio numeric vector, optional. Aspect ratio of the image in the form of a 2-element vector, with the width as the first element and the height as the second element. Default is c(4,3).
#' @param ... additional arguments to be passed to the function.
#' @return If `plot` is TRUE, returns a `ggplot` object visualizing the photo grid. If `plot` is FALSE, returns a data frame of consisting of photoID and points representing the centers of each photo, and altitude.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr desc
#' @importFrom tidyr expand_grid
#' @import ggplot2
#'
#' @export
#' @examples
#' fp_photo_grid()
#' fp_photo_grid(
#'   altitude = 60, overlap_width = 0.6,
#'   overlap_height = 0.2, survey_xaxis = 400, survey_yaxis = 500, plot = TRUE
#' )
#'
fp_photo_grid <- function(altitude = 40,
                          overlap_width = 0.4,
                          overlap_height = 0.1,
                          survey_xaxis = 200,
                          survey_yaxis = 300,
                          plot = TRUE,
                          fov = 94,
                          aspectRatio = c(4, 3),
                          ...) {

  # Validate inputs
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

  # Check that plot is a boolean value
  if (!is.logical(plot)) {
    stop("Plot must be a boolean value (TRUE or FALSE)")
  }

  # Calculate the width of the footprint of each photo
  photo_footprint_width <- footprint_width(altitude, fov = fov, aspectRatio = aspectRatio)

  # Calculate the distance between photos in the width-wise direction
  distanceBetweenPhotos_widthways <- photo_footprint_width - photo_footprint_width * overlap_width

  # Calculate the height of the footprint of each photo
  photo_footprint_height <- footprint_height(altitude, fov = fov, aspectRatio = aspectRatio)

  # Calculate the distance between photos in the height-wise direction
  distanceBetweenPhotos_heightways <- photo_footprint_height - photo_footprint_height * overlap_height

  # Create a grid of points representing the centers of each photo
  # First line horizontal
  horiz <- seq(0, survey_xaxis, distanceBetweenPhotos_widthways)

  # First line vertical
  vert <- seq(0, survey_yaxis, distanceBetweenPhotos_heightways)

  # Expand the grid to create a matrix of points
  # Add the min/max edges for photo footprints
  photo_grid <- expand_grid(horiz, vert) |>
    mutate(xmin = horiz - (photo_footprint_width / 2), xmax = horiz + (photo_footprint_width / 2)) |>
    mutate(ymin = vert - (photo_footprint_height / 2), ymax = vert + (photo_footprint_height / 2)) |>
    mutate(photoID = as.factor(1:dplyr::n())) |>
    mutate(altitude = altitude)


  # If plot is set to TRUE, create a visual representation of the grid with ggplot
  if (plot == TRUE) {
    # Visualise the image grid, withe the photo overlaps
    (p <- ggplot(photo_grid, aes(x = horiz, y = vert)) +
      geom_point() +
      geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = photoID), alpha = 0.1) +
      coord_fixed() +
      ggtitle("Photo grid", subtitle = paste0("Altitude: ", altitude, "m; Gimball at -90 degrees")) +
      theme_minimal() +
      theme(legend.position = "none"))
    return(p)
  }
  # If plot is set to FALSE, return the data frame of points
  if (plot == FALSE) {
    return(photo_grid |>
      select(photoID, horiz, vert, altitude))
  }
}
