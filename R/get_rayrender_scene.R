#' Get Scene Info
#'
#' @description Extract the rayrender scene with the rollercoaster (to render on it's own).
#' @return Data frame
#'
#' @export
#' @examples
#' #This will return an empty data frame if no rollercoaster has been created.
#' get_rayrender_scene()
get_rayrender_scene = function() {
  scene = get("scene_info",  envir = ray_environment)
  if(is.null(scene) || nrow(scene) == 0) {
    message("No scene saved yet.")
  }
  return(scene)
}

#' Get Animation Info
#'
#' @description Extract the motion information for the rollercoaster scene.
#' @return Data frame
#'
#' @export
#' @examples
#' #This will return an empty data frame if no rollercoaster has been created.
#' get_rayrender_motion()
get_rayrender_motion = function() {
  motion = get("motion_info",  envir = ray_environment)
  if(is.null(motion) || nrow(motion) == 0) {
    message("No motion info saved yet.")
  }
  return(motion)
}
