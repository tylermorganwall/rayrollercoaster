#' Generate Rayrender Rollercoaster
#'
#'@param scene A rayrender scene.
#'@param frames Default `360`. Number of frames in the animation.
#'@param closed Default `TRUE`. Whether to close the loop in the rollercoaster.
#'@param track_radius Default `1`. Radius of the track.
#'@param track_material Default `diffuse(color="red")`. Material for the track.
#'@param posts Default `TRUE`. Whether to include posts holding the track up.
#'@param post_interval Default `10`. Keyframe interval between posts supporting track.
#'@param post_material Default `diffuse(color="grey10")`. Material for the track.
#'@param post_depth Default `-1`. Depth to which the posts are drawn.
#'@param viewer_offset Default `1`. Height of the viewer along the track.
#'@param ... Additional parameters to pass to `rayrender::render_scene()`.
#'
#' @return A list with the resulting rayrender scene and the motion keyframes to pass to `rayrender::render_animation()`
#' @export
#'
#'@examples
#'#Generate a rayrender
generate_rayrender_coaster = function(scene, frames=360,  closed = TRUE, viewer_offset = 2,
                                      track_radius = 1,
                                      track_material = rayrender::diffuse(color="red"),
                                      posts = TRUE, post_interval = 10,
                                      post_material = rayrender::diffuse(color="grey10"), post_depth = -1,
                                      ...) {
  if(!rayrender:::has_gui_capability()) {
    stop("This version of rayrender was not built with interactive controls enabled—build the developmental version from github.")
  }

  message("Fly through the scene and press `K` at each point where you want the rollercoaster to travel through. Below are the interactive controls. Try pressing `TAB` to switch to free flying mode if you get stuck.\n")
  rayrender::render_scene(scene, samples=1000,sample_method="sobol_blue", ...)
  keyframes = rayrender::get_saved_keyframes()

  if(nrow(keyframes) == 0) {
    stop("No control points saved during interactive session.")
  }
  if(nrow(keyframes) == 2 && closed) {
    warning("Only two control points--can't be a closed curve.")
    closed = FALSE
  }
  path_offset = keyframes[,1:3]
  path_offset[,1] = 0
  path_offset[,3] = 0
  path_offset[,2] = viewer_offset

  motion = rayrender::generate_camera_motion(positions = keyframes[,1:3]+path_offset, lookats=keyframes[,1:3]+path_offset,
                                             frames = frames, constant_step = T,
                                             type = "bezier", offset_lookat = 1, closed = closed)

  motion_offset = motion[,1:3]
  motion_offset[,1] = 0
  motion_offset[,3] = 0
  motion_offset[,2] = viewer_offset
  path_obj = rayrender::path(points = motion[,1:3] - motion_offset,
                             width = track_radius,material=track_material, closed = closed)

  if(posts) {
    strut_depth = post_depth
    strutlist = list()
    for(i in seq(1,nrow(motion),by=post_interval)) {
      end_height = as.numeric(motion[i,1:3])-c(0,viewer_offset,0)
      strutlist[[i]] = rayrender::segment(start = end_height-c(0,track_radius/2,0),
                                          end = c(end_height[1],strut_depth,end_height[3]),
                                          radius=track_radius/3, material=post_material)
    }
    strutscene = do.call(rbind,strutlist)
    scene = rayrender::add_object(scene, strutscene)
  }
  return(list(scene = rayrender::add_object(scene, path_obj),motion = motion))
}
