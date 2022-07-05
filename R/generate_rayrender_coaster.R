#' Generate Rayrender Rollercoaster
#'
#'@param scene A rayrender scene.
#'@param frames Default `360`. Number of frames in the animation.
#'@param closed Default `TRUE`. Whether to close the loop in the rollercoaster.
#'@param track_radius Default `NA`. Radius of the track. Defaults to 1/100th the minimum bounding box of the motion.
#'@param track_material Default `diffuse(color="red")`. Material for the track.
#'@param posts Default `TRUE`. Whether to include posts holding the track up.
#'@param post_interval Default `10`. Keyframe interval between posts supporting track.
#'@param post_material Default `diffuse(color="grey10")`. Material for the track.
#'@param post_depth Default `-1`. Depth to which the posts are drawn.
#'@param viewer_offset Default `NA`. Height of the viewer along the track. Defaults to twice the `track_radius`.
#'@param ... Additional parameters to pass to `rayrender::render_scene()`.
#'
#' @return A list with the resulting rayrender scene and the motion keyframes to pass to `rayrender::render_animation()`
#' @export
#'
#'@examples
#' #Generate a rayrender scene (an R on a checkerboard, with a glass sphere)
#' \donttest{
#' library(rayrender)
#' scene = generate_ground(material=diffuse(checkercolor="grey20")) |>
#'   add_object(obj_model(r_obj(),y=-1,x=1, material=diffuse(color = "purple"))) |>
#'   add_object(sphere(y=-0,x=-1,radius=1, material=dielectric())) |>
#'   add_object(sphere(y=5,x=5,z=5,material=light(intensity = 50))) |>
#'   add_object(sphere(y=5,x=-5,z=5,material=light(color="orange",intensity = 50)))
#' render_scene(scene, fov=40,sample_method="sobol_blue")
#'
#' #Now, let's fly through a series of points to build the rollercoaster.
#' #You can pass arguments to `render_scene()` via `...`.
#' #This function doesn't return anything, but saves the scene with the rollercoaster and the
#' #motion information internally.
#' generate_rayrender_coaster(scene, fov = 120)
#'
#' #We now can create our animation using `animate_rollercoaster()`. This extracts the information
#' #(scene and motion) in the previous step. If no filename is given,
#' #it will simply preview the animation in the interactive screen, without saving anything.
#' #Set samples to `1` to render a quick preview.
#' animate_rollercoaster(samples=1)
#'
#' #Increase the number of samples and resolution for a high quality animation (but longer render):
#' animate_rollercoaster(samples=128, width=800, height=800)
#'
#' #Set the FOV to 360 to render a movie for VR headsets
#' #Use the Spatial Media Metadata Injector app from google to add the metadata to render in VR on
#' #youtube (available here: https://github.com/google/spatial-media/releases)
#' animate_rollercoaster(samples=128, width=800, height=800, fov=360, filename="video360.mp4")
#' }
generate_rayrender_coaster = function(scene, frames=360,  closed = TRUE, viewer_offset = NA,
                                      track_radius = NA,
                                      track_material = rayrender::diffuse(color="red"),
                                      posts = TRUE, post_interval = 10,
                                      post_material = rayrender::diffuse(color="grey10"), post_depth = -1,
                                      ...) {
  if(!rayrender:::has_gui_capability()) {
    stop("This version of rayrender was not built with interactive controls enabled--build the developmental version from github.")
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

  if(is.na(track_radius)) {
    track_radius = max((apply(keyframes,2,max) - apply(keyframes,2,min))[c(1,3)])/100
    viewer_offset = track_radius*2
  }
  path_offset = keyframes[,1:3]
  path_offset[,1] = 0
  path_offset[,3] = 0
  path_offset[,2] = viewer_offset

  motion = rayrender::generate_camera_motion(positions = keyframes[,1:3]+path_offset, lookats=keyframes[,1:3]+path_offset,
                                             frames = frames, constant_step = F,
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
  scene = rayrender::add_object(scene, path_obj)
  assign("motion_info",motion, envir = ray_environment)
  assign("scene_info",scene, envir = ray_environment)
  message("Scene and motion info saved. Create the animation by calling `animate_rollercoaster()`, or call `get_rayrender_scene()` or `get_rayrender_motion()` to access the rayrender scene and motion info directly (which can be passed to `rayrender::render_animation()`.")
}
