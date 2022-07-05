#' Generate Rayshader Rollercoaster
#'
#'@param frames Default `360`. Number of frames in the animation.
#'@param closed Default `TRUE`. Whether to close the loop in the rollercoaster.
#'@param track_radius Default `NA`. Radius of the track. Defaults to 1/100th the minimum bounding box of the motion.
#'@param track_material Default `diffuse(color="red")`. Material for the track.
#'@param post_interval Default `10`. Keyframe interval between posts supporting track.
#'@param post_material Default `diffuse(color="grey10")`. Material for the track.
#'@param viewer_offset Default `NA`. Height of the viewer along the track.  Defaults to twice the `track_radius`.
#'@param posts Default `TRUE`. Whether to include posts holding the track up.
#'@param light Default `TRUE`. Whether there should be a light in the scene. If not, the scene will be lit with a bluish sky.
#'@param lightdirection Default `315`. Position of the light angle around the scene.
#'If this is a vector longer than one, multiple lights will be generated (using values from
#'`lightaltitude`, `lightintensity`, and `lightcolor`)
#'@param lightaltitude Default `45`. Angle above the horizon that the light is located.
#'If this is a vector longer than one, multiple lights will be generated (using values from
#'`lightdirection`, `lightintensity`, and `lightcolor`)
#'@param lightsize Default `NULL`. Radius of the light(s). Automatically chosen, but can be set here by the user.
#'@param lightintensity Default `500`. Intensity of the light.
#'@param lightcolor Default `white`. The color of the light.
#'@param obj_material Default `rayrender::diffuse()`. The material properties of the object file.
#'@param cache_filename Name of temporary filename to store OBJ file, if the user does not want to rewrite the file each time.
#'@param width Defaults to the width of the rgl window. Width of the rendering.
#'@param height Defaults to the height of the rgl window. Height of the rendering.
#'@param ground_material Default `diffuse()`. Material defined by the rayrender material functions.
#'@param ground_size Default `100000`. The width of the plane representing the ground.
#'@param scene_elements Default `NULL`. Extra scene elements to add to the scene, created with rayrender.
#'@param ... Additional parameters to pass to `rayrender::render_scene()`
#'
#' @return A list with the resulting rayrender scene and the motion keyframes to pass to `rayrender::render_animation()`
#' @export
#'
#' @examples
#' #Generate a rayshader scene
#' \donttest{
#' library(rayshader)
#'
#' volcano |>
#'   sphere_shade() |>
#'   plot_3d(volcano,fov=90)
#'
#' #Now, let's fly through a series of points to build the rollercoaster.
#' #You can pass arguments to `render_scene()` via `...`.
#' #This function doesn't return anything, but saves the scene with the rollercoaster and the
#' #motion information internally. This extracts the scene from `rayshader` directly from `rgl`.
#'
#' #Here, we turn off the lighting and use ambient lighting only
#' generate_rayshader_coaster(light=FALSE, ambient_light=T)
#'
#' #We can also add extra lights by passing arguments via `...` to `rayshader::render_highquality()`
#' generate_rayshader_coaster(lightdirection=c(315,45),lightcolor=c("orange","white"))
#'
#' #We now can create our animation using `animate_rollercoaster()`. This extracts the information
#' #(scene and motion) in the previous step. If no filename is given,
#' #it will simply preview the animation in the interactive screen, without saving anything.
#' #Set samples to `1` to render a quick preview.
#' animate_rollercoaster(samples=1, width=200,height=200)
#'
#' #Increase the number of samples and resolution for a high quality animation (but longer render):
#' animate_rollercoaster(samples=128, width=800, height=800)
#'
#' #Set the FOV to 360 to render a movie for VR headsets
#' #Use the Spatial Media Metadata Injector app from google to add the metadata to render in VR on
#' #youtube (available here: https://github.com/google/spatial-media/releases)
#' animate_rollercoaster(samples=128, width=800, height=800, fov=360, filename="video360.mp4")
#'
#' #Alternatively, get the scene and motion data yourself and call `rayrender::render_animation()`
#' #directly.
#' scene = get_rayrender_scene()
#' motion = get_rayrender_motion()
#'
#' render_animation(scene, motion, samples=1, sample_method="sobol_blue")
#'}
generate_rayshader_coaster = function(frames=360,  closed = TRUE, viewer_offset = NA,
                                      track_radius = NA,
                                      track_material = rayrender::diffuse(color="red"),
                                      posts = TRUE, post_interval = 10,
                                      post_material = rayrender::diffuse(color="grey10"),
                                      light = TRUE, lightdirection = 315, lightaltitude = 45, lightsize=NULL,
                                      lightintensity = 500, lightcolor = "white", obj_material = rayrender::diffuse(),
                                      cache_filename=NULL, width = NULL, height = NULL,
                                      ground_material = rayrender::diffuse(),
                                      ground_size=10000, scene_elements=NULL, ...) {
  if(rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  cam = rayshader::render_camera()
  if(cam[4] == 0) {
    stop("FOV must be greater than 0 to fly through scene--preferably something large, like 120.")
  }
  if(!rayrender:::has_gui_capability()) {
    stop("This version of rayrender was not built with interactive controls enabled--build the developmental version from github.")
  }

  message("Fly through the scene and press `K` at each point where you want the rollercoaster to travel through, and press ESC when done (or close the window). Below are the interactive controls. Try pressing `TAB` to switch to free flying mode if you get stuck.\n")
  message(
    "--------------------------Interactive Mode Controls---------------------------
W/A/S/D: Horizontal Movement: | Q/Z: Vertical Movement | Up/Down: Adjust FOV | ESC: Close
Left/Right: Adjust Aperture  | 1/2: Adjust Focal Distance | 3/4: Rotate Environment Light
P: Print Camera Info | R: Reset Camera |  TAB: Toggle Orbit Mode |  E/C: Adjust Step Size
K: Save Keyframe | L: Reset Camera to Last Keyframe (if set) | F: Toggle Fast Travel Mode
Left Mouse Click: Change Look At (new focal distance) | Right Mouse Click: Change Look At ")
  scene = rayshader::render_highquality(light = light, lightdirection = lightdirection, lightaltitude = lightaltitude, lightsize=lightsize,
                                        lightintensity = lightintensity, lightcolor = lightcolor, obj_material = obj_material,
                                        cache_filename=cache_filename, width = width, height = height,
                                        ground_material = ground_material,
                                        ground_size=ground_size, scene_elements=scene_elements, return_scene = TRUE, ...)
  suppressMessages(
  rayshader::render_highquality(light = light, lightdirection = lightdirection, lightaltitude = lightaltitude, lightsize=lightsize,
                                        lightintensity = lightintensity, lightcolor = lightcolor, obj_material = obj_material,
                                        cache_filename=cache_filename, width = width, height = height,
                                        ground_material = ground_material,samples=1000, sample_method="sobol_blue",
                                        ground_size=ground_size, scene_elements=scene_elements, return_scene = FALSE, ...))
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
                               type = "bezier", offset_lookat = 0.1, closed = closed)
  motion_offset = motion[,1:3]
  motion_offset[,1] = 0
  motion_offset[,3] = 0
  motion_offset[,2] = viewer_offset
  path_obj = rayrender::path(points = motion[,1:3]-motion_offset,
                             width = track_radius,material=track_material, closed = closed)

  ground_depth = scene[scene$shape == "xz_rect",]
  if(posts && !is.null(ground_depth)) {
    strut_depth = as.numeric(ground_depth$y)
    strutlist = list()
    for(i in seq(1,nrow(motion),by=post_interval)) {
      end_height = as.numeric(motion[i,1:3]) - c(0,viewer_offset,0)
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
