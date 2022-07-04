#' Generate Rayshader Rollercoaster
#'
#'@param frames Default `360`. Number of frames in the animation.
#'@param closed Default `TRUE`. Whether to close the loop in the rollercoaster.
#'@param track_radius Default `1`. Radius of the track.
#'@param track_material Default `diffuse(color="red")`. Material for the track.
#'@param post_interval Default `10`. Keyframe interval between posts supporting track.
#'@param post_material Default `diffuse(color="grey10")`. Material for the track.
#'@param viewer_offset Default `5`. Height of the viewer along the track.
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
generate_rayshader_coaster = function(frames=360,  closed = TRUE, viewer_offset = 5,
                                      track_radius = 1,
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
    stop("FOV must be greater than 0 to fly through scene—preferably something large, like 120.")
  }
  if(!rayrender:::has_gui_capability()) {
    stop("This version of rayrender was not built with interactive controls enabled—build the developmental version from github.")
  }

  message("Fly through the scene and press `K` at each point where you want the rollercoaster to travel through, and press ESC when done (or close the window). Below are the interactive controls. Try pressing `TAB` to switch to free flying mode if you get stuck.\n")
  scene = rayshader::render_highquality(light = light, lightdirection = lightdirection, lightaltitude = lightaltitude, lightsize=lightsize,
                                        lightintensity = lightintensity, lightcolor = lightcolor, obj_material = obj_material,
                                        cache_filename=cache_filename, width = width, height = height,
                                        ground_material = ground_material,
                                        ground_size=ground_size, scene_elements=scene_elements, return_scene = TRUE, ...)
  rayshader::render_highquality(light = light, lightdirection = lightdirection, lightaltitude = lightaltitude, lightsize=lightsize,
                                        lightintensity = lightintensity, lightcolor = lightcolor, obj_material = obj_material,
                                        cache_filename=cache_filename, width = width, height = height,
                                        ground_material = ground_material,samples=1000, sample_method="sobol_blue",
                                        ground_size=ground_size, scene_elements=scene_elements, return_scene = FALSE, ...)
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
  return(list(scene = rayrender::add_object(scene, path_obj),motion = motion))
}
