#'Animate Rollercoaster
#'
#' @param filename Default `NA`. No files saved by default, simply plotted to the preview display.
#' By default, any string will save the individual frames as `png` image files. If
#' the file extension is `mp4`, the frames will automatically be converted to a movie using the {av}
#' package.
#' @param samples Default `128`. Number of samples
#' @param sample_method Default `sobol_blue`. Sample method (see rayrender documentation).
#' @param fov Default `NA`. Field of view, overriding the value set in the motion dataframe. Set
#' this to `360` to get a video that can be converted for virtual reality/headset viewing.
#' @param ... Additional parameters to pass to `rayrender::render_animation()`.
#' @param keep_images Default `TRUE`. Whether to keep all the image frames if the output is an mp4 file.
#' @param framerate Default `30`. Only used if rendering an mp4 file. Frames per second of the movie.
#' @param vfilter Default `"null"`. See documentation for `av::av_encode_video`.
#' @param codec Default `NULL`. See documentation for `av::av_encode_video`.
#' @param audio Default `NULL`. See documentation for `av::av_encode_video`.
#'
#' @return None
#' @export
#'
#'@examples
#'#Generate a rayrender scene (an R on a checkerboard, with a glass sphere)
#'\donttest{
#'library(rayrender)
#'scene = generate_ground(material=diffuse(checkercolor="grey20")) |>
#'  add_object(obj_model(r_obj(),y=-1,x=1, material=diffuse(color = "purple"))) |>
#'  add_object(sphere(y=-0,x=-1,radius=1, material=dielectric())) |>
#'  add_object(sphere(y=5,x=5,z=5,material=light(intensity = 50))) |>
#'  add_object(sphere(y=5,x=-5,z=5,material=light(color="orange",intensity = 50)))
#'render_scene(scene, fov=40,sample_method="sobol_blue")
#'
#'#Now, let's fly through a series of points. You can pass arguments to `render_scene()` via `...`
#'#This function doesn't return anything, but saves the scene with the rollercoaster and the
#'#motion information internally.
#'generate_rayrender_coaster(scene, fov = 120)
#'
#'#We now can create our animation using `animate_rollercoaster()`. This extracts the information
#'#(scene and motion) in the previous step. If no filename is given,
#'#it will simply preview the animation in the interactive screen, without saving anything.
#'#Set samples to `1` to render a quick preview.
#'animate_rollercoaster(samples=1, width=200,height=200)
#'
#'#Increase the number of samples and resolution for a high quality animation (but longer render):
#'animate_rollercoaster(samples=128, width=800, height=800)
#'
#'#Set the FOV to 360 to render a movie for VR headsets
#'animate_rollercoaster(samples=128, width=800, height=800, filename="video360.mp4")
#'
#'#Alternatively, get the scene and motion data yourself and call `rayrender::render_animation()`
#'#directly.
#'scene = get_rayrender_scene()
#'motion = get_rayrender_motion()
#'
#'render_animation(scene, motion, samples=1, sample_method="sobol_blue")
#'}
animate_rollercoaster = function(filename = NA, samples = 128, sample_method="sobol_blue", fov=NA,
                                 keep_images = TRUE, framerate = 30, vfilter = "null",
                                 codec = NULL, audio = NULL,
                                 ...) {
  save_video = FALSE
  save_images = FALSE
  if(!is.na(filename)) {
    ext = tools::file_ext(filename)
    if(ext == "mp4") {
      save_video = TRUE
    }
    if(ext == "png" || ext == "") {
      save_images = TRUE
      filename = tools::file_path_sans_ext(filename)
    }
  }
  scene = get_rayrender_scene()
  motion = get_rayrender_motion()
  if(nrow(scene) == 0 || nrow(motion) == 0) {
    stop("Generate rollercoaster with `generate_rayshader_coaster()` or `generate_rayrender_coaster()` first.")
  }
  if(!is.na(fov)) {
    motion$fov = fov
  }

  if(save_images || is.na(filename)) {
    rayrender::render_animation(scene,motion, filename = filename, samples= samples,
                                sample_method = sample_method, ...)
  }
  if(save_video) {
    if(!keep_images) {
      tempfilename = tempfile()
    } else {
      tempfilename = tools::file_path_sans_ext(filename)
    }
    frames = nrow(motion)
    rayrender::render_animation(scene, motion, filename = tempfilename, samples= samples,
                                sample_method = sample_method, ...)
    av::av_encode_video(input = sprintf("%s%d.png",tempfilename,seq_len(frames)),
                        output = filename, framerate = framerate, vfilter = vfilter,
                        codec = codec, audio = audio)
  }
}
