
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rayrollercoaster

https://user-images.githubusercontent.com/297609/177256172-ddfc3bc7-cc97-4dbf-b33c-9a6045c3d67b.mp4

Make 3D rollercoasters out of rayshader and rayrender scenes, without
leaving R! Use rayrender’s interactive renderer to fly through your
scene or dataviz and add a fun ride to your scene.

## Installation

You can install rayrollercoaster via github (Note: installation requires
X11/XQuartz on Linux/macOS):

``` r
remotes::install_github("tylermorganwall/rayrollercoaster")
```

## Example

You can make roller coasters via a pre-made rayrender scene, or by
calling `generate_rayshader_coaster()`. Here, we load a scene up using
rayshader.

``` r
library(rayrollercoaster)
library(rayshader)

volcano |>
  sphere_shade() |>
  plot_3d(volcano,fov=90)

#Now, let's fly through a series of points to build the rollercoaster.
#You can pass arguments to `render_scene()` via `...`.
#This function doesn't return anything, but saves the scene with the rollercoaster and the
#motion information internally. This extracts the scene from `rayshader` directly from `rgl`.
#Here, we turn off the lighting and use ambient lighting only
generate_rayshader_coaster(light=FALSE, ambient_light=T)

#We can also add extra lights by passing arguments via `...` to `rayshader::render_highquality()`
generate_rayshader_coaster(lightdirection=c(315,45),lightcolor=c("orange","white"))

#We can pass an HDR file for high quality realistic lighting (which we will use in the animation 
#as well by passing `environment_light = tempfilehdr` to `animate_rollercoaster()`:
tempfilehdr = tempfile(fileext = ".hdr")
download.file("https://www.tylermw.com/data/venice_sunset_2k.hdr",tempfilehdr)
generate_rayshader_coaster(light=FALSE, environment_light=tempfilehdr)

#We now can create our animation using `animate_rollercoaster()`. This extracts the information
#(scene and motion) in the previous step. If no filename is given,
#it will simply preview the animation in the interactive screen, without saving anything.
#Set samples to `1` to render a quick preview.
animate_rollercoaster(samples=1, width=200,height=200, environment_light=tempfilehdr)

#Increase the number of samples and resolution for a high quality animation (but longer render):
animate_rollercoaster(samples=128, width=800, height=800, environment_light=tempfilehdr)

#Alternatively, get the scene and motion data yourself and call `rayrender::render_animation()`
#directly.
scene = get_rayrender_scene()
motion = get_rayrender_motion()

render_animation(scene, motion, samples=1, sample_method="sobol_blue",environment_light=tempfilehdr)
```

You can also build rollercoasters directly from rayrender scenes:

``` r
library(rayrender)
scene = generate_ground(material=diffuse(checkercolor="grey20")) |>
  add_object(obj_model(r_obj(),y=-1,x=1, material=diffuse(color = "purple"))) |>
  add_object(sphere(y=-0,x=-1,radius=1, material=dielectric())) |>
  add_object(sphere(y=5,x=5,z=5,material=light(intensity = 50))) |>
  add_object(sphere(y=5,x=-5,z=5,material=light(color="orange",intensity = 50)))
render_scene(scene, fov=40,sample_method="sobol_blue")

#Now, let's fly through a series of points to build the rollercoaster.
#You can pass arguments to `render_scene()` via `...`.
#This function doesn't return anything, but saves the scene with the rollercoaster and the
#motion information internally.
generate_rayrender_coaster(scene, fov = 120)

#We now can create our animation using `animate_rollercoaster()`. This extracts the information
#(scene and motion) in the previous step. If no filename is given,
#it will simply preview the animation in the interactive screen, without saving anything.
#Set samples to `1` to render a quick preview.
animate_rollercoaster(samples=1)

#Increase the number of samples and resolution for a high quality animation (but longer render):
animate_rollercoaster(samples=128, width=800, height=800)

#Set the FOV to 360 to render a movie for VR headsets
#Use the Spatial Media Metadata Injector app from google to add the metadata to render in VR on 
#youtube (available here: https://github.com/google/spatial-media/releases)
animate_rollercoaster(samples=128, width=800, height=800, fov=360, filename="video360.mp4")
```
