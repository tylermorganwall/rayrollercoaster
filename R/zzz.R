ray_environment = new.env(parent = emptyenv())

.onLoad = function(libname, pkgname) {
  assign("motion_info", data.frame(), envir = ray_environment)
  assign("scene_info", data.frame(), envir = ray_environment)
}
