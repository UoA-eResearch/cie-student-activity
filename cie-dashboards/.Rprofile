project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
Sys.setenv(RENV_PROJECT = project_root)

site_library <- "/usr/local/lib/R/site-library"
if (dir.exists(site_library)) {
	.libPaths(c(site_library, .libPaths()))
}
