options <- list(
	global_dir = if (!internal) {
	    file.path(Sys.getenv("HOME"), "optionr")
	} else {
	    file.path(Sys.getenv("HOME"), "optionr", basename(getwd()))
	},
	wd = getwd(),
	runtime_mode = "dev",
	lib = .libPaths()[1]
)
