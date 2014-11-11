options <- list(
	ns = libr::asPackage(".")$package,
	is_internal = TRUE,
	global_dir = if (getNsRappOption(id = "is_internal")) {
	    file.path(getNsRappOption(ns = getRappOption(".rte/ns_prime"), 
	        id = "global_dir"), "ns", libr::asPackage(".")$package)
	} else {
	    file.path(Sys.getenv("HOME"), "rapp/ns", libr::asPackage(".")$package)
	},
	github_name = NA_character_,
	option_1 = "your option value here (can be any R object)",
	option_2 = "your option value here (can be any R object)",
	option_3 = "your option value here (can be any R object)"
)
