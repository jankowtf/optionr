optionr
======

Management of customized (and nested) project/package options

## Installation 

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/reactr")
devtools::install_github("Rappster/optionr")
require("optionr")
```
## Purpose 

The package provides an extendable interface to conventiently create customized project and package options. The package allows to specify and retrieve options based on path-like names/identifiers (e.g. `output/type/pdf = TRUE` will be translated into the following nested environment structure: `output$type$pdf` with the value being `TRUE`). 

Also, it allows to specify reactive options, i.e. options that are
dynamically linke to other options and thus automatically stay synced.

### Vignettes

None

----------

## Examples 

The examples all assume that you your working directory points to a valid package project.

### Option container

By default, an option container (i.e. an `environment`) is assigned to the name
that corresponds to the name of the (package) project (determined via 
`devtools::as.package(".")$package`).

This option container has three elements:

1. `options`: used for storing actual project options via `setProjectOption()`
2. `.meta`: used for storing desired meta information via `setMetaValue()`
2. `.registry`: used for storing desired registry information via `setRegistryValue()`

```
container <- getOptionContainer()
ls(container, all.names = TRUE)
```

### Project options

Simple name/ID:

```
setProjectOption(id = "a", value = TRUE)
getProjectOption(id = "a")
```

Path-like name/ID:

```
setProjectOption(id = "b/c/d", value = 10, gap = TRUE)
getProjectOption(id = "b")
getProjectOption(id = "b/c")
getProjectOption(id = "b/c/d")
```

Typed options:

```
setProjectOption(id = "a", value = "abc", typed = TRUE, strict_set = 2)
setProjectOption(id = "a", value = 1:3)
getProjectOption(id = "a")
setProjectOption(id = "a", value = "def")
getProjectOption(id = "a")
```

Reactive options:

```
setProjectOption(id = "x_1/a", value = 10, gap = TRUE, reactive = TRUE)
setProjectOption(
  id = "x_2/a", 
  value = reactiveOption(getProjectOption("x_1/a") * 10), 
  gap = TRUE
)
getProjectOption(id = "x_1/a")
getProjectOption(id = "x_2/a")

setProjectOption(id = "x_1/a", value = 50)
getProjectOption(id = "x_2/a")
```

### Meta values 

```
setMetaValue(id = "a/b/c", value = 10, gap = TRUE)
getMetaValue(id = "a/b/c")
identical(getMetaValue(id = "a/b/c"), container$.meta$a$b$c)
```

### Registry values

```
setRegistryValue(id = "a/b/c", value = 10, gap = TRUE)
getRegistryValue(id = "a/b/c")
identical(getRegistryValue(id = "a/b/c"), container$.registry$a$b$c)
```

### Free options

You can also set options to whatever environment you like

```
setFreeOption("directly/below/option/container", 1:3, gap = TRUE)
getFreeOption("directly/below/option/container")
ls(container, all.names = TRUE)
container$directly$below$option$container
```
