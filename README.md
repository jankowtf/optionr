optionr
======

Management of customized (and nested) project/package options

## Installation 

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/nestr")
devtools::install_github("Rappster/optionr")
require("optionr")
```
## Purpose 

The package provides an extendable interface to conveniently create customized project/package options. Options can be specified and retrieved based on path-like names/identifiers (e.g. `output/type/pdf = TRUE` will be translated into the following nested environment structure: `output$type$pdf` with the value being `TRUE`).

Also, it allows to specify reactive options, i.e. options that are dynamically linked to other options and thus automatically stay synced.

## Interface character

The package aims at providing a clearly structured and easy to use interface for other programmers. For almost all of the function custom S4 methods can be specified.

## Vignettes

None so fare

----------

## Examples

All examples assume that your working directory points to a directory containing an R package project. Running `devtools::as.package(".")$package` should result in the package/project name.

### Empty option container

By default, a so called *option container* (which simply an empty `environment`) is created and assigned to the name corresponding to the package/project name (determined via `devtools::as.package(".")$package`). 

The function that takes care of this is `ensureOptionContainer()`.
Assuming that your package/project has name `my.package`, `ensureOptionContainer()` assigns an empty `environment` to the R option `.my.package` (or `my.package` if `hidden = FALSE`). You can retrieve the option container via the convenience function `getOptionContainer()` or via `getOption(".my.package")`.

For package `optionr` itself this would look like this:

```
## Hidden //
ensureOptionContainer(overwrite = TRUE)
".optionr" %in% names(options())
## --> only TRUE for package `optionr`; use your package name instead

## Not hidden //
ensureOptionContainer(overwrite = TRUE, hidden = FALSE)
"optionr" %in% names(options())
## --> only TRUE for package `optionr`; use your package name instead

## Retrieve container //
container <- getOptionContainer()
ls(container, all.names = TRUE)
## --> empty
```

### Pre-configured option container

A slightly more pre-configured version of such an option container is created/ensured via `initializeOptionContainer()` which is the preferred top-level function to making sure an option container exists.

The container already has three elements (which are in turn `environment`s) for reasons of a better encapsulation of option container components:

1. `options`: 

    Initialized via `initializeProjectOptions()` and used for storing and retrieving actual project options via `setProjectOption()` and `getProjectOption()`.
    
2. `.meta`: 
    
    Initialized via `initializeMeta()` and used for storing and retrieving certain meta information via `setMetaValue()` and `getMetaValue()` (if desired; can be omitted by changing the value of `components` in call to `initializeOptionContainer()`).
    
3. `.registry`: 

    Initialized via `initializeRegistry()` and used for storing and retrieving certain registry information via `setRegistryValue()` and `getRegistryValue()` (if desired; can be omitted by changing the value of `components` in call to `initializeOptionContainer()`).

### Use as interface 

For each of the functions mentioned in this chapter custom S4 methods can be defined. Thus, the entire initialization process can be fully customized in a clean and extendable way.

### Example

For package `optionr` itself this would look like this:

```
## Initialize //
container <- initializeOptionContainer(overwrite = TRUE)

## Inspect //
ls(container, all.names = TRUE)
## --> pre-configured
```

-----

## Actual project options 

The following functions are convenience wrappers around `setAnywhereOption()` and `getAnywhereOption()` (see section below).

### Simple name/ID

```
setProjectOption(id = "x_1", value = TRUE)
existsProjectOption(id = "x_1")
getProjectOption(id = "x_1")
rmProjectOption(id = "x_1")
existsProjectOption(id = "x_1")
## --> removed
```

### Path-like name/ID resulting in nested/tree-like option structures 

```
setProjectOption(id = "x_2/a/b", value = 10)
getProjectOption(id = "x_2")
## --> branch
getProjectOption(id = "x_2/a")
## --> branch
getProjectOption(id = "x_2/a/b")
## --> leaf, i.e. the actual option and its value
```

### Typed options

Implicit typing by remembering the type/class when setting an option:

```
setProjectOption(id = "a", value = "abc", typed = TRUE, strict = 2)
## --> `strict` controls how strict you would like to be with respect to 
## what happens when trying to assign values of the wrong type/class

setProjectOption(id = "a", value = 1:3)
getProjectOption(id = "a")
## --> value is still "abc"
setProjectOption(id = "a", value = "def")
getProjectOption(id = "a")
## --> as "def" has the correct type/class, the option value is changed
```

### Reactive options

```
setProjectOption(id = "dirs/wd", value = getwd(), reactive = TRUE)
setProjectOption(
  id = "dirs/reactive_subdir", 
  value = reactiveOption(file.path(getProjectOption("dirs/wd"), "my_directory"))
)
getProjectOption(id = "dirs/wd")
getProjectOption(id = "dirs/reactive_subdir")

setProjectOption(id = "dirs/wd", value = "c:/temp")
getProjectOption(id = "dirs/reactive_subdir")
```

-----

## Meta values 

Sometimes I found it usefull to have a central place for keeping certain meta information. 

By default, they live in environment `.meta` below the root level of the option container. The existence of this environment can be suppressed by specifying `components` accordingly in the call to `initializeOptionContainer()`.

Also see the function controlling the initialization: `initializeMeta()`

The following functions are convenience wrappers around `setAnywhereOption()` and `getAnywhereOption()` (see section below).

```
setMetaValue(id = "a/b/c", value = 10)
existsMetaValue(id = "a/b/c")
getMetaValue(id = "a/b/c")
identical(getMetaValue(id = "a/b/c"), container$.meta$a$b$c)
rmMetaValue(id = "a/b/c")
existsMetaValue(id = "a/b/c")
```

-----

## Registry values

Sometimes I found it usefull to have a central place for keeping certain registry information (e.g. see package [optionr](https://github.com/Rappster/reactr)). 

By default, they live in environment `.registry` below the root level of the option container. The existence of this environment can be suppressed by specifying `components` accordingly in the call to `initializeOptionContainer()`.

Also see the function controlling the initialization: `initializeRegistry()`

The following functions are convenience wrappers around `setAnywhereOption()` and `getAnywhereOption()` (see section below).

```
setRegistryValue(id = "a/b/c", value = 10)
existsRegistryValue(id = "a/b/c")
getRegistryValue(id = "a/b/c")
identical(getRegistryValue(id = "a/b/c"), container$.registry$a$b$c)
rmRegistryValue(id = "a/b/c")
existsRegistryValue(id = "a/b/c")
```

-----

## Anywhere options

The actual workhorse functions that are called by the convenvience function described above are `setAnywhereOption()` and `getAnywhereOption()`. 

These let you set (retrieve) options (from) anywhere in an option container. 

```
setAnywhereOption("directly/below/option/container", 1:3)
existsAnywhereOption("directly/below/option/container")
getAnywhereOption("directly/below/option/container")
ls(container, all.names = TRUE)
container$directly$below$option$container
rmAnywhereOption("directly/below/option/container")
existsAnywhereOption("directly/below/option/container")
```
