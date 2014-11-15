# CHANGES IN optionr VERSION 0.3.1

## NEW FEATURES

## BUG FIXES

- fixed: #1 
  Bug in `setAnywhereOption()` related to a trailing comma.
  Also see this [Stackoverflow thread](http://stackoverflow.com/questions/26940474/lexical-scoping-calling-stack-issue-r-fails-to-recognize-an-arguments-defaul)

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN optionr VERSION 0.3

## NEW FEATURES

- added: `exists*`functions

## BUG FIXES

## MAJOR CHANGES

- modified imports: removed `reactr`, added `nestr` (which imports `reactr`)
- better encapsulation of management of nested object:
  - `set*` functions now are essentially only wrappers around `nestr::set*()`
  - `get*` functions now are essentially only wrappers around `nestr::get*()`
  - `rm*` functions now are essentially only wrappers around `nestr::rm*()`
  - `exists*` functions now are essentially only wrappers around `nestr::exists*()`
  
## MINOR CHANGES

- modified: doc in `setAnywhereOption()`
- modified: `README`

## MISC

- triggered: workaround necessary in `reactr::setShinyReactive()` (`lazy` problem; see issue #28)

-----

# CHANGES IN optionr VERSION 0.2

## NEW FEATURES

- removal functions

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

- auto-reset for reactive observers
- updated documentation

## MISC

-----

# CHANGES IN optionr VERSION 0.1

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- initial version

-----


