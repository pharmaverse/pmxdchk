# Package state: the check registry lives here, populated at load time.
the <- new.env(parent = emptyenv())
the$registry <- list()

.onLoad <- function(libname, pkgname) {
  register_builtin_checks()
}
