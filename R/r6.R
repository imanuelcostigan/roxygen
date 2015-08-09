# Extract only public methods from an R6 definition, returning a list of
# "objects". Only public, because users shouldn't have to worry about private
# methods. If they do, then this suggests the private method should be made
# public
r6_public_methods <- function(obj) {
  stopifnot(is(obj, "R6ClassGenerator"))
  methods <- Map(add_r6_metadata, obj$public_methods, names(obj$public_methods),
    obj$classname)
  parent <- obj$get_inherit()
  while (!is.null(parent)) {
    inherited_methods <- Map(add_r6_metadata, parent$public_methods,
      names(parent$public_methods), parent$classname)
    methods <- c(methods, inherited_methods)
    parent <- parent$get_inherit()
  }
  lapply(methods, object)
}

r6_active_bindings <- function(obj) {
  stopifnot(is(obj, "R6ClassGenerator"))
  bindings <- lapply(obj$active, add_r6_metadata, names(obj$active),
    obj$classname)
  parent <- obj$get_inherit()
  while (!is.null(parent)) {
    inherited_bindings <- lapply(parent$active, add_r6_metadata,
      names(parent$active), parent$classname)
    bindings <- c(bindings, inherited_bindings)
    parent <- parent$get_inherit()
  }
  if (isTRUE(length(bindings) == 0)) {
    return(NULL)
  } else{
    return(lapply(bindings, object))
  }
}

add_r6_metadata <- function(val, name, class) {
  if (!is.function(val)) return(val)
  class(val) <- c("r6method", "function")
  attr(val, "r6class") <- class
  attr(val, "r6method") <- name
  val
}
