context("r6")

# Method documentation ---------------------------------------------------------

A <- R6::R6Class(
  classname = "A",
  public = list(
    f = function() {
      "This function has a docstring"
      1
    },
    g = function() {
      "This function doesn't"
    }
  ),
  active = list(
    x2 = function(value) {
      if (missing(value)) return(self$x * 2)
      else self$x <- value/2
    },
    rand = function() rnorm(1)
  ))

B <- R6::R6Class(
  classname = "B",
  inherit = A,
  public = list(
    f1 = function() {
      "This function has a docstring"
      1
    },
    g1 = function() {
      "This function doesn't"
    }
  ))

test_that("r6_public_methods works", {
  # the clone method from the archetypal R6 generator.
  # NB: not testing equivalance of names because A and B will inherit at least
  expect_true(all(c("f", "g") %in% names(r6_public_methods(A))))
  expect_true(all(c("f", "g", "f1", "g1") %in% names(r6_public_methods(B))))
})

test_that("r6_active_bindings works", {
  expect_true(all(c("x2", "rand") %in% names(r6_active_bindings(A))))
  expect_true(all(c("x2", "rand") %in% names(r6_active_bindings(B))))
})


test_that("R6 methods included in own section", {
  library(R6)
  out <- roc_proc_text(rd_roclet(), "
    #' Class ABC
    R6Class('ABC', public = list(
    f = function() {
    'This function has a docstring'
    1
    }
    ))
    ")[[1]]
  methods <- get_tag(out, "r6methods")$values
  expect_equal(names(methods), "f()")
  expect_match(methods[[1]], "This function has a docstring")
})

rm(B)
rm(A)
