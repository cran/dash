test_that("invalid attributes trigger an error", {
  external_stylesheets <- list(
                            list(
                              href="https://codepen.io/chriddyp/pen/bWLwgP.css",
                              foo="somedata",
                              bar="moredata"
                              )
                            )

  external_scripts <- list(
                        "https://www.google-analytics.com/analytics.js",
                        list(
                          src = "https://cdn.polyfill.io/v2/polyfill.min.js"
                        ),
                        list(
                          src = "https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.10/lodash.core.js",
                          integrity = "sha256-Qqd/EfdABZUcAxjOkMi8eGEivtdTkh3b65xCZL4qAQA=",
                          baz = "anonymous"
                        )
                      )

  expect_error(assertValidExternals(external_scripts, external_stylesheets),
    "The following script or stylesheet attributes are invalid: baz, foo, bar.")
})

test_that("not passing named attributes triggers an error", {
  external_stylesheets <- list(
                            list(
                              href="https://codepen.io/chriddyp/pen/bWLwgP.css",
                              foo="somedata",
                              "moredata"
                              )
                            )

  external_scripts <- list()

  expect_error(assertValidExternals(external_scripts, external_stylesheets),
    "Please verify that all attributes are named elements when specifying URLs for scripts and stylesheets.")
})


test_that("not passing named attributes triggers an error", {
  external_stylesheets = list(
                            list(
                              href="https://codepen.io/chriddyp/pen/bWLwgP.css",
                              "somedata"
                              )
                            )

  expect_error(app <- Dash$new(external_stylesheets=external_stylesheets),
    "Please verify that all attributes are named elements when specifying URLs for scripts and stylesheets.")
  expect_error(app <- Dash$new(serve_locally=FALSE, external_stylesheets=external_stylesheets),
    "Please verify that all attributes are named elements when specifying URLs for scripts and stylesheets.")
})

test_that("passing a list with no href/src fails", {
  stylesheet_pattern <- '^.*<link href="(https.*)">.*$'
  script_pattern <- '^.*<script src="(https.*)">.*$'

  expect_error(app <- Dash$new(external_stylesheets = list(
                      href="https://codepen.io/chriddyp/pen/bWLwgP.css",
                            list(
                              integrity="somedata",
                              crossorigin="moredata"
                              )
                            )
                  ),
                  "A valid URL must be included with every entry in external_stylesheets. Please sure no 'href' entries are missing or malformed.")

  expect_error(app <- Dash$new(serve_locally=FALSE, external_stylesheets = list(
                      href="https://codepen.io/chriddyp/pen/bWLwgP.css",
                            list(
                              integrity="somedata",
                              crossorigin="moredata"
                              )
                            )
                  ),
                  "A valid URL must be included with every entry in external_stylesheets. Please sure no 'href' entries are missing or malformed.")

  expect_error(app <- Dash$new(external_scripts = list(
                      href="https://rivetscriptemporium.com/test.js",
                            list(
                              integrity="somedata",
                              crossorigin="moredata"
                              )
                            )
                  ),
                  "A valid URL must be included with every entry in external_scripts. Please sure no 'src' entries are missing or malformed.")

  expect_error(app <- Dash$new(serve_locally=FALSE, external_scripts = list(
                      href="https://rivetscriptemporium.com/test.js",
                            list(
                              integrity="somedata",
                              crossorigin="moredata"
                              )
                            )
                  ),
                  "A valid URL must be included with every entry in external_scripts. Please sure no 'src' entries are missing or malformed.")
})
