#' Package Setup with Usethis
#'
#' Just a wrapper around the usethis package
#' with some reasonable defaults
package_setup <- function(name = "mypkg") {

  # Create a new package -------------------------------------------------
  file.path(name) %>% usethis::create_package()
  #> Changing active project to mypkg
  #> ✔ Creating 'R/'
  #> ✔ Creating 'man/'
  #> ✔ Writing 'DESCRIPTION'
  #> ✔ Writing 'NAMESPACE'

  # Modify the description ----------------------------------------------
  #usethis::use_gpl3_license(name)
  #usethis::use_mit_license(name)
  #usethis::use_apl2_license(name)
  usethis::use_cc0_license(name)
  #> ✔ Setting License field in DESCRIPTION to 'MIT + file LICENSE'
  #> ✔ Writing 'LICENSE.md'
  #> ✔ Adding '^LICENSE\\.md$' to '.Rbuildignore'
  #> ✔ Writing 'LICENSE'

  usethis::use_package("SomaObjects", "Suggests")
  #> ✔ Adding 'SomaObjects' to Suggests field in DESCRIPTION
  #> ● Use `requireNamespace("MASS", quietly = TRUE)` to test if package is installed
  #> ● Then use `MASS::fun()` to refer to functions.

  #use_dev_package("callr")
  #> ✔ Adding 'callr' to Imports field in DESCRIPTION
  #> ● Refer to functions with `callr::fun()`
  #> ✔ Adding 'r-lib/callr' to DESCRIPTION Remotes

  # Set up various packages ---------------------------------------------
  usethis::use_roxygen_md()
  #> ✔ Setting Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
  #> ✔ Setting RoxygenNote field in DESCRIPTION to '6.0.1.9000'
  #> ● Re-document

  # Set up other files -------------------------------------------------
  usethis::use_readme_md()
  #> ✔ Writing 'README.md'

  usethis::use_test("my-test")
  #> ✔ Adding 'testthat' to Suggests field in DESCRIPTION
  #> ✔ Creating 'tests/testthat/'
  #> ✔ Writing 'tests/testthat.R'
  #> ✔ Writing 'tests/testthat/test-my-test.R'
  #> ● Edit 'test-my-test.R'

  x <- 1
  y <- 2
  usethis::use_data(x, y)
  #> ✔ Creating 'data/'
  #> ✔ Saving x to data/x.rda
  #> ✔ Saving y to data/y.rda

  # Use git ------------------------------------------------------------
  usethis::use_git()
  #> ✔ Initialising Git repo
  #> ✔ Adding '.Rhistory', '.RData', '.Rproj.user' to './.gitignore'
  #> ✔ Adding files and committing

}
