context("Directory Creation")

main <- "./testing"

test_that("a directory folder structure can be created", {

  expect_message(create_dir(main = main))

})

