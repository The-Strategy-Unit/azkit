test_that("edited path starts with '/' unless it's the root folder", {
  fix_path <- \(path) sub("^/+$", "", sub("^([^/])(.*)", "/\\1\\2", path))
  expect_equal(fix_path("/"), "")
  expect_equal(fix_path(""), "")
  expect_equal(fix_path("/QA"), "/QA")
  expect_equal(fix_path("QA"), "/QA")
})
