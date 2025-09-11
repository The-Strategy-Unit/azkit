test_that("function behaves as expected", {
  skip_on_ci()
  cont <- expect_no_error(get_container("supporting-data"))
  res <- expect_no_error(AzureStor::list_blobs(cont))
  expect_equal(nrow(res), 15L)
  res2 <- dplyr::filter(res, !dplyr::if_any("isdir"))
  expect_equal(nrow(res2), 14L)
  file_ext <- NULL
  file_ext <- file_ext %||% ".*"
  res3 <- res2 |>
    dplyr::filter(dplyr::if_any("name", \(x) gregg(x, "\\.{file_ext}$")))
  expect_equal(nrow(res2), nrow(res3)) # because nothing filtered out yet
  file_ext <- "json"
  res4 <- res2 |>
    dplyr::filter(dplyr::if_any("name", \(x) gregg(x, "\\.{file_ext}$")))
  expect_equal(nrow(res4), 6L)
})

test_that("we can evolve list_files()", {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  # only run the test if this variable is set (ie locally, but not on GitHub)
  if (nzchar(endpoint_uri)) {
    cont <- expect_no_error(get_container("supporting-data"))

    list_files <- \(container) AzureStor::list_blobs(container)
    expect_equal(nrow(expect_no_error(list_files(cont))), 15L)

    list_files <- function(container, recursive = TRUE) {
      AzureStor::list_blobs(container, recursive = recursive) |>
        dplyr::filter(!dplyr::if_any("isdir")) |>
        dplyr::pull("name")
    }
    expect_length(expect_no_error(list_files(cont)), 14L)

    list_files <- function(container, file_ext = ".*", recursive = TRUE) {
      AzureStor::list_blobs(container, recursive = recursive) |>
        dplyr::filter(
          !dplyr::if_any("isdir") &
            dplyr::if_any("name", \(x) gregg(x, "\\.{file_ext}$"))
        ) |>
        dplyr::pull("name")
    }
    expect_length(expect_no_error(list_files(cont, "json", FALSE)), 6L)
    expect_true(rlang::is_bare_character(list_files(cont, "json")))

    list_files <- function(container, path = "", ext = ".*", recursive = TRUE) {
      stopifnot("path not found" = AzureStor::blob_dir_exists(container, path))
      tbl <- AzureStor::list_blobs(container, dir = path, recursive = recursive)
      if (nrow(tbl) == 0) {
        return(character(0))
      } else {
        tbl |>
          dplyr::filter(
            !dplyr::if_any("isdir") &
              dplyr::if_any("name", \(x) gregg(x, "\\.{ext}$"))
          ) |>
          dplyr::pull("name")
      }
    }
    expect_length(list_files(cont), 14L)
    expect_length(list_files(cont, ext = "json"), 6L)
    # root folder can be specified as "" or "/", it doesn't matter
    expect_length(list_files(cont, "/", "json"), 6L)
    # no files with this extension present - but should not error
    expect_length(expect_no_error(list_files(cont, ext = "xlsx")), 0L)

    expect_no_error(list_files(cont, "QA")) # folder QA should exist
    expect_no_error(list_files(cont, "/QA")) # /QA also works
    expect_length(list_files(cont, "QA"), 0)
    # folder does not exist
    expect_error(list_files(cont, "BLAH"), "path not found")
  }
})


test_that("edited path starts with '/' unless it's the root folder", {
  fix_path <- \(path) sub("^/+$", "", sub("^([^/])(.*)", "/\\1\\2", path))
  expect_equal(fix_path("/"), "")
  expect_equal(fix_path(""), "")
  expect_equal(fix_path("/QA"), "/QA")
  expect_equal(fix_path("QA"), "/QA")
})
