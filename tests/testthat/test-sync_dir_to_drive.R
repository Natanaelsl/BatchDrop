library(testthat)

test_that("Fails if the local directory does not exist", {
  expect_error(
    sync_dir_to_drive(
      "pasta_inexistente_12345",
      "id_ficticio",
      verbose = FALSE
    ),
    "The provided local directory does not exist or is inaccessible"
  )
})

test_that("Fails if the root folder ID is invalid or missing", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(
    sync_dir_to_drive(
      pasta_local = temp_dir,
      verbose = FALSE
    ),
    "The 'id_pasta_raiz_drive' parameter must be provided as a valid character string"
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("Silently returns FALSE if the directory is empty", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  resultado <- sync_dir_to_drive(
    temp_dir,
    "id_ficticio",
    verbose = FALSE
  )

  expect_false(resultado)

  unlink(temp_dir, recursive = TRUE)
})

test_that("Successfully uploads by mocking the API", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  cat("test content", file = file.path(temp_dir, "arq.txt"))

  testthat::local_mocked_bindings(
    drive_ls = function(...) data.frame(name = character(), id = character()),
    drive_mkdir = function(...) list(id = "id_nova_subpasta"),
    drive_upload = function(...) TRUE,
    as_id = function(x) x,
    .package = "googledrive"
  )

  resultado <- sync_dir_to_drive(
    temp_dir,
    "id_raiz",
    verbose = FALSE
  )

  expect_true(resultado)

  unlink(temp_dir, recursive = TRUE)
})
