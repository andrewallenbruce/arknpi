pin_update <- function(x, name, title, description) {
  board <- pins::board_folder(
    here::here("inst/extdata/pins")
  )

  board |>
    pins::pin_write(
      x,
      name = name,
      title = title,
      description = description,
      type = "rds",
      force_identical_write = TRUE
    )

  board |> pins::write_board_manifest()
}

delete_pins <- function(pin_names) {
  board <- pins::board_folder(
    here::here("inst/extdata/pins")
  )

  pins::pin_delete(board, names = pin_names)
}
#
# x <- list_pins()
# x <- purrr::map(x, get_pin) |>
#   rlang::set_names(x)
#
# pin_update(
#   x$deactivated,
#   name = "deactivated",
#   title = "Deactivated NPIs",
#   description = "Deactivated NPIs and Date of Deactivation"
# )
#
# pin_update(
#   x$`wk_2024_01_01-07`,
#   name = "wk_2024_01_01-07",
#   title = "NPPES 2024, Jan 1 - Jan 7",
#   description = "NPPES NPI Registry, Week of January 1 - January 7, 2024"
# )
#
# pin_update(
#   x$`wk_2024_01_08-14`,
#   name = "wk_2024_01_08-14",
#   title = "NPPES 2024, Jan 8 - Jan 14",
#   description = "NPPES NPI Registry, Week of January 8 - January 14, 2024"
# )
#
# pin_update(
#   x$`wk_2024_01_15-21`,
#   name = "wk_2024_01_15-21",
#   title = "NPPES 2024, Jan 15 - Jan 21",
#   description = "NPPES NPI Registry, Week of January 15 - January 21, 2024"
# )
#
# pin_update(
#   x$`wk_2024_01_22-28`,
#   name = "wk_2024_01_22-28",
#   title = "NPPES 2024, Jan 22 - Jan 28",
#   description = "NPPES NPI Registry, Week of January 22 - January 28, 2024"
# )
#
# pin_update(
#   x$`wk_2024_01_29-04`,
#   name = "wk_2024_01_29-04",
#   title = "NPPES 2024, Jan 29 - Feb 4",
#   description = "NPPES NPI Registry, Week of January 29 - February 4, 2024"
# )
