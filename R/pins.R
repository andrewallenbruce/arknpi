#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mount_board <- function(source = c("local", "remote")) {

  gh_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)

  source <- match.arg(source)

  switch(
    source,
    local = pins::board_folder(fs::path_package("extdata/pins", package = "arknpi")),
    remote = pins::board_url(gh_raw("andrewallenbruce/arknpi/master/inst/extdata/pins/")),
    stop("Invalid source")
  )
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @param ... additional arguments passed to `mount_board()`
#'
#' @returns `<tibble>` or `<data.frame>` of selected pin
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- match.arg(pin, list_pins())

  pins::pin_read(board, pin)
}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<chr>` vector of [pins][pins::pins-package] names
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)
}
