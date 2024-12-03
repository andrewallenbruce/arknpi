#' Recode NPI entity codes
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @examples
#' entity_recode(c("1", "2", "3"))
#'
#' @autoglobal
#'
#' @export
entity_recode <- function(x) {
  data.table::fcase(x == "1", "I", x == "2", "O", default = NA_character_)
}

#' Make a case statement
#'
#' @param x A character vector
#'
#' @param y A character vector
#'
#' @returns A character vector
#'
#' @examples
#' make_case(c("1", "1", "3", NA), c("1", "2", NA, "4"))
#'
#' @autoglobal
#'
#' @export
make_case <- function(x, y) {
  data.table::fcase(
    codex::not_na(x) & codex::not_na(y) & x == y, y,
    codex::not_na(x) & codex::not_na(y) & x != y, paste0(x, ", ", y),
    codex::not_na(x) & codex::na(y), x,
    codex::na(x) & codex::not_na(y), y,
    default = NA_character_)
}

#' Format a 9-digit zip code
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @examples
#' make_zip(c("31605", "316051234", NA))
#'
#' @autoglobal
#'
#' @export
make_zip <- function(x) {
  ifelse(
    codex::not_na(x) & codex::sf_nchar(x) == 9,
    paste0(
      substr(x, 1, 5),
      "-",
      substr(x, 6, 9)
      ),
    x)
}

#' Format a 10-digit phone number
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @examples
#' phone_eq_10(c("9122474701", NA))
#'
#' @autoglobal
#'
#' @export
phone_eq_10 <- function(x) {

  x <- x[codex::not_na(x)]

  paste0(
    "(",
    substr(x, 1, 3),
    ") ",
    substr(x, 4, 6),
    "-",
    substr(x, 7, 10)
    )
}

#' Format a 10-digit phone number
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @examples
#' phone_gt_10(c("9122474701  9122474701", NA))
#'
#' @autoglobal
#'
#' @export
phone_gt_10 <- function(x) {

  x <- x[codex::not_na(x)]

  paste0(
    "(",
    substr(x, 1, 3),
    ") ",
    substr(x, 4, 6),
    "-",
    substr(x, 7, 10),
    ", (",
    substr(x, 13, 15),
    ") ",
    substr(x, 16, 18),
    "-",
    substr(x, 19, 22)
    )
}

#' Format a 10-digit phone number
#'
#' @param x A character vector
#'
#' @returns A character vector
#'
#' @examples
#' make_phone("9122474701")
#'
#' make_phone("9122474701  9122474701")
#'
#' @autoglobal
#'
#' @export
make_phone <- function(x) {
  data.table::fcase(
    codex::not_na(x) & codex::sf_nchar(x) == 10, phone_eq_10(x),
    codex::not_na(x) & codex::sf_nchar(x) > 10, phone_gt_10(x),
    default = x)
}

#' Format a Date
#'
#' @param x character string
#'
#' @param fmt string format, e.g. '%m/%d/%Y'
#'
#' @returns date string
#'
#' @examples
#' as_date("12/31/2021")
#'
#' @autoglobal
#'
#' @export
as_date <- function(x, fmt = "%m/%d/%Y") as.Date(x, format = fmt)

#' Remove periods
#'
#' @param x character string
#'
#' @returns character string
#'
#' @examples
#' remove_periods("M.D..")
#'
#' @autoglobal
#'
#' @export
remove_periods <- function(x) codex::sf_remove(x, "\\.")

#' Wrap a string
#'
#' @param x character string
#'
#' @param left character string
#'
#' @param right character string
#'
#' @returns character string
#'
#' @examples
#' wrap("912", "(", ")")
#'
#' @autoglobal
#'
#' @export
wrap <- function(x, left, right) paste0(left, x, right)

