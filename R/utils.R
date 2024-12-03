
#' @noRd
entity_recode <- function(x) {
  data.table::fcase(x == "1", "I", x == "2", "O", default = x)
}

#' @noRd
make_case <- function(x, y) {
  data.table::fcase(
    codex::not_na(x) & codex::not_na(y) & x == y, y,
    codex::not_na(x) & codex::not_na(y) & x != y, paste0(x, ", ", y),
    codex::not_na(x) & codex::na(y), x,
    codex::na(x) & codex::not_na(y), y,
    default = NA_character_)
}

#' @noRd
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

#' @noRd
phone_eq_10 <- function(x) {
  paste0(
    "(",
    substr(x, 1, 3),
    ")",
    substr(x, 4, 6),
    "-",
    substr(x, 7, 10)
    )
}

#' @noRd
phone_gt_10 <- function(x) {
  paste0(
    "(",
    substr(x, 1, 3),
    ")",
    substr(x, 4, 6),
    "-",
    substr(x, 7, 10),
    ", (",
    substr(x, 13, 15),
    ")-",
    substr(x, 16, 18),
    "-",
    substr(x, 19, 22)
    )
}

#' @noRd
make_phone <- function(x) {
  data.table::fcase(
    codex::not_na(x) & codex::sf_nchar(x) == 10, phone_eq_10(x),
    codex::not_na(x) & codex::sf_nchar(x) > 10, phone_gt_10(x),
    default = x)
}

#' @noRd
as_date <- function(x, fmt = "%m/%d/%Y") as.Date(x, format = fmt)

#' @noRd
remove_periods <- function(x) codex::sf_remove(x, "\\.")

#' @noRd
wrap <- function(x, left, right) paste0(left, x, right)

