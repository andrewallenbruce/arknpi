
#' @noRd
make_case <- function(x, y) {
  data.table::fcase(
    codex::not_na(x) & codex::not_na(y) & x == y, stringr::str_glue("{y}") |> as.character(),
    codex::not_na(x) & codex::not_na(y) & x != y, stringr::str_glue("{x}, {y}") |> as.character(),
    codex::not_na(x) & codex::na(y), stringr::str_glue("{x}") |> as.character(),
    codex::na(x) & codex::not_na(y), stringr::str_glue("{y}") |> as.character(),
    default = NA_character_)
}

#' @noRd
make_zip <- function(x) {
  ifelse(
    codex::not_na(x) & codex::sf_nchar(x) == 9,
    stringr::str_glue("{substr(x, 1, 5)}-{substr(x, 6, 9)}") |> as.character(),
    x)
}

#' @noRd
make_phone <- function(x) {

  c1 <- "({substr(x, 1, 3)}) {substr(x, 4, 6)}-{substr(x, 7, 10)}"
  c2 <- ", ({substr(x, 13, 15)})-{substr(x, 16, 18)}-{substr(x, 19, 22)}"
  c3 <- codex::smush(c1, c2)

  data.table::fcase(
    codex::not_na(x) & codex::sf_nchar(x) == 10, stringr::str_glue(c1) |> as.character(),
    codex::not_na(x) & codex::sf_nchar(x) > 10, stringr::str_glue(c3) |> as.character(),
    default = x)
}

#' @noRd
as_date <- function(x, fmt = "%m/%d/%Y") as.Date(x, format = fmt)

#' @noRd
remove_periods <- function(x) codex::sf_remove(x, "\\.")

#' @noRd
wrap <- \(x, left, right) paste0(left, x, right)

#' @noRd
entity_recode <- function(x) {

  ent_lvls <- c("I" = "1", "O" = "2")

  forcats::fct(x, levels = c("1", "2")) |>
    forcats::fct_recode(!!!ent_lvls) |>
    as.character()
}
