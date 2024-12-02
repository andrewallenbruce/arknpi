source(here::here("data-raw", "pins_internal.R"))
library(collapse)

# npidata_paths <- dplyr::filter(nppez::get_pin("nber_weekly_info")$unzipped, file == "npidata_pfile")$path

npidata_paths <- vctrs::vec_slice(
  fs::dir_ls(path = fs::path("D:/NBER_NPI_Archives/weekly", "unzipped"), regexp = "npidata"),
  fs::dir_ls(path = fs::path("D:/NBER_NPI_Archives/weekly", "unzipped"), regexp = "npidata") |>
    stringr::str_which("[Ff]ile[Hh]eader", negate = TRUE))

make_pin_id <- \(path) {

  x <- gsub("-", "_", tools::file_path_sans_ext(basename(npidata_paths[1])))

  strs <- codex::sf_strsplit(x, "_")[[1]]

  dates <- as_date(strs[3:4], fmt = "%Y%m%d")

  yr <- as.character(lubridate::year(dates[1]))

  mn <- lubridate::month(dates[1]) |>
    stringr::str_pad(2, pad = "0")

  days <- lubridate::day(dates) |>
    stringr::str_pad(2, pad = "0") |>
    stringr::str_c(collapse = "-")

  stringr::str_glue("wk_{yr}_{mn}_{days}")

}

as_date        <- \(x, fmt = "%m/%d/%Y") as.Date(x, format = fmt)

remove_periods <- \(x) codex::sf_remove(x, "\\.")

wrap           <- \(x, left, right) paste0(left, x, right)

entity_recode <- function(x) {

  enlvls <- c("I" = "1", "O" = "2")

  forcats::fct(x, levels = c("1", "2")) |>
    forcats::fct_recode(!!!enlvls)
}

# Address functions
make_case <- function(x, y) {
  data.table::fcase(
    codex::not_na(x) & codex::not_na(y) & x == y, stringr::str_glue("{y}"),
    codex::not_na(x) & codex::not_na(y) & x != y, stringr::str_glue("{x}, {y}"),
    codex::not_na(x) & codex::na(y), stringr::str_glue("{x}"),
    codex::na(x) & codex::not_na(y), stringr::str_glue("{y}")
  )
}

make_zip <- function(x) {
  purrr::map_chr(x, function(x) {
    if (codex::not_na(x) & nchar(x) == 9)
      stringr::str_glue("{substr(x, 1, 5)}-{substr(x, 6, 9)}")
    else
      x
  })
}

make_phone <- function(x) {

  cnd1 <- "({substr(x, 1, 3)}) {substr(x, 4, 6)}-{substr(x, 7, 10)}"
  cnd2 <- codex::smush(cnd1, ", ({substr(x, 13, 15)})-{substr(x, 16, 18)}-{substr(x, 19, 22)}")

  purrr::map_chr(x, function(x) {
    if (codex::not_na(x)) {
      data.table::fcase(
        nchar(x) == 10, stringr::str_glue(cnd1),
        nchar(x) > 10, stringr::str_glue(cnd2),
        default = stringr::str_glue("{x}")
      )
    } else str_glue("{x}") }
  )
}
