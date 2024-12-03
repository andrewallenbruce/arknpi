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
