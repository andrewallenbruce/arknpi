nber_tbl <- function(x) {

  url <- list(
    monthly = "https://data.nber.org/npi/zip/",
    weekly = "https://data.nber.org/npi/weekly/",
    byvar = "https://data.nber.org/npi/byvar/",
    zipcode = "https://data.nber.org/distance/"
  )

  rvest::read_html(url[[x]]) |>
    rvest::html_table() |>
    purrr::pluck(1) |>
    janitor::clean_names() |>
    dplyr::slice(2:dplyr::n()) |>
    fuimus::remove_quiet() |>
    dplyr::mutate(
      last_modified = anytime::anytime(last_modified),
      size = fs::as_fs_bytes(size),
      url = as.character(glue::glue('{url[[x]]}{name}'))) |>
    dplyr::arrange(dplyr::desc(last_modified))
}

nber_tbl("monthly")

nber_sum <- function(table, directory) {

  table |>
    dplyr::summarise(
      files = dplyr::n(),
      size = sum(size, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      directory = directory,
      .before = 1
    )
}

nber_all <- function() {

  tbl <- list(
    monthly = nber_tbl("monthly"),
    weekly  = nber_tbl("weekly"),
    byvar   = nber_tbl("byvar"),
    zipcode = nber_tbl("zipcode"))

  sums <- list(
    monthly = nber_sum(tbl$monthly, "monthly"),
    weekly  = nber_sum(tbl$weekly, "weekly"),
    byvar   = nber_sum(tbl$byvar, "byvar"),
    zipcode = nber_sum(tbl$zipcode, "zipcode")) |>
    purrr::list_rbind()

  list(
    summary  = sums,
    datasets = purrr::list_rbind(tbl, names_to = "dataset")
  )
}

download_zips <- function(table, directory) {

  zip_paths <- stringr::str_glue("{directory}{table$name}")

  result <- curl::multi_download(
    urls = table$url,
    destfile = zip_paths,
    resume = TRUE,
    timeout = 60
  )
  return(c(result, zip_paths))
}

create_zip_file_names <- function(
    file_names,
    remove = ".zip|week|npidata_pfile_",
    collapse = "|",
    left = "week:",
    right = "") {

  x <- basename(file_names) |>
    stringr::str_remove_all(remove) |>
    strex::str_split_by_numbers() |>
    purrr::list_transpose() |>
    purrr::discard_at(2) |>
    purrr::set_names(c("start", "end"))

  x |>
    purrr::map(as_date) |>
    purrr::list_transpose() |>
    purrr::map(paste0, collapse = collapse) |>
    purrr::map(nppez::wrap, left = left, right = right) |>
    unlist(use.names = FALSE)

}
