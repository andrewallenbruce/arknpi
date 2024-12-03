source(here::here("data-raw", "pins_internal.R"))
library(collapse)

# npidata_paths <- dplyr::filter(nppez::get_pin("nber_weekly_info")$unzipped, file == "npidata_pfile")$path
# npidata_paths <- vctrs::vec_slice(
#   fs::dir_ls(path = fs::path("D:/NBER_NPI_Archives/weekly/unzipped", "npidata"), regexp = "npidata_pfile"),
#   fs::dir_ls(path = fs::path("D:/NBER_NPI_Archives/weekly/npidata", "unzipped"), regexp = "npidata") |>
#     stringr::str_which("[Ff]ile[Hh]eader", negate = TRUE))

npidata_paths <- fs::dir_ls(
  path = fs::path("D:/NBER_NPI_Archives/weekly/unzipped", "npidata"),
  regexp = "npidata_pfile_202401")

make_pin_id <- function(path) {

  x <- gsub("-", "_", tools::file_path_sans_ext(basename(path)))

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

read_npi_raw_csv <- function(path) {

  raw <- tidytable::fread(
    path,
    colClasses = list(character = 1:330)) |>
    janitor::clean_names() |>
    fuimus::remove_quiet() |>
    purrr::map_dfr(fuimus::na_if_common) |>
    dplyr::mutate(
      entity = entity_recode(entity_type_code) |> as.character(),
      entity_type_code = NULL,
      dplyr::across(.cols = dplyr::contains("_date"), .fns = as_date),
      dplyr::across(.cols = dplyr::contains("_name"), .fns = remove_periods))

  deactivated <- raw[
      codex::not_na(raw$npi_deactivation_date) &
        codex::na(raw$npi_reactivation_date), ] |>
    fuimus::remove_quiet() |>
    dplyr::select(npi, deact_date = npi_deactivation_date) |>
    dplyr::arrange(deact_date)

  list(
    raw = dplyr::filter(raw, !npi %in% deactivated$npi),
    deactivated = deactivated
  )
}

clean_weekly_npi_file <- function(path) {

  .c(raw, deactivated) %=% read_npi_raw_csv(path)

  base <- raw |>
    dplyr::reframe(
      npi,
      npi_replace = replacement_npi,
      entity,
      last_update = last_update_date,
      cert_date = certification_date,
      enum_date = provider_enumeration_date,
      deact_date = npi_deactivation_date,
      react_date = npi_reactivation_date,
      deact_code = npi_deactivation_reason_code,
      sole_prop = is_sole_proprietor,
      org_subpart = is_organization_subpart,
      org_parent = parent_organization_lbn,
      org_provider = provider_organization_name_legal_business_name,
      gender = provider_gender_code,
      prefix = provider_name_prefix_text,
      first_name = provider_first_name,
      middle_name = provider_middle_name,
      last_name = provider_last_name_legal_name,
      suffix = provider_name_suffix_text,
      credential = remove_periods(provider_credential_text)) |>
    fuimus::remove_quiet()

  address <- raw |>
    dplyr::reframe(
      npi,
      entity,
      mail_1 = provider_first_line_business_mailing_address,
      mail_2 = provider_second_line_business_mailing_address,
      mail_city = provider_business_mailing_address_city_name,
      mail_state = provider_business_mailing_address_state_name,
      mail_zip = provider_business_mailing_address_postal_code,
      mail_country = provider_business_mailing_address_country_code_if_outside_u_s,
      mail_phone = provider_business_mailing_address_telephone_number,
      mail_fax = provider_business_mailing_address_fax_number,
      prac_1 = provider_first_line_business_practice_location_address,
      prac_2 = provider_second_line_business_practice_location_address,
      prac_city = provider_business_practice_location_address_city_name,
      prac_state = provider_business_practice_location_address_state_name,
      prac_zip = provider_business_practice_location_address_postal_code,
      prac_country = provider_business_practice_location_address_country_code_if_outside_u_s,
      prac_phone = provider_business_practice_location_address_telephone_number,
      prac_fax = provider_business_practice_location_address_fax_number
    ) |>
    fuimus::combine(mail_address, c("mail_1", "mail_2"), sep = " ") |>
    fuimus::combine(prac_address, c("prac_1", "prac_2"), sep = " ") |>
    purrr::map_dfr(fuimus::na_if_common) |>
    fuimus::remove_quiet()

  other <- raw |>
    dplyr::reframe(
      npi,
      entity,
      other_org_name = provider_other_organization_name,
      other_org_type = provider_other_organization_name_type_code,
      other_prefix = provider_other_name_prefix_text,
      other_first = provider_other_first_name,
      other_middle = provider_other_middle_name,
      other_last = provider_other_last_name,
      other_last_type = provider_other_last_name_type_code,
      other_suffix = provider_other_name_suffix_text,
      other_credential = provider_other_credential_text) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::contains("_other"), .fns = remove_periods))

  other <- vctrs::vec_slice(other, which(cheapr::row_na_counts(other) < 9))

  ao <- raw |>
    dplyr::filter(entity == "O") |>
    dplyr::reframe(
      npi,
      entity,
      ao_prefix = authorized_official_name_prefix_text,
      ao_first = authorized_official_first_name,
      ao_middle = authorized_official_middle_name,
      ao_last = authorized_official_last_name,
      ao_suffix = authorized_official_name_suffix_text,
      ao_credential = authorized_official_credential_text,
      ao_title = authorized_official_title_or_position,
      ao_phone = authorized_official_telephone_number) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::contains("ao_"), .fns = remove_periods))

  cols_pattern <- fuimus::single_line_string("
  healthcare_provider_taxonomy_code|
  provider_license_number|
  provider_license_number_state_code|
  healthcare_provider_primary_taxonomy_switch|
  healthcare_provider_taxonomy_group")

  taxonomy_license <- raw |>
    dplyr::select(npi, entity, dplyr::matches(rlang::as_string(cols_pattern))) |>
    fuimus::remove_quiet() |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = 1) |>
    tidyr::pivot_longer(
      cols = dplyr::matches(rlang::as_string(cols_pattern)),
      names_to = c("variable", "group_id"),
      names_pattern = "(^[a-zA-Z_]+)_(.*)",
      values_to = "value",
      names_transform = list(group_id = as.integer)) |>
    dplyr::filter(codex::not_na(value)) |>
    dplyr::mutate(
      variable = dplyr::case_match(
        variable,
        "healthcare_provider_primary_taxonomy_switch" ~ "taxonomy_primary",
        "healthcare_provider_taxonomy_code"           ~ "taxonomy_code",
        "healthcare_provider_taxonomy_group"          ~ "taxonomy_group",
        "provider_license_number"                     ~ "license_no",
        "provider_license_number_state_code"          ~ "license_state",
        .default = variable)) |>
    dplyr::group_by(npi, group_id) |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::ungroup() |>
    dplyr::reframe(
      npi,
      entity,
      taxonomy_code,
      taxonomy_group = substr(taxonomy_group, 1, 10),
      taxonomy_primary,
      license_no,
      license_state)

  identifier <- raw |>
    dplyr::select(npi, entity, dplyr::starts_with("other_provider_identifier_")) |>
    fuimus::remove_quiet() |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = 1) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("other_provider_identifier_"),
      names_to = c("variable", "group_id"),
      names_pattern = "(^[a-zA-Z_]+)_(.*)",
      values_to = "value",
      names_transform = list(group_id = as.integer)) |>
    dplyr::filter(codex::not_na(value)) |>
    dplyr::mutate(
      variable = dplyr::case_match(
        variable,
        "other_provider_identifier"           ~ "otid",
        "other_provider_identifier_type_code" ~ "otid_type",
        "other_provider_identifier_issuer"    ~ "otid_issuer",
        "other_provider_identifier_state"     ~ "otid_state",
        .default = variable)) |>
    dplyr::group_by(npi, group_id) |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::ungroup() |>
    dplyr::select(npi, entity, otid, otid_type, otid_state, otid_issuer)

  list(
    deactivated = deactivated,
    base = base,
    address = address,
    other = other,
    ao = ao,
    taxonomy_license = taxonomy_license,
    identifier = identifier
  )
}

npidata_paths

dataset <- clean_weekly_npi_file(npidata_paths[5])

pin_update(
  dataset,
  name = make_pin_id(npidata_paths[5]),
  title = "NPPES 2024, Jan 29 - Feb 4",
  description = "NPPES NPI Registry, Week of January 29 - February 4, 2024"
)

# purrr::map(npidata_paths[1:2], clean_weekly_npi_file) |>
#   purrr::set_names(make_id(npidata_paths[1:2]))
