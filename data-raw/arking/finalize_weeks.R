source(here::here("data-raw", "pins_internal.R"))

# "wk_2024_01_01-07"
# "wk_2024_01_08-14"
# "wk_2024_01_15-21"
# "wk_2024_01_22-28"
# "wk_2024_01_29-04"

wk <- arknpi::get_pin("wk_2024_01_29-04")

#----------- DEACTIVATED NPIs
deactivated <- vctrs::vec_rbind(
  arknpi::get_pin("deactivated"),
  wk$deactivated)

pin_update(
  deactivated,
  name        = "deactivated",
  title       = "Deactivated NPIs",
  description = "Deactivated NPIs and Date of Deactivation"
)

#----------- NPPES BASE
base <- wk$base

base |>
  collapse::fcount(suffix)

# Split into individuals and organizations
base_grp <- collapse::GRP(base, ~ entity)

base_split <- collapse::rsplit(base, base_grp) |>
  purrr::map(fuimus::remove_quiet) |>
  setNames(collapse::GRPnames(base_grp))

base_split
