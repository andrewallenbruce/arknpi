#' NPPES Other Identifier Type
#'
#' @param x `<chr>` vector of `otid` types
#'
#' @returns `<chr>` vector of `otid` descriptions
#'
#' @examples
#' dplyr::tibble(
#'  otid_type = as.character(1:8),
#'  otid_desc = other_id_type(otid_type))
#'
#' @autoglobal
#'
#' @family NPPES Decoders
#'
#' @export
other_id_type <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Other",
    "2", "Medicare UPIN",
    "4", "Medicare ID-Type Unspecified",
    "5", "Medicaid",
    "6", "Medicare Oscar-Certification",
    "7", "Medicare NSC",
    "8", "Medicare PIN",
    default = NA_character_
  )
}

#' NPPES Other Name Type
#'
#' @param x `<chr>` vector; `other_last_type` or `other_org_type`
#'
#' @returns `<chr>` vector of `other_*_type` descriptions
#'
#' @examples
#' dplyr::tibble(
#'  oname_type = as.character(1:5),
#'  oname_desc = other_name_type(oname_type))
#'
#' @autoglobal
#'
#' @family NPPES Decoders
#'
#' @export
other_name_type <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Former Name",
    "2", "Professional Name",
    "3", "Doing Business As",
    "4", "Former Legal Business Name",
    "5", "Other Name",
    default = NA_character_
  )
}
