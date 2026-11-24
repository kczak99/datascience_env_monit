library(readr)
library(dplyr)

#' Read half-hourly FLUXNET datasets from a folder
#'
#' Finds every CSV file whose name contains `FLUXMET_HH` in the supplied
#' directory or its subdirectories, then reads the selected FLUXNET variables.
#' Dataset names are taken from site codes in filenames (for example,
#' `CA-Gro`). If a filename has no site code matching that pattern, its name
#' without the `.csv` extension is used instead.
#'
#' @param data_dir Character scalar. Directory to search recursively for
#'   half-hourly FLUXNET CSV files. Defaults to `"data/raw/unzipped"`.
#'
#' @return A named list of tibbles, with one tibble per matching CSV file.
#'   The selected timestamp columns are read as character, `-9999` is treated
#'   as missing, and a `time` column is added by parsing `TIMESTAMP_START` as
#'   UTC.
#'
#' @details
#' The returned list is not assigned to a variable automatically. Assign it
#' with, for example, `flux_data <- read_hh("data/raw/unzipped")`, then access
#' a dataset with `flux_data[["CA-Gro"]]`.
#'
#' Requires the `readr` and `dplyr` packages.
#'
#' @examples
#' \dontrun{
#' flux_data <- read_hh("data/raw/unzipped")
#' flux_data[["CA-Gro"]]
#' }
#'
read_hh <- function(data_dir = "data/raw/unzipped") {
  keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
           "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
           "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")

  if (!dir.exists(data_dir)) {
    stop("Dataset directory does not exist: ", data_dir)
  }

  hh_files <- list.files(data_dir, pattern = "FLUXMET_HH.*\\.csv$",
                         ignore.case = TRUE, recursive = TRUE,
                         full.names = TRUE)
  if (length(hh_files) == 0) {
    stop("No FLUXMET_HH CSV datasets found in ", data_dir, ".")
  }

  dataset_names <- vapply(hh_files, function(file) {
    site_code <- regmatches(
      basename(file),
      regexpr("[A-Z]{2}-[A-Za-z0-9]{3}", basename(file), perl = TRUE)
    )
    if (length(site_code) == 0 || !nzchar(site_code)) {
      tools::file_path_sans_ext(basename(file))
    } else {
      site_code
    }
  }, character(1))

  if (anyDuplicated(dataset_names)) {
    duplicates <- unique(dataset_names[duplicated(dataset_names)])
    stop("Dataset names must be unique; duplicates found: ",
         paste(duplicates, collapse = ", "), ".")
  }

  datasets <- lapply(hh_files, function(file) {
    read_csv(file,
             na = "-9999",
             col_select = any_of(keep),
             col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c", .default = "d")) |>
      mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"))
  })
  setNames(datasets, dataset_names)
}

  
