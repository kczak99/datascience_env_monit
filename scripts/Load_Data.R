# Creates unzipped file folder and loads all five datasets into flux_data

read_hh <- function(file) {
  read_csv(file.path("data/raw/unzipped", file),
           na = "-9999",
           col_select = any_of(keep),
           col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c", .default = "d")) |>
    mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"))
}

# ------------------------------------------- 

for (i in 1:5) {
read_hh(i)
}

zip_files <- list.files("data/raw", pattern = "\\.zip$", full.names = TRUE)
if (length(zip_files) == 0) {
  stop("No FLUXNET ZIP archives found in data/raw.")
}

dir.create("data/raw/unzipped", recursive = TRUE, showWarnings = FALSE)
for (zip_file in zip_files) {
  hh_files <- grep(
    "FLUXMET_HH",
    unzip(zip_file, list = TRUE)$Name,
    value = TRUE
  )
  if (length(hh_files) != 1) {
    stop("Expected one FLUXMET_HH file in ", basename(zip_file),
         "; found ", length(hh_files), ".")
  }
  unzip(zip_file, files = hh_files, exdir = "data/raw/unzipped")
}

library(readr)
library(dplyr)

keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
          "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
          "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")

FLUXNET_HHs <- list.files(
  "data/raw/unzipped",
  pattern = "FLUXMET_HH",
  recursive = TRUE
)

site_codes <- c(
  CA_Gro = "CA-Gro",
  US_Ro4 = "US-Ro4",
  CZ_RAJ = "CZ-RAJ",
  FR_Aur = "FR-Aur",
  JP_Spp = "JP-Spp"
)
site_files <- setNames(
  vapply(site_codes, function(code) {
    matches <- FLUXNET_HHs[grepl(code, FLUXNET_HHs, fixed = TRUE)]
    if (length(matches) != 1) {
      stop("Expected one half-hour file for ", code, "; found ", length(matches), ".")
    }
    matches
  }, character(1)),
  names(site_codes)
)

flux_data <- lapply(site_files, read_hh)
  