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