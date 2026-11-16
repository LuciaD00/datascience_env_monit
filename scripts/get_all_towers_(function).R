# import all towers (function)

library(readr)
library(dplyr)
library(purrr)
library(stringr)

keep <- c("TIMESTAMP_START", "TIMESTAMP_END", "TA_F", "SW_IN_F", "VPD_F", "P_F",
          "NETRAD", "G_F_MDS", "H_F_MDS", "H_F_MDS_QC", "LE_F_MDS", "LE_F_MDS_QC",
          "NEE_VUT_REF", "NEE_VUT_REF_QC", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")

read_hh <- function(zip_file) {
  # find the half-hourly FLUXMET file inside this zip
  hh_name <- grep("FLUXMET_HH", unzip(zip_file, list = TRUE)$Name, value = TRUE)[1]
  # unzip only that csv
  unzip(zip_file, files = hh_name, exdir = "data/raw/unzipped", overwrite = T)
  # read it
  hh <- read_csv(file.path("data/raw/unzipped", hh_name),
               na = "-9999",
               col_select = any_of(keep),
               col_types = cols(TIMESTAMP_START = "c", TIMESTAMP_END = "c", .default = "d")) |>
  mutate(time = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"))    
  # add site id from file name
  hh$site <- str_extract(basename(hh_name), "[A-Z]{2}-[A-Za-z0-9]+")
  # return clean table
  hh
}

# list of all five zips
zip_files <- list.files("data/raw", pattern = "\\.zip$", full.names = TRUE)

# apply the functiont to each one and combine results
flux <- map_dfr(zip_files, read_hh)

# check
unique(flux$site) # 5 sites?
count(flux, site) # number of half-hourly rows per tower 
flux |>
  group_by(site) |>
  summarise(first_time = min(time), last_time = max(time),
    .groups = "drop") # data range for each tower