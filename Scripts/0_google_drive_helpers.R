# Author: Camila Pacheco
# Year: 2024
# This script contains functions that allow for downloading a file from Google Drive 
# only if the file has been updated or doesn't exist locally. The script compares 
# the local and remote versions to determine if a download is necessary.

check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = TRUE)
  }
}

# List of packages to check
packages <- c("digest", "tools", "crayon", "googledrive", "tidyverse")

# Loop through each package and check/install
for (pkg in packages) {
  check_and_install(pkg)
}

normalize_file <- function(file_path) {
  if (!is.null(file_path) && file.exists(file_path)) {
    lines <- suppressWarnings(readLines(file_path))
    temp_normalized_file <- tempfile()
    writeLines(lines, temp_normalized_file)
    return(temp_normalized_file)
  }
  return(NULL)  # Return NULL if file doesn't exist
}

calculate_checksum <- function(file_path) {
  if (!is.null(file_path) && file.exists(file_path)) {
    file_size <- file.info(file_path)$size
    if (file_size > 0) {
      return(digest(file_path, algo = "md5", file = TRUE))
    }
  }
  return(NULL)  # Return NULL if file doesn't exist or is empty
}

# Function to download files only if they differ
download_if_different <- function(file_id, file_name, dir_local) {
  
  teal <- make_style("#008080") 
  # Extract the extension from the Google Drive file
  file_ext_drive <- file_ext(file_name)
  
  # Build the local file name by looking for a file with the same base name and extension
  local_file_base <- file.path(dir_local, file_path_sans_ext(file_name))
  
  # Search for any existing file with matching base name and any extension
  local_file_candidates <- list.files(dir_local, pattern = paste0("^", basename(local_file_base), "\\."), full.names = TRUE)
  
  # Select the local file if it exists, otherwise assume we have no matching file
  local_file_ext <- if (length(local_file_candidates) > 0) local_file_candidates[1] else file.path(dir_local, file_name)
  
  # Calculate checksum of the local file (if it exists)
  local_file<-normalize_file(local_file_ext)
  local_checksum <- calculate_checksum(local_file)
  
  # Download file metadata from Google Drive
  drive_file <- drive_get(as_id(file_id))
  correct_name <- drive_file$name
  
  # Create a full path for the temporary file, appending the correct file name from Google Drive
  temp_file_path <- file.path(tempdir())
  temp_file_with_name<-paste0(temp_file_path,"/",correct_name)
  # Download a temporary file to compare checksums without overwriting
  #local_drive_quiet()
  with_drive_quiet(
    drive_download(as_id(file_id), path = temp_file_with_name, overwrite = TRUE)
  )
  # Search for any existing file with matching base name and any extension
  temp_file <- list.files(temp_file_path, pattern = paste0("^", basename(temp_file_with_name), "\\."), full.names = TRUE)
  
  # Calculate checksum of the downloaded file
  temp_file<-normalize_file(temp_file)
  drive_checksum <- calculate_checksum( temp_file )
  
  # Compare checksums and only download if they are different
  if (!is.null(local_checksum) && !is.null(drive_checksum)) {
    if (local_checksum != drive_checksum) {
      message(teal(paste("Downloading and updating:", file_name)))
      drive_download(as_id(file_id), path = local_file_ext, overwrite = TRUE)
    } else {
      message(teal(paste("File is up-to-date:", file_name)))
    }
  } else {
    message(teal(paste("file doesn't exist locally. Downloading:", file_name)))
    drive_download(as_id(file_id), path = local_file_ext, overwrite = TRUE)
  }
  # Clean up the temporary file
  unlink(temp_file_path)
  unlink(local_file)
  unlink(temp_file)
}
