if (!requireNamespace("googledrive", quietly = TRUE)) {
  install.packages("googledrive")
} else {
  print("googledrive is already installed")
}


library(googledrive)
library(tidyverse)

# Authorization process ----

drive_auth()

#If you don't have access to the EDGE Google Drive required folder, contact Anne or the owner of the folder.

#Verifying Google Drive Access ----

# List files in your Google Drive
drive_find(n_max = 5)


#Reading a file from Google Drive ----

# .Assuming you know the file ID or name, here is how to read a file ----
file <- drive_get(as_id("1MjtJYPdBGU4z4jeurquRL2Qrq2hPOwDMK__xmHJGEDo")) # Get the file from Google Drive
drive_download(file, path = "Inputs/Raw_data/Example",overwrite=TRUE) # Download it locally
data <- read_csv("Inputs/Raw_data/Example.csv") # Read the file into R

#. IF you do not know the file ID 

# Identify files
drive_find(n_max = 30)
x<-drive_find(type = "spreadsheet")  
x<-x |> dplyr::filter(name=="Example",overwrite==TRUE)

# download
drive_download(x$id,
               path = "Inputs/Raw_data/Example.csv")

#. Download Folders ----


#folder link to id
folder <- drive_get( "https://drive.google.com/drive/u/1/folders/12rePKv_2_MAYIYo1Q_SE3FPRiatAu_wr")

#find files in folder
files <- drive_ls(folder)
#In this example we only have one file in case you have more the tibble will be longer
files_bind <- dplyr::bind_rows(files) # make it a tibble

dir_local<-"Inputs/Raw_data/"

# Batch download the files
map2(files_bind$id, files_bind$name, ~drive_download(as_id(.x), path = file.path(dir_local, .y),overwrite=TRUE))


# More advance code to check if the files I am downloading are the last version ----

#required functions 
source("Scripts/0_google_drive_helpers.R")

# Folder link to id
folder <- drive_get("https://drive.google.com/drive/u/1/folders/12rePKv_2_MAYIYo1Q_SE3FPRiatAu_wr")

# Find files in folder
files <- drive_ls(folder)
files_bind <- dplyr::bind_rows(files)  # Make it a tibble

# Local directory
dir_local <- "Inputs/Raw_data/"

#Only one file
download_if_different(files_bind$id, files_bind$name,dir_local)

# Batch process files in the folder
walk2(files_bind$id, files_bind$name, ~download_if_different(.x, .y, dir_local),.progress = TRUE)
