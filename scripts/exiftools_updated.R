# this is a test script

## USE EXIFTOOL TO PULL PHOTO METADATA
## 2022-07-15
## R. Peek

## Some notes on using exiftool
## this line will pull photo name, creation date, and exposure/gamecam strip info
## It's fast and requires "exiftools"
##@ exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:Software -EXIF:ImageDescription . > photoinfo/exifinfo.txt"

## Other commands
## exiftool -list -EXIF:All
## exiftool -r -w -common
## exiftool -r -s -G
## exiftool -r -T '-*Baro*' PICT0001.JPG 
## exiftool -r -T -IFD0:Image\\ Description PICT0001.JPG
## exiftool -T -r -g1 -IFD0:Image\\Description
## exiftool -r -T '-*ExposureLuma2' PICT0001.JPG
## exiftool -r -s -G -EXIF:ModifyDate PICT0001.JPG


# LIBRARIES ---------------------------------------------------------------
library(dplyr)
library(glue)
library(exiftoolr)
# exiftoolr::install_exiftool() # one time


# Photos Test ------------------------------------------------------------------

# test that it works
image_files <- dir(system.file("images", package = "exiftoolr"), 
                   full.names = TRUE)
exifinfo <- exif_read(image_files)
dim(exifinfo)
names(exifinfo)
glimpse(exifinfo)

# get these fields
fields_to_keep <- c("FileName", "FileCreateDate", "ExposureTime", 
                    "BrightnessValue", "LightValue", "GPSLatitude", "GPSLongitude", "GPSAltitude", "FOV", "FocalLength35efl")

# read data
exif_read(image_files, tags = fields_to_keep)

# call exif
(exifinfo <- exif_call(args=c("-r", "-s", "-T", "-n", "-q", "-FILE:FileName", "-EXIF:CreateDate", "-EXIF:MakerNoteUnknownText", "-Composite:LightValue"), path=image_files[1]))


# Photo Browning Extract --------------------------------------------------


path_to_photos <- "~/../Downloads/data_temp/T_LAPSE/"

# check what can be read with exif?
exif_read(path_to_photos, recursive = TRUE)

# rename files to avi

# Rename To AVI -----------------------------------------------------------

library(fs)

fspath <- path_expand("~/Downloads/data_temp/T_LAPSE/")
img_files <- fs::dir_ls(path = fspath, type = "file", glob = 
                          "*TLS")

# change extension on everything to AVI
fs::file_move(img_files, path_ext_set(img_files, ext = "AVI"))

# Convert Images ----------------------------------------------------------

# need AV package for avis
# install.packages("av")
library(magick)
library(here)
library(tidyr)

# convert to png
tst <- image_read_video(glue("{path_to_photos}/TIMEL0015.avi"), 
                        fps = 5, format = "png")
#tst_png <- image_convert(tst, "jpg")
#head(tst_png)

# write one
image_write(tst[1], path = "~/../Downloads/data_temp/output/tst.jpg")

# write all
library(purrr)
map(1:length(tst), ~image_write(tst[.x], path = glue("output/tst_{.x}.jpeg"), format = "jpeg"))

# now get info from image: this is with magick
image_info(image_read("output/tst_1.jpeg"))

# this is with exifinfo
tst_exif <- exif_read("output/",recursive = TRUE, pipeline = "csv")
(jpg_only <- exif_read(path = c("output/tst_1.jpeg", "output/tst_2.jpeg"), 
                       pipeline = "csv"))

tst2 <- exif_read(path = "output/tst_1.jpeg") |> as_tibble()
tst2