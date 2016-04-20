#(C) Eric R Schultz 2016
#UF SpacePlantsLab
#University of Florida

####SET PATHS AND CORRECTION FACTORS####
#If library() errors, use lib.loc="(path to library)"
#See ?library() for more info

library("plyr")
library("plotrix")
library("dplyr")

Pre <- function() {
  
  message("Enter Path")
  Path <- readLines(n = 1)
  
  message("Enter Save Path")
  SavePath <- readLines(n = 1)
  
  message("Enter Scale Correction Factor")
  CF <- readLines(n = 1)
  
  message("Enter Angle B Correction Factor")
  BCF <- readLines(n = 1)
  paste(Path, SavePath, CF, BCF, sep = " ")
}

Pre()->RunFirst
