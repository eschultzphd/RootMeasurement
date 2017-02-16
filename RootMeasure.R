#(C) Eric R Schultz 2017

####SET PATHS AND CORRECTION FACTORS####
library(plyr)
library(plotrix)
library(dplyr)
Pre <- function() {
  message("Enter Scale Correction Factor")
    CF <- scan(n = 1)
  message("\n","Enter Angle B Correction Factor")
    BCF <- scan(n = 1)
  paste(CF, BCF, sep = " ")
}

Path <- file.choose()

####   IMPORTING   ####
#Split output from RunFirst
strsplit(Pre(), " ")->Settings
t(data.frame(unlist(Settings)))->set2

as.numeric(set2[,1])->CF
as.numeric(set2[,2])->BCF

#Parse data into distinct time points
snakes <- read.delim(Path, header=FALSE, skip=11)
time <- lapply(1:(NROW(unique(snakes[,1]))-2), function(x){paste0("t-",x)}) #assign var
times <- lapply(1:(NROW(time)), function(x){snakes[snakes[,"V1"]==x,]}) #separation occurs

#Determine number of traces (snakes)
totpl<-sum(times[[1]][,2]==0)

#First/Last point identification
FLs <- lapply(1:NROW(times), function(x){suppressWarnings(
    filter(
      times[[x]],
        times[[x]][,2]==0 | 
        abs((times[[x]][(1:NROW(times[[x]])),"V2"]-
              times[[x]][(1:NROW(times[[x]]))+1,"V2"])>0) | 
        times[[x]][(1:NROW(times[[x]])),"V4"]-
              times[[x]][(1:NROW(times[[x]]))-1,"V4"]>0)
  )})

####  QUANTIFICATION, CALCULATION, AND CONCATINATION  ####
#Calculation of Length Along Trace or Integrated Length (L; using times)
L1 <- lapply(1:NROW(times), function(x){
  ifelse(
    (times[[x]][1:NROW(times[[x]]),2]-times[[x]][1:NROW(times[[x]])+1,2])<0, 
      (sqrt(((times[[x]][1:NROW(times[[x]]),3] - times[[x]][1:NROW(times[[x]])+1,3])^2) +
             (times[[x]][1:NROW(times[[x]]),4] - times[[x]][1:NROW(times[[x]])+1,4])^2)), 
  NA)
})

L2 <- lapply(1:NROW(times), function(x){
  split(L1[[x]],ifelse(is.na(L1[[x]]),NA,cumsum(is.na(L1[[x]]))))
})

L3 <- lapply(1:NROW(times), function(x){
  lapply(L2[[x]],sum)
})

LENGTH <- lapply(1:NROW(times), function(x){
  unlist(L3[[x]])/CF
})

#Calculation of Length of the Cord or First Point to Last Point (Lc; using FLs)
Lc <- lapply(1:NROW(FLs), function(x){
  (sqrt(
    ((FLs[[x]][seq(1, by=2, NROW(FLs[[x]])),3] - 
           FLs[[x]][seq(1, by=2, NROW(FLs[[x]]))+1,3])^2) +
            ((FLs[[x]][seq(1, by=2, NROW(FLs[[x]])),4] - 
                FLs[[x]][seq(1, by=2, NROW(FLs[[x]]))+1,4])^2)
    )
  /CF)
})

#Calculation of Angle of Trace (B; using FLs)
B <- lapply(1:NROW(FLs), function(x){
  (atan((FLs[[x]][seq(1, by=2, NROW(FLs[[x]]))+1,3] - 
        FLs[[x]][seq(1, by=2, NROW(FLs[[x]])),3]) / 
       (FLs[[x]][seq(1, by=2, NROW(FLs[[x]]))+1,4] - 
        FLs[[x]][seq(1, by=2, NROW(FLs[[x]])),4])
       )
    * (180/pi)
   + BCF)
})

#Lc/L for straightness
STR <- relist((unlist(Lc)/unlist(LENGTH))*100, Lc)

#Lx/L for HGI
HGI <- relist(((sin(unlist(B)*(pi/180))*unlist(Lc))/unlist(LENGTH))*100, Lc)

##Counting Wave Number for Wave Density (WD; using times)
Wn1 <- lapply(1:NROW(times), function(x){
  ifelse(
    (
      (times[[x]][1:NROW(times[[x]]),2] - times[[x]][1:NROW(times[[x]])+1,2])<0),
    (times[[x]][1:NROW(times[[x]]),3] - times[[x]][1:NROW(times[[x]])+1,3]),
    NA
  )
}) #Count directional changes

Wn2 <- lapply(1:NROW(times), function(x){
  split(Wn1[[x]][!is.na(Wn1[[x]])],cumsum(is.na(Wn1[[x]]))[!is.na(Wn1[[x]])])
}) #Split into list

Wn3 <- lapply(1:NROW(times), function(x){
  mapply("-", Wn2[[x]], lapply(Wn2[[x]],mean), SIMPLIFY = FALSE)
}) #Calculate and correct using mean of trace

Wn <- lapply(1:NROW(times), function(x){
  as.list(
    sapply(lapply(lapply(lapply(Wn3[[x]], sign), diff), abs), sum)/2)
}) #Calculate Wave Number (Wn)

WD <- lapply(1:NROW(times), function(x){
  unlist(Wn[[x]])/LENGTH[[x]]}) #Wn to WD; waves / mm

#Apply stats and build table
L.mean <- lapply(1:NROW(times), function(x){mean(LENGTH[[x]])})
Lc.mean <- lapply(1:NROW(times), function(x){mean(Lc[[x]])})
STR.mean <- lapply(1:NROW(times), function(x){mean(STR[[x]])})
WD.mean <- lapply(1:NROW(times), function(x){mean(WD[[x]])})
B.mean <- lapply(1:NROW(times), function(x){mean(B[[x]])})
HGI.mean <- lapply(1:NROW(times), function(x){mean(HGI[[x]])})

L.sd <- lapply(1:NROW(times), function(x){sd(LENGTH[[x]])})
Lc.sd <- lapply(1:NROW(times), function(x){sd(Lc[[x]])})
STR.sd <- lapply(1:NROW(times), function(x){sd(STR[[x]])})
WD.sd <- lapply(1:NROW(times), function(x){sd(WD[[x]])})
B.sd <- lapply(1:NROW(times), function(x){sd(B[[x]])})
HGI.sd <- lapply(1:NROW(times), function(x){sd(HGI[[x]])})

L.combo <- lapply(1:NROW(times), function(x){
  c(unlist(LENGTH[[x]]), L.mean[[x]], L.sd[[x]])})
Lc.combo <- lapply(1:NROW(times), function(x){
  c(unlist(Lc[[x]]), Lc.mean[[x]], Lc.sd[[x]])})
STR.combo <- lapply(1:NROW(times), function(x){
  c(unlist(STR[[x]]), STR.mean[[x]], STR.sd[[x]])})
WD.combo <- lapply(1:NROW(times), function(x){
  c(unlist(WD[[x]]), WD.mean[[x]], WD.sd[[x]])})
B.combo <- lapply(1:NROW(times), function(x){
  c(unlist(B[[x]]), B.mean[[x]], B.sd[[x]])})
HGI.combo <- lapply(1:NROW(times), function(x){
  c(unlist(HGI[[x]]), HGI.mean[[x]],HGI.sd[[x]])})

all.dat <- as.data.frame(c(
  L.combo, "",
  Lc.combo, "",
  STR.combo, "",
  WD.combo, "",
  B.combo, "",
  HGI.combo)) #Final table build

rownames(all.dat) <- c(paste("Trace", 1:totpl), "Mean", "Std. Dev")
colnames(all.dat) <- c(paste("Length - Time ", 1:NROW(times)), "", 
                       paste("Lc - Time", 1:NROW(times)), "",
                       paste("STR - Time", 1:NROW(times)), "",
                       paste("WD - Time", 1:NROW(times)), "",
                       paste("B - Time", 1:NROW(times)), "",
                       paste("HGI - Time", 1:NROW(times)))

####  EXPORT AND SAVING  ####
SavePath <- dirname(Path)
message("\n","Enter filename (Use _ insead of space):") 
processed.filename <- scan(n = 1, what = "character") # read 1 line from console 
Save.Path <- file.path(SavePath, paste0(processed.filename,".csv"))
write.csv(all.dat, Save.Path)
message("\n","Process complete - file saved")