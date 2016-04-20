#(C) Eric R Schultz 2016
#UF SpacePlantsLab
#University of Florida

####IMPORT####
##Split output from RunFirst##
strsplit(RunFirst, " ")->Settings
t(data.frame(unlist(Settings)))->set2

set2[,1]->Path
set2[,2]->SavePath
as.numeric(set2[,3])->CF
as.numeric(set2[,4])->BCF

snakes <- read.delim(Path, header=FALSE, skip=11)
day8<-snakes[snakes[, "V1"]==1,]

####MAKEFL####
#Selecting first and last days
#n set for each day
#Must be run after ImportAndParseToDays code

#Start day8
n=(1:nrow(day8))
suppressWarnings(
  day8fl<-filter(
    day8, V2==0 | abs((day8[n,"V2"]-day8[n+1,"V2"])>0) | (day8[n,"V4"]-day8[n-1,"V4"])>0
  ))
#End day8

####CALC####
#Calculations
#L=length along root
#Lc=length of cord (first to last point)
#B=angle of Lc

##Calculation of L by day by snake
#Determine number of plants (snakes)
totpl<-sum(day8[,2]==0)

#Return matrix of distances between points, with snake separations as "NA"

n=(1:nrow(day8))
ifelse(
  (day8[n,2]-day8[n+1,2])<0, 
  ((sqrt(((day8[n,3]-day8[n+1,3])^2)+(day8[n,4]-day8[n+1,4])^2))), 
  NA)->m
m<-data.frame(m)
g8<-ifelse(is.na(m$m),NA,cumsum(is.na(m$m)))
s8<-split(m,g8)
il8<-(sapply(s8, sum)/CF)

#Calculation of Lc

Lc<-function(n)
  ((sqrt(((day8fl[n,3]-day8fl[n+1,3])^2)+(day8fl[n,4]-day8fl[n+1,4])^2))/CF)
n<-seq(1, by=2, nrow(day8fl))
lc8<-Lc(n)

#Calculation of B

B<-function(n)
  atan((day8fl[n+1,3]-day8fl[n,3])/(day8fl[n+1,4]-day8fl[n,4]))*(180/pi)
n<-seq(1, by=2, nrow(day8fl))
(B(n)+BCF)->b8

####STAT####
#Compile and Calculate
#Data into one form
#Statistics (std.err)

##Name snakes
names<-c(paste("root", 1:totpl), "Mean", "Std. Err")

##Lc/L for straightness
lc8/il8->str8

##Lx/L for HGI
(sin(b8*(pi/180))*lc8)/il8->hgi8

##Counting number of direction changes (wave number)
n=(1:nrow(day8))
ifelse(
  (
    (day8[n,2]-day8[n+1,2])<0),
  (day8[n,3]-day8[n+1,3]),
  NA
)->sc8
sc8<-data.frame(sc8)
split(sc8[!is.na(sc8)],cumsum(is.na(sc8))[!is.na(sc8)])->ssc8
sapply(ssc8, mean)->wvx8
mapply("-", ssc8, wvx8, SIMPLIFY=FALSE)->wv8
lapply(wv8, sign)->wv81
lapply(wv81, diff)->wv82
lapply(wv82, abs)->wv83
sapply(wv83, sum)/2->wvno8

##Convert wave number to wave density (WD)
wvno8/il8->WD

##Build data table of L, Lc, B, STR, WD, and HGI
rbind(il8, lc8, b8, str8, WD, hgi8, NA)->d8
rbind(d8)->DATA

##Add Statistics-Mean
apply(DATA,1,mean)->mean

##Add Statistics-Std.Err.
apply(DATA,1,std.error)->stderr

##Combine again
cbind(DATA, mean, stderr)->DATA
rbind(names,DATA)->DATA
DATA[is.na(DATA)] <- " "

####EXPORT####
write.table(DATA, SavePath, sep="\t")