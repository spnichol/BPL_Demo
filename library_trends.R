setwd("C:\\Users\\Steven\\Google Drive\\6. Data_Fun\\library_trends")

survey <- read.csv("trends.csv")
head(survey)
loc <- read.csv("loc.csv")
head(loc)

help(combine)

loc1 <- subset(loc, select=c("LIBNAME", "FSCSKEY", "Location"))
head(loc1)
rbind(

subset(loc, loc$LIBNAME == "DODGE LIBRARY")
loc['id'] <- paste(loc$FSCSKEY, loc$FSCS_SEQ, sep="-")
library(dplyr)
library(tidyr)

loc2 <- separate(loc1, Location, into=c("Address", "City", "Coordinates"), sep="\n", extra="merge")
head(loc2)
loc2 <- separate(loc2, Coordinates, into=c("Latitude", "Longitude"), sep=",", extra="merge")
head(loc2)
loc2$Latitude <- gsub("(", "", loc2$Latitude, fixed=TRUE)
loc2$Longitude <- gsub(")", "", loc2$Longitude, fixed=TRUE)


survey <- survey[survey$STABR == "NY", ]
libraries <- survey[survey$LIBNAME %in% c("BROOKLYN PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY, THE BRANCH LIBRARIES", "QUEENS BOROUGH PUBLIC LIBRARY"), ]
libraries <- subset(libraries, select=c("LIBNAME", "TOTCIR", "PLSYEAR", "SUBSCRIP", "BKVOL", "ATTEND"))
bk <- subset(libraries, libraries$LIBNAME == "BROOKLYN PUBLIC LIBRARY")
library(stringr)


bkSub <- subset(bk, select=c("SUBSCRIP", "PLSYEAR", "TOTCIR"))

length(years)
  
years <- bkSub$PLSYEAR
sub <- bkSub$SUBSCRIP
cat(paste(shQuote(years, type="cmd"), collapse=", "))
cat(paste(shQuote(sub, type="cmd"), collapse=", "))
library(RColorBrewer)
brewer.pal(12, "BuGn")
help("RColorBrewer")
install.packages("randomcoloR")
library(randomcoloR)
n <- 17
palette <- distinctColorPalette(n)
palette
cat(paste(shQuote(palette, type="cmd"), collapse=", "))


#fix year for tableau analysis 

libraries["Year"]<-   paste("01/01/",libraries$PLSYEAR, sep="")
librariesNew <- subset(libraries, select=c("Year", "TOTCIR"))
libraries$CENTRACT <- NULL

newlib <- libraries[, (1:110)]
newlib["Year"]<-   paste("01/01/", newlib$PLSYEAR, sep="")
#write to CSV
write.csv(newlib, "libraries.csv")


#read in 2013 file

lib_2013 <- read.csv("2013.csv")
head(lib_2013)
lib_2013$TOTCIR
ny_lib_2013 <- subset(lib_2013, lib_2013$STABR == "NY")
ny_lib_2013 <- ny_lib_2013[ny_lib_2013$ny_lib_2013 %in% c("BROOKLYN PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY, THE BRANCH LIBRARIES", "QUEENS BOROUGH PUBLIC LIBRARY"), ]


x <- intersect(colnames(newlib), colnames(ny_lib_2013))

ny_lib_2013 <- subset(ny_lib_2013, select=x)
newlib_combo <- subset(newlib, select=x)
ny_lib_2013 <- as.data.frame(ny_lib_2013)
newlib_combo <- as.data.frame(newlib_combo)
combo <- rbind()
help(rbind)
library(data.table)

df_combo <- rbind(ny_lib_2013, newlib_combo)

