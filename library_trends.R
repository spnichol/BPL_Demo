setwd("C:\\Users\\Steven\\Google Drive\\6. Data_Fun\\library_trends\\data_files")

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
lib_2013['Year'] <- "01/01/2013"
ny_lib_2013 <- subset(lib_2013, lib_2013$STABR == "NY")
ny_lib_2013 <- ny_lib_2013[ny_lib_2013$LIBNAME %in% c("BROOKLYN PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY, THE BRANCH LIBRARIES", "QUEENS BOROUGH PUBLIC LIBRARY"), ]

#find intersection between column names 
x <- intersect(colnames(newlib), colnames(ny_lib_2013))

ny_lib_2013 <- subset(ny_lib_2013, select=x)
newlib_combo <- subset(newlib, select=x)
ny_lib_2013 <- as.data.frame(ny_lib_2013)
newlib_combo <- as.data.frame(newlib_combo)

#combine 
df_combo <- rbind(ny_lib_2013, newlib_combo)


df_combo$ELMATEXP <- gsub(",", "", df_combo$ELMATEXP, fixed=TRUE)
df_combo$ELMATEXP <- gsub("$", "", df_combo$ELMATEXP, fixed=TRUE)
df_combo$TOTEXPCO <- gsub(",", "", df_combo$TOTEXPCO, fixed=TRUE)
df_combo$TOTEXPCO <- gsub("$", "", df_combo$TOTEXPCO, fixed=TRUE)



df_combo["e_expend"] <- as.numeric(df_combo$TOTEXPCO) / as.numeric(df_combo$ELMATEXP)


just_money <- as.data.frame(just_money)
just_money <- subset(df_combo, select=c("ELMATEXP", "TOTEXPCO", "PRMATEXP", "Year", "LIBNAME", "e_expend", "BKVOL", "BKMOB", "EBOOK"))
just_money["per_ebook"] <- as.numeric(just_money$ELMATEXP)/as.numeric(just_money$EBOOK)
just_money["test_books"] <- as.numeric(just_money$EBOOK)
just_money["test_exp"] <- as.numeric(just_money$ELMATEXP)
write.csv(just_money, "expend.csv")
#export to CSV to see which variables should be matched manually for 2014 file, which has completely different column names

write.csv(df_combo, "libraries.csv")

  #read in 2014 file 

lib_2014 <- read.csv("2014.csv")
lib_2014['Year'] <- "01/01/2013"
ny_lib_2014 <- subset(lib_2014, lib_2014$STABR == "NY")
ny_lib_2014 <- ny_lib_2014[ny_lib_2014$LIBNAME %in% c("BROOKLYN PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY", "NEW YORK PUBLIC LIBRARY, THE BRANCH LIBRARIES", "QUEENS BOROUGH PUBLIC LIBRARY"), ]


loc <- read.delim("NYCfac.csv", header=TRUE, sep=",")
#read in geo file 2014
head(loc)
bk_loc <- loc[loc$BoroCode== 3, ]

bk_loc <- bk_loc[grep("LIBRARY", bk_loc$FacName), ]



#geocode addresses 
library(ggmap)
bk_loc$FacAddress <- as.character(bk_loc$FacAddress)
bk_loc$Address <- paste(bk_loc$FacAddress, bk_loc$ZipCode, sep=", ")
bb <- geocode(bk_loc$Address, output=c("latlona"), source="google")
bk_loc <- cbind(bk_loc, bb)

write.csv(bk_loc, "bk_loc.csv")


#read in wifi data

wifi <- read.csv("wifi.csv")
head(wifi)
bk_for_merge <- subset(bk_loc, select=c("FacName", "lat", "lon", "ZipCode"))
library(tools)
bk_for_merge$FacName <- as.character(bk_for_merge$FacName)

text_name <- bk_for_merge$FacName
bk_for_merge$FacName <- toTitleCase(bk_loc$FacName)

text_name <- tolower(text_name)
text_name <- toTitleCase(text_name)
bk_for_merge$FacName <- text_name
bk_for_merge$Branch <- bk_for_merge$FacName
bk_for_merge$Branch <- gsub(" Library", "", fixed=TRUE, bk_for_merge$Branch)

#fix inconsistencies stemming from data clean
bk_for_merge$Branch <- gsub("Brooklyn Central", "Central Library", fixed=TRUE, bk_for_merge$Branch)
bk_for_merge$Branch <- gsub("Stone Ave", "Stone Avenue", fixed=TRUE, bk_for_merge$Branch)
bk_for_merge$Branch <- gsub("Mckinley", "McKinley", fixed=TRUE, bk_for_merge$Branch)

bk_for_merge$Branch <- as.factor(bk_for_merge$Branch)
wifi$Branch <- wifi$Branch..group.
new_wifi <- merge(x=wifi, y=bk_for_merge, by="Branch")

present <- new_wifi$Branch
original <- wifi$Branch


#check for missing values 
wifi$Branch[!wifi$Branch %in% new_wifi$Branch]

new_wifi$Branch..group. <- NULL

#calcualte yearly sums 

new_wifi$sum_2014 <- sum(new_wifi[, (14:15)])
new_wifi$sum_2015 <- rowSums(new_wifi[, 14:26])


#read in density file 

density <- read.csv("bis_density.csv")
head(density)

#merge density with wifi data 
new_wifi$Zipcode <- new_wifi$ZipCode
density_wifi <- merge(x=new_wifi, y=density, by="Zipcode")
write.csv(density_wifi, "clean_wifi.csv")

#calculate percentage change for periods 

time <- ts(data.frame(x1=c(3:14), x2=c(15:26), x=c(27:38)), start=2014, frequency=12)


#create TS for all neighborhoods 
all <- density_wifi[, (3:38)]
time_period <- names(all)
all_sums <- colSums(all, na.rm=TRUE)

df_all <- all_sums
df_all <- as.data.frame(df_all)
df_all.ts <- ts(df_all, frequency=12, start=2014)
df_all.ts.d <- decompose(df_all.ts)
df_all.ts
plot(df_all.ts.d)
##create subset of all crime by category 
crime_groups_melt <- crime_groups %>% group_by(Month_Year, Category) %>% tally()
##melt subset and set as DF
crime_groups_melt <- melt(crime_groups_melt, id=c("Month_Year", "Category", "nn"), na.rm=TRUE)
crime_groups_melt <- as.data.frame(crime_groups_melt)
##cast category to header 
crime_groups_melt <- dcast(crime_groups_melt, Month_Year ~ Category, value.var="nn")
crime_groups_melt <- as.data.frame(crime_groups_melt)


#read in total_wifi to calculate BPL share of total free wifi in BK 

total_Wifi <- read.csv("total_wifi.csv")


#read in teen trends 

teen_trends <- read.csv("teen_trends.csv")
