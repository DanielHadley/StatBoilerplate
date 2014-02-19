# Boilerplate R Code 
# Created by Daniel Hadley, 2014
# These packages are used at various points: 
# install.packages("reshape2", )


### Loading Data  ### 
# A nifty trick to load data from your clipboard: 
myData <- read.delim("clipboard")
# Or from CSV: 
`myData` <- read.csv("C:/mypath/forward/slashes/myData.csv")
# But For this we will use randomly-generated reproduceable data 
my.df <- data.frame(col1 = sample(c(1,2), 10, replace = TRUE),
                    col2 = as.factor(sample(10)), col3 = letters[1:10],
                    col4 = sample(c(TRUE, FALSE), 10, replace = TRUE))

### Review your Data ###
View(my.df) 
summary(my.df)
names(my.df)
class(my.df)
sapply(my.df[1,],class)
str(my.df) #number of observations, number of vars, class of variables
head(my.df, 20)

### Clean and Transorm your Data ###
# I often need to transorm data that is stored as factor or character to numeric
# For example, if there is a "?" in one cell, it will be stored as non numeric
my.df$col2Numeric <- as.numeric(my.df$col2) # Transforms to numeric

# Missing Values
missing <- is.na(my.df$col1) 
sum(missing) #Number of missing values
sum(!missing) #Number of non-missing values

#OR
good <- complete.cases(my.df$col1)
sum(!good) #Number of missing values
sum(good) #Number of non-missing values

# Dropping (removing)
remove(good) #object
remove(my.df) #dataframe

# To drop variable/column:
my.df$col1 <- NULL #column OR
my.df <- subset(my.df, select = -c(col1)) #OR

# To drop observations with a given value:
my.df <- subset(my.df, col3 %in% c("a","b"))

# I usually make a "1" column to make tabulations easier
my.df$Tab <- 1

# Date
# Suppose you had a column with a date
my.df$Date <- sample(c("1/1/2013", "3/20/2013", "6/22/2014"), 10, replace = TRUE) # make column
my.df$Date <- as.Date(my.df$Date,"%m/%d/%Y") # Tell R it's a date
my.df$Month <- format(my.df$Date, format='%m') # Break it into month, day, year...
my.df$Day <- format(my.df$Date, format='%d')
my.df$Year <- format(my.df$Date, format='%Y')
my.df$Month <- as.numeric(as.character(my.df$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
my.df$Season <- ifelse((my.df$Month >= 3) & (my.df$Month <= 5), "Spring", 
                       ifelse((my.df$Month >= 6) & (my.df$Month <= 8), "Summer",
                              ifelse((my.df$Month >= 9) & (my.df$Month <= 11), "Fall", "Winter")))



# How to switch from Excel: the pivot table
aggregate(col1 ~ col4, my.df, mean ) # makes a two-way table

# aggregate works for a couple of variables. 
# "Cast" from reshape2 works when you have more than two variables:
# http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/
library(reshape2)
data.m <- melt(my.df, id=c(3:4), measure=c(2,5)) # id = non-numeric; measure = numeric
data.c <- dcast(data.m, col4 ~ variable, sum)

# Export
write.csv(my.df, file = "mydf.csv")


###  Visualize ###
library(ggplot2)

my.theme <- 
  theme(plot.background = element_blank(), # Remove background
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.ticks = element_blank(), # Remove axis ticks
        axis.text=element_text(size=24), # Enlarge axis text font
        axis.title=element_text(size=26), # Enlarge axis title font
        plot.title=element_text(size=32, hjust=0)) # Enlarge, left-align title


p <- qplot(Year, data=data.m, geom="bar", fill=Gender, alpha=I(.7), main="Youth Drug ODs", ylab="ODs")
p + my.theme


# Map it!
# First I add a column with Somerville addresses
my.df$Address <- sample(c("Highland AVe @ Somerville Ave", "41 Beacon St", "Weird St"), 10, replace = TRUE)
addresses <- paste(my.df$Address, "Somerville", "MA", sep=", ")

# Geocodes using the Google engine
library(ggmap) 
locs <- geocode(addresses)
locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# I map locs2 because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps

map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14)
SHmap + geom_density2d(
  aes(x=locs2$lon, y=locs2$lat,  
      fill = ..level.. , alpha = ..level..),size = 1.5, bins = 26, color="red", 
  data = locs2) 

map.center <- geocode("Highland Ave @ School St Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 13)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 10, alpha = .7, bins = 26, color="red", 
  data = locs2) 
