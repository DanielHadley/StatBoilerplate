# Boilerplate R Code 
# Created by Daniel Hadley, 2014. Modified 2015
# These packages are used at various points: 
# install.packages("dplyr", "tidyr", "ggplot2", "ggmap", "scales", "lubridate")

# For most operations I defer to the dplyr cheatsheet 
# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

library(dplyr) # data manipulation
library(tidyr) # a few pivot-table functions
library(ggplot2) # plotting  
library(scales) # works with ggplot2 to properly label axes on plots
library(ggmap) # making maps, duh
library(lubridate) # dealing with dates

# Set working director - this is where you will save plots and other files
setwd("C:/Users/dhadley/Documents/GitHub/StatBoilerplate")


#### Loading Data  #### 
# A nifty trick to load data from your clipboard: 
# d <- read.delim("clipboard")
# Or from CSV: 
# `myData` <- read.csv("C:/mypath/forward/slashes/myData.csv")

# But For this we will use randomly-generated data
# d = my dataframe. I use the letter d instead of something more descriptive because 
# I ended up typing it so often.
set.seed(123) # so random numbers are the same each time

d <- data.frame(col1 = sample(c(1:50), 20, replace = TRUE),
                col2 = as.factor(sample(20)),
                col3 = sample(c(TRUE, FALSE), 20, replace = TRUE))

# Make a money column to show how GSUB works
d$Money <- sample(c("$100", "$3,200", "$311"), 20, replace = TRUE)

# This adds a column with Somerville addresses for the mapping portion
d$Address <- sample(c("Highland AVe @ Somerville Ave", "41 Beacon St", "27 Broadway"), 20, replace = TRUE)

# And Finally I add a date column to the df for dat transformations
d$Date <- sample(c("1/1/2013", "3/20/2013", "6/22/2014"), 20, replace = TRUE) # make column




#### Review your Data ####
summary(d)
names(d)
str(d) #number of observations, number of vars, class of variables
head(d, 20)


#### Clean and Transorm your Data ####
# I often need to transorm data that is stored as factor or character to numeric
# For example, if there is a "?" in one cell, it will be stored as non numeric
# So I gsub the non-numeric characters and then transform to numeric
# Notice the pipe operator (%>%). You'll see this a lot.
# It means take the thing on the left and do this to it
d <- d %>%
  mutate(col2 = as.numeric(as.character(col2))) %>%  # Transforms to numeric
  mutate(Money = as.numeric(gsub("([/$,])", "", Money))) # Take out $ and , and turn to numeric
  

# Excel users often want to change the value in one cell
# To do that in R, we use the index (stuff in the [brackets])
# data[row, column]
d[3,2] <- 33


# Tell R it's a date and then make new variables
# Using Lubridate
d <- d %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"),
         Month = month(Date),
         Day = day(Date),
         Year = year(Date),
         WeekDay = wday(Date)
         )
  
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
d$Season <- ifelse((d$Month >= 3) & (d$Month <= 5), "Spring", 
                       ifelse((d$Month >= 6) & (d$Month <= 8), "Summer",
                              ifelse((d$Month >= 9) & (d$Month <= 11), "Fall", "Winter")))


# Often you want to see your data arranged
# In the newest version of RStudio, you can also do this with the GUI
arrange(d, Date) # Ascending
arrange(d, desc(Date)) #Descending
d <- arrange(d, desc(Date)) # Do it to the dataframe





#### How to switch from Excel: the pivot table ####
# Usually we use group_by and summarise to make tables

# The absolute easiest
MoneyByTrue <- d %>%
  group_by(col3) %>%
  summarise(TotalMoney = sum(Money))

# Add Average and arrange it at the end
MoneyBySeason <- d %>%
  group_by(Season) %>%
  summarise(TotalMoney = sum(Money),
            AverageMoney = mean(Money)) %>%
  arrange(TotalMoney)

# Records by Date
ObservationsByDate <- d %>%
  group_by(Date) %>%
  summarise(Observations = n()) #Just counts everything

# Kind of like a sumif function in Excel 
FalsesByDate <- d %>%
  group_by(Date) %>%
  summarise(Falses = sum(col3 =="FALSE"),
            Trues = sum(col3 == "TRUE"))
  

# You can export any of these
# write.csv(d, file = "mydf.csv")





####  Visualize ####
# I add some colors and my.theme to micromanage how the plots look
# These lines are not neccessary, but can really make plots look better

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"


my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    # panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )



## Ggplot Magic
# It seems harder than excel, but it just takes some getting used to
# Most basic plots are easy. I'm making some hard ones to demonstrate what is possible:

# This plot probably requires the most tweaking
# It has seasons and money
# In the first line, I specify the order that I want the seasons to appear in the plot
MoneyBySeason$Season <- factor(MoneyBySeason$Season, levels=c("Winter", "Spring", "Summer", "Fall"))
# Now we make the plot
ggplot(MoneyBySeason, aes(x=Season, y=TotalMoney)) + 
  geom_bar(stat="identity", fill=nice_blue) + 
  my.theme + ggtitle("Money By Season") + 
  xlab("Season") + ylab("Total Money Collected") + 
  scale_y_continuous(labels = dollar) # This turns it back into $ money for the plot
# Uncomment the next line to save to your current directory
ggsave("./plots/2015_MoneyBySeason.png", dpi=250, width=5, height=3)


# Plot FalsesByDate
# I first transform into long format to put them side by side
data_long <- FalsesByDate  %>% 
  gather(TorF, value, Trues:Falses)
# Now we make the plot
ggplot(data_long, aes(x=Date, y=value, fill=TorF)) + 
  geom_bar(stat="identity", position="dodge") +
  my.theme + ggtitle("True or False Over Time") + xlab("Time") +
  ylab("True or False") + 
  scale_x_date(breaks = "1 month", labels=date_format("%B %Y"))
# Uncomment the next line to save to your current directory
ggsave("./plots/2015_FalsesByDate.png", dpi=250, width=5, height=3)


# Plot FalsesByDate but without the missing dates
# I first transform into long format to put them side by side
data_long <- FalsesByDate  %>% 
  gather(TorF, value, Trues:Falses) %>%
  mutate(Date = as.factor(Date)) # Just turn this into a factor
# Now we make the plot
ggplot(data_long, aes(x=Date, y=value, fill=TorF)) + 
  geom_bar(stat="identity", position="dodge") +
  my.theme + ggtitle("True or False By Date") + xlab("Date") +
  ylab("True or False") 
# Uncomment the next line to save to your current directory
ggsave("./plots/2015_FalsesByDate2.png", dpi=250, width=5, height=3)




###### Map it! ######
d$FullAddress <- paste(d$Address, "Somerville", "MA", sep=", ")

# Geocodes using the Google engine
locs <- geocode(d$FullAddress)
#d <- bind_cols(d, locs) # Add the lat and long back to d
# ^ Didn't work, so
d$lon <- locs$lon
d$lat <- locs$lat

# Optional: delete ones that were not geocoded properly
# I do this because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps
d <- subset(d, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin


# Dot map centered on Conway Park
map.center <- geocode("Conway Park, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 16)
SHmap + geom_point(
  aes(x=d$lon, y=d$lat),size = 10, alpha = .7, bins = 26, color="red", 
  data = d) 

# More traditional heat map
Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% d + aes(x = d$lon, y = d$lat) +
  # geom_density2d(data = d, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = d, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12))


# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill")

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) +
    geom_point(data=d,size=4,
              aes(x=lon,y=lat,color=as.factor(Year)))+
    labs(x="",y="") +
    theme(axis.text=element_blank(),axis.ticks=element_blank()) +
    ggtitle(neighborhoodList[n])
  
  ggsave(paste("./plots/map_",neighborhoodList[n], ".png", sep=""), dpi=250, width=6, height=5)
  
}

