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
head(my.df, 20)

### Clean and Transorm your Data ###
# I often need to transorm data that is stored as factor or character to numeric
# For example, if there is a "?" in one cell, it will be stored as non numeric
my.df$col2Numeric <- as.numeric(as.character(my.df$col2)) # Transforms

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
# http://www.r-bloggers.com/pivot-tables-in-r/
library(reshape2)
dcast(my.df, col4 ~ col1 + col3)

