'
Applied Data Visualization with ggplot2 - Lesson 1

Description: Codes and activities for Lesson1

Author:Tania Moulik (tania.moulik@gmail.com)

Prior to use install the packages: 
install.packages("ggplot2")
install.packages("tibble")
install.packages("dplyr")
install.packages("Lock5Data")

'
require("ggplot2")
require("tibble")
require("dplyr")
require("Lock5Data")

#Basic commands in R
#Set working directory to wherever you have your data
setwd(".")

#Check working directory
getwd()

#Data and variable investigation
data()
str(mtcars)
str(airquality)
str(rock)
str(sleep)

#Exercise: plotting with qplot and R
hist(airquality$Temp)
qplot(airquality$Temp)

#Topic B: Geomtric Objects

#Exercise: Load and view datasets

#Load the Humidity Data
df_hum <- read.csv("../data/historical-hourly-weather-data/humidity.csv")

#Read weather description data
df_desc <- read.csv("../data/historical-hourly-weather-data/weather_description.csv")

str(df_hum)
str(df_desc)

#Exercise: Creating a histogram using qplot and ggplot
qplot(df_hum$Vancouver) 
ggplot(df_hum$Vancouver)
ggplot(df_hum, aes(x=Vancouver)) + geom_histogram()

#Activity B-1:Creating a Histogram and Explaining its Features
df_t <- read.csv("data/historical-hourly-weather-data/temperature.csv")

ggplot(df_t,aes(x=Vancouver))+geom_histogram()
ggplot(df_t,aes(x=Miami))+geom_histogram()

#Subtopic : Creating Barcharts
glimpse(df_desc)
ggplot(df_desc,aes(x=Vancouver)) + geom_bar()

#Exercise: Create a 1-D barchart
ggplot(df_desc,aes(x=Seattle)) + geom_bar()

#Exercise:Create a 2D bar chart
ggplot(RetailSales,aes(x=Month,y=Sales)) + geom_bar(stat="identity")

#Note: months are not ordered.
#Order the months:
#Check levels
levels(RetailSales$Month)
#Check if it has NA vaues
is.na(RetailSales)
#It has NA values so remove them and create a new data set
MyRetailSales <- na.omit(RetailSales)
#Reorder the months
MyRetailSales$Month <-factor(MyRetailSales$Month, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#Plot
ggplot(MyRetailSales,aes(x=Month,y=Sales)) + geom_bar(stat="identity")


#Subtopic : Create a boxplot
#Get the months from the datetime variable and create a month column.
df_hum$datetime <- as.character(df_hum$datetime)
df_hum$month <- substr(df_hum$datetime,6,7)

#Display
ggplot(df_hum,aes(x=month,y=Vancouver)) + geom_boxplot()

#Exercise - Create a boxplot
ggplot(df_hum,aes(x=month,y=Seattle)) + geom_boxplot()
ggplot(df_hum,aes(x=month,y=San.Francisco)) + geom_boxplot()

#Subtopic: Scatterplot
a=3.4
v0=27

#Generate some random time numbers
time <- runif(50, min=0, max=200)
distance <- sapply(time, function(x) v0*x + 0.5*a*x^2)
df <- data.frame(time,distance)
ggplot(df,aes(x=time,y=distance)) + geom_point()

ggplot(df,aes(x=time,y=distance)) + geom_line()

#Exercise - Create a Linechart
df_hum$monthn <- as.numeric(df_hum$month)
gp1 <- group_by(df_hum,monthn)
gp1 <- na.omit(gp1)
dfgp1 <- dplyr::summarise(gp1,med = median(Vancouver),
                 mean=mean(Vancouver),sd=sd(Vancouver),n=n())
ggplot(data = dfgp1, aes(x=monthn,y=mean)) + geom_line() + xlab("Month") + 
  ylab("Mean Humidity")

#Activity B-2 - Creating One- and Two-Dimensional Visualizations with a Given Dataset
df_edu <- read.csv("data/xAPI-Edu-Data.csv")
str(df_edu)

#Functions for Plotting a barchart/Histogram
plotbar <- function(df,mytxt) {
  ggplot(df,aes_string(x=mytxt)) + geom_bar()
}

plothist <- function(df,mytxt) {
  ggplot(df,aes_string(x=mytxt)) + geom_histogram()
}

#Alternatively one can use a function to plot but students can just
#do it directly at this point.
#1-D Plots
plotbar(df_edu,"Topic")
plotbar(df_edu,"gender")
plotbar(df_edu,"ParentschoolSatisfaction")
plothist(df_edu,"VisitedResources")

#2-D Plots
ggplot(df_edu,aes(x=Topic,y=VisitedResources)) + geom_boxplot()
ggplot(df_edu,aes(x=AnnouncementsView,y=VisitedResources)) + geom_point()
ggplot(df_edu,aes(x=gender,y=Discussion)) + geom_boxplot()



#Grammar of Graphics (Changing the defaults using layered structure)
#Subtopic: Understanding and using grammar of graphics

#Rebinning
ggplot(df_hum,aes(x=Vancouver))+geom_histogram()+ggtitle("Default Binning")
ggplot(df_hum,aes(x=Vancouver))+geom_histogram(bins=15)+ggtitle("Rebinned")

#Improve plot by chaging defaults
ggplot(df_hum,aes(x=Vancouver))+
                    geom_histogram(bins=15,fill="white",color=1)+
                    ggtitle("Humidity for Vancouver city")+
                    xlab("Humidity")+
                    theme(axis.text.x=element_text(size = 12),
                          axis.text.y=element_text(size=12))

#Checking the theme options
?theme

#Exercise - Create a improved Boxplot                
ggplot(df_hum,aes(x=month,y=Vancouver)) + geom_boxplot()

#Improved Boxplot
ggplot(df_hum,aes(x=month,y=Vancouver)) + 
  geom_boxplot(color=1,fill=3) + 
  ylab("Humidity")+ 
  theme(axis.text.y=element_text(size=15),
  axis.text.x=element_text(size = 15),
  axis.title.x=element_text(size=15,color=2),
  axis.title.y=element_text(size=15,color=2))

#Activity C - Improving the Default Visualization

p1 <- ggplot(df_edu,aes(x=Topic))
p2 <- ggplot(df_edu,aes(x=VisitedResources))

p1 + 
  geom_bar(color=1,fill=3) + 
  ylab("Count")+ 
  theme(axis.text.y=element_text(size=10),
        axis.text.x=element_text(size = 10),
        axis.title.x=element_text(size=15,color=4),
        axis.title.y=element_text(size=15,color=4))+
  ggtitle("Topics in Education data")

p2 + 
  geom_histogram(bins=20,fill="white",color=1)+
  ggtitle("Visited Resources for Education data")+
  xlab("Visited Resources")+
  theme(axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=15,color=4),
        axis.title.y=element_text(size=15,color=4))
