require("ggplot2")
require("tibble")

#Basic commands in R
#Set working directory to wherever you have your data
setwd(".")
#Check working directory
getwd()

#Load a data file - Read the Humidity Data
df_hum <- read.csv("data/historical-hourly-weather-data/humidity.csv")

#Display the summary
str(df_hum)

#Display the column names
colnames(df_hum)

#Number of columns and rows
ndim(df_hum)

#Data and variable investigation
data()
str(Titanic)
str(mtcars)
str(airquality)
str(rocks)
str(sleep)

#First plot
hist(df_hum$Vancouver) 
qplot(df_hum$Vancouver)

#Read weather description data
df_desc <- read.csv("data/historical-hourly-weather-data/weather_description.csv")

str(df_hum)
str(df_desc)

#SubTopic : Create one and two-dimensional objects 

#Histogram using ggplot
ggplot(df_hum,aes(x=Vancouver))+geom_histogram()

#Activity B : Create a histogram for Temperature data
df_t <- read.csv("data/historical-hourly-weather-data/temperature.csv")
hist(df_t$Vancouver) 
qplot(df_t$Vancouver)
ggplot(df_t,aes(x=Vancouver))+geom_histogram()
ggplot(df_t,aes(x=Seattle))+geom_histogram()

#Barchart

glimpse(df_desc)
ggplot(df_desc,aes(x=Vancouver)) + geom_bar()


#Exercise: Create a barchart
ggplot(df_desc,aes(x=Seattle)) + geom_bar()

#Create a boxplot

#Get the months from the datetime variable and create a month column.
df_hum$datetime <- as.character(df_hum$datetime)
df_hum$month <- substr(df_hum$datetime,6,7)

ggplot(df_hum,aes(x=month,y=Vancouver)) + geom_boxplot()

#Exercise - Create a boxplot
ggplot(df_hum,aes(x=month,y=Seattle)) + geom_boxplot()
ggplot(df_hum,aes(x=month,y=San.Francisco)) + geom_boxplot()

#Scatterplot
a=3.4
v0=27
#time <- seq(from = 0, to = 200, by = 20)
#Generate some random time numbers
time <- runif(50, min=0, max=200)
distance <- sapply(time, function(x) v0*x + 0.5*a*x^2)
df <- data.frame(time,distance)
ggplot(df,aes(x=time,y=distance)) + geom_point()

#Exercise - Create a Linechart
df_hum$monthn <- as.numeric(df_hum$month)
gp1 <- group_by(df_hum,monthn)
gp1 <- na.omit(gp1)
dfgp1 <- dplyr::summarise(gp1,med = median(Vancouver),
                 mean=mean(Vancouver),sd=sd(Vancouver),n=n())
ggplot(data = dfgp1, aes(x=monthn,y=mean)) + geom_line() + xlab("Month") + 
  ylab("Mean Humidity")

#Activity C - Dataset
df_edu <- read.csv("data/xAPI-Edu-Data.csv")
str(df_edu)
plotbar(df_edu,"Topic")
plotbar(df_edu,"gender")
plotbar(df_edu,"ParentschoolSatisfaction")
plothist(df_edu,"VisITedResources")
ggplot(df_edu,aes(x=Topic,y=VisITedResources)) + geom_boxplot()
ggplot(df_edu,aes(x=AnnouncementsView,y=VisITedResources)) + geom_point()
ggplot(df_edu,aes(x=gender,y=Discussion)) + geom_boxplot()

#Functions for Plotting a barchart/Histogram
plotbar <- function(df,mytxt) {
  ggplot(df,aes_string(x=mytxt)) + geom_bar()
}

plothist <- function(df,mytxt) {
  ggplot(df,aes_string(x=mytxt)) + geom_histogram()
}

#Grammar of Graphics (Changing the defaults using layeres structure)

#Rebinning
ggplot(df_hum,aes(x=Vancouver))+geom_histogram(bins=15)

#Improve the plot
ggplot(df_hum,aes(x=Vancouver))+
                    geom_histogram(bins=15,fill="white",color=1)+
                    ggtitle("Humidity for Vancouver city")+
                    xlab("Humidity")+
                    theme(axis.text.x=element_text(size = 12),
                          axis.text.y=element_text(size=12))

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

#Activity D - Improve the visualizations using grammar of graphics
p1 <- ggplot(df_edu,aes(x=Topic))
p2 <- ggplot(df_edu,aes(x=VisITedResources))

#Improved plot
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


