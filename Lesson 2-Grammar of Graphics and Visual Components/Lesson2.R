'
Lesson2: Grammar of graphics and visual components
This code contain Exercies and Activities for Lesson 2

Prior to use, install the following packages:
install.packages("ggplot2")

install.packages("ggplot2")
install.packages("tibble")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("Lock5Data")

Used datafiles and sources:
a) gapminder.csv - Modified dataset from various datasets available at:
https://www.gapminder.org/data/
b) xAPI-Edu-Data.csv:
https://www.kaggle.com/aljarah/xAPI-Edu-Data/data
c) LoanStats.csv:
Loan Data from Lending Tree - https://www.lendingclub.com/info/download-data.action

'
#Load Libraries
library("ggplot2")
library("tibble")
library("gridExtra")
library("dplyr")
library("Lock5Data")

#Set pathname for the directory where you have the lesson in
setwd(".")
#Check working directory
getwd()

#Load the data files
df <- read.csv("data/gapminder-data.csv")
df2 <- read.csv("data/xAPI-Edu-Data.csv")
df3 <- read.csv("data/LoanStats.csv")


#Summary of the three datasets
str(df)
str(df2)
str(df3)

#Exercise - Layers
p1 <- ggplot(df,aes(x=Electricity_consumption_per_capita))
p2 <- p1+geom_histogram()
p3 <- p1+geom_histogram(bins=15)
p4 <- p3+xlab("Electricity consumption per capita")
p4

#Exercise- Scales
#Where does the maximum occur? We need to have a finer labelling to answer
#the question
p1 + scale_x_continuous(breaks=seq(0,40000,4000) )

#Coordinates - Differentiating between cartesian and polar coordinates.
#Generate some random time numbers
t <- seq(0, 360, by=15)
r <- 2
qplot(r,t)+coord_polar(theta="y")+scale_y_continuous(breaks=seq(0,360,30))

#Activity A
p <- ggplot(df, aes(x=gdp_per_capita, y=Electricity_consumption_per_capita)) +
  geom_point() +
  scale_x_continuous(name="GDP",breaks = seq(0,50000,5000),
                     labels=scales::unit_format("K", 1e-3)) +
  scale_y_continuous(name="Electricity Consumption",
                     breaks = seq(0,20000,2000),
                     labels=scales::unit_format("K", 1e-3))
p



#Facet

#Exercise
dfs <- subset(df,Country %in% c("Germany","India","China","United States"))
p <- ggplot(df, aes(x=gdp_per_capita, y=Electricity_consumption_per_capita)) + 
  geom_point()
p + facet_grid(Country ~ .) #Horizontally Arranged
p + facet_grid(. ~ Country) #Vertically Arranged
p + facet_wrap(Country ~ .)

#Topic C: Visual components - Color and shape Differentiated

#Exercise - Using colors in plots
var1<-"Electricity_consumption_per_capita"
var2<-"gdp_per_capita"
name1<- "Electricity/capita"
name2<- "GDP/capita"
# Change color and shape of points
p1<- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(color=2,shape=2)+xlim(0,10000)+xlab(name1)+ylab(name2)
#Grouping points by a variable mapped to colour and shape
p2 <- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(aes(color=Country,shape=Country))+xlim(0,10000)+xlab(name1)+ylab(name2)
grid.arrange(p1, p2, nrow = 2)

#Exercise
#Boxplot - color differentiated.
ggplot(df2,aes(x=GradeID,y=VisitedResources))+geom_boxplot(aes(fill=Class))

#Activity C:
#color differentiate with credit grade.
dfn <- df3[,c("home_ownership","loan_amnt","grade")]
dfn <- na.omit(dfn)
dfn <- subset(dfn, !dfn$home_ownership %in% c("NONE"))
ggplot(dfn,aes(x=home_ownership,y=loan_amnt))+geom_boxplot(aes(fill=grade))
#People with higher credit grades take smaller loans
#People with lower credit grades take small loans if they don't have a mortgage.

#Themes
#Exercise
dfn <- subset(HollywoodMovies, Genre %in% c("Action","Adventure","Comedy","Drama","Romance")
              & LeadStudio %in% c("Fox","Sony","Columbia","Paramount","Disney"))
p1 <- ggplot(dfn,aes(Genre,WorldGross)) 
p2 <- p1+geom_bar(stat="Identity",aes(fill=LeadStudio),position="dodge")
p3 <- p2+theme(axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    plot.background=element_rect(fill="gray87"),
    panel.background = element_rect(fill="beige"),
    panel.grid.major = element_line(color="Gray",linetype=1)
    )

#Using predefined themes - Just show slide
p4 <- p2+theme_bw()+ggtitle("theme_bw()")
p5 <- p2+theme_classic()+ggtitle("theme_classic()")
p6 <- p2+theme_classic()+ggtitle("theme_gray()")
p7 <- p2+theme_minimal()+ggtitle("theme_minimal()")
grid.arrange(p4,p5,p6,p7,nrow=2,ncol=2)

#Exercise : Create your own theme
mytheme <- theme(legend.title = element_blank(),
  legend.position ="bottom",
  text = element_text(colour="Blue"),
  axis.text = element_text(size=12,color="Red"),
  axis.title = element_text(size = rel(1.5)))
p2 <- p2+ggtitle("Original Plot")
p8 <- p2+mytheme+ggtitle("Changed Plot with my theme")
p8
grid.arrange(p2,p8,ncol=2)

#Geoms and statistics
#density curve

#Summarizing
#Mean, Median, Mode, Quartiles



