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

#Set pathname for the directory where you have data
setwd(".")
#Check working directory
getwd()

#Note: Working directory should be "Beginning-Data-Visualization-with-ggplot2-and-R"

#Load the data files
df <- read.csv("../data/gapminder-data.csv")
df2 <- read.csv("../data/xAPI-Edu-Data.csv")
df3 <- read.csv("../data/LoanStats.csv")


#Summary of the three datasets
str(df)
str(df2)
str(df3)

#Subtopic - Layers
p1 <- ggplot(df,aes(x=Electricity_consumption_per_capita))
p2 <- p1+geom_histogram()
p3 <- p1+geom_histogram(bins=15)
p3

#Exercise-Layers
p4 <- p3+xlab("Electricity consumption per capita")
p4

#Exercise- Scales
p1 <- ggplot(df,aes(x=gdp_per_capita))
p2 <- p1+geom_histogram()
p2
#Where does the maximum occur? We need to have a finer labelling to answer
#the question
p2 + scale_x_continuous(breaks=seq(0,40000,4000) )


#Subtopic - Coordinates
#Exercise: Understanding polar coordinates.

#Generate some random time numbers
t <- seq(0, 360, by=15)
r <- 2
qplot(r,t)
qplot(r,t)+coord_polar(theta="y")
qplot(r,t)+coord_polar(theta="y")+scale_y_continuous(breaks=seq(0,360,30))

#Activity A:Applying Grammar of graphics to create a complex visualization
pA <- ggplot(df, aes(x=gdp_per_capita, y=Electricity_consumption_per_capita)) +
  geom_point() +
  scale_x_continuous(name="GDP",breaks = seq(0,50000,5000),
                     labels=scales::unit_format("K", 1e-3)) +
  scale_y_continuous(name="Electricity Consumption",
                     breaks = seq(0,20000,2000),
                     labels=scales::unit_format("K", 1e-3))
pA


#Topic B: Facet
#Exercise: Using facets to split data
p <- ggplot(df, aes(x=gdp_per_capita, y=Electricity_consumption_per_capita)) + 
  geom_point()
p + facet_grid(Country ~ .) #Horizontally Arranged
p + facet_grid(. ~ Country) #Vertically Arranged
p + facet_wrap(~Country)


#Activity B:Using faceting to understand data
pb1<-ggplot(df3,aes(x=loan_amnt))
pb2<-pb1+geom_histogram(bins=10,fill="cadetblue4")

#Facet_wrap
pb3<-pb2+facet_wrap(~grade) 
#Free y coordinate for the subplots
pb4<-pb3+facet_wrap(~grade, scale="free_y")

#Topic C: Visual components - Color and shape Differentiated
#Topic C: Using and changing styles and colors
#Subtopic: using colors in plots

#Exercise - Using color to group points by variable
dfs <- subset(df,Country %in% c("Germany","India","China","United States"))
var1<-"Electricity_consumption_per_capita"
var2<-"gdp_per_capita"
name1<- "Electricity/capita"
name2<- "GDP/capita"
# Change color and shape of points
p1<- ggplot(df,aes_string(x=var1,y=var2))+
  geom_point(color=2,shape=2)+xlim(0,10000)+xlab(name1)+ylab(name2)
#Grouping points by a variable mapped to colour and shape
p2 <- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(aes(color=Country,shape=Country))+xlim(0,10000)+xlab(name1)+ylab(name2)
grid.arrange(p1, p2, nrow = 2)

#Exercise
#Boxplot - color differentiated.
ggplot(df2,aes(x=GradeID,y=VisitedResources))+geom_boxplot(aes(fill=Class))

#Activity C:Using color differentiation in plots
#color differentiate with credit grade.
dfn <- df3[,c("home_ownership","loan_amnt","grade")]
dfn <- na.omit(dfn)
dfn <- subset(dfn, !dfn$home_ownership %in% c("NONE"))
ggplot(dfn,aes(x=home_ownership,y=loan_amnt))+geom_boxplot(aes(fill=grade))
#People with higher credit grades take smaller loans
#People with lower credit grades take small loans if they don't have a mortgage.

#Finer labelling in y to answer 5c
ggplot(dfn,aes(x=home_ownership,y=loan_amnt))+geom_boxplot(aes(fill=grade))+
  scale_y_continuous(breaks=seq(0,40000,2000))

#Subtopic: Themes and changing the appearance of graphs

#Exercise:Using theme to customize a plot
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

#Exercise : Using or setting your own theme globally
mytheme <- theme(legend.title = element_blank(),
  legend.position ="bottom",
  text = element_text(colour="Blue"),
  axis.text = element_text(size=12,color="Red"),
  axis.title = element_text(size = rel(1.5)))
p2 <- p2+ggtitle("Original Plot")
p8 <- p2+mytheme+ggtitle("Changed Plot with my theme")
p8
grid.arrange(p2,p8,ncol=2)

#Exercise: Changing the color scheme of the given theme
p4 + scale_fill_brewer(palette="Spectral")
p4 + scale_fill_brewer(palette="Pastel1")
p4 + scale_fill_brewer(palette="Oranges")



#Activity C: Using themes and color differentiation in a plot
pd1 <- ggplot(df,aes(x=BMI_male,y=BMI_female))
pd2 <- pd1+geom_point()
pd3 <- pd1+geom_point(aes(color=Country),size=2)+
  scale_colour_brewer(palette="Dark2")
pd4 <- pd3+theme(axis.title=element_text(size=15,color="cadetblue4",
                                         face="bold"),
                 plot.title=element_text(color="cadetblue4",size=18,
                                         face="bold.italic"),
                 panel.background = element_rect(fill="azure",color="black"),
                 panel.grid=element_blank(),
                 legend.position="bottom",
                 legend.justification="left",
                 legend.title = element_blank(),
                 legend.key = element_rect(color=3,fill="gray97")
)+
  xlab("BMI Male")+
  ylab("BMI female")+
  ggtitle("BMI female vs BMI Male")

pd4

#Geoms and statistics
#Groupby and summarizing
#Group the movies by Genre
ggplot(HollywoodMovies,aes(Genre,AudienceScore))+geom_point()+theme(axis.text.x=element_text(angle=40))

gp_scr <- group_by(HollywoodMovies,Genre)

gp_scr <- na.omit(gp_scr)
#Calculate mean and SD for AudienceScore for a given Genre.
dfnew <- dplyr::summarise(gp_scr,as_mean=mean(AudienceScore),
                          as_sd=sd(AudienceScore),n=n())
#Print
dfnew

#We need to order according to AS Mean.
dfnew <- dfnew[order(dfnew$as_mean), ]

#Plot
pg1 <- ggplot(data = dfnew, aes(x=Genre,y=as_mean)) 
#Note: R orders the names alphabetically. We don't want that.

#Change the ordering of levels to keep Genres unordered.
dfnew$Genre <-factor(dfnew$Genre, levels = dfnew$Genre)


#New Plot 
pg2 <- ggplot(data = dfnew, aes(x=Genre,y=as_mean)) 
pg2 + geom_point()+
  geom_errorbar(color="red",aes(ymin=as_mean-(as_sd/sqrt(n-1)), ymax = as_mean+
                                  (as_sd/sqrt(n-1))))+ylab("Audiencescore Mean")+
  theme(axis.text = element_text(angle=90))
