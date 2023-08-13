rm(list=ls())
# library(Hmisc) # first install this library using install.packages("Hmisc")
library("ggplot2")
library("graphics")
library("datasets")

options(width.cutoff=150)
# for data set death 
death <- read.csv("~/NMIT BANGALORE/4th Semester/R/DeathAnalysisData/death.csv")
View(death)
#changing big col names to small col names
colnames(death)<-c("Country", "Code", "Year", "Executions", "Meningitis", "Neoplasm", "Fire", "Malaria", "Drown", "Voilence", "Hiv/Aids", "Drugs", "Tuberculosis", "RoadAccidents", "Pregnencies", "Respiratory", "Alcohol", "NutritionalDeficiencies", "Suicide", "Poisoning", "Terrorists", "LiverDisease", "AlzheimerDisease", "ParkinsonDisease")

# to arrange the death frame according to the year
# death<-death[order(death$Year),]
View(death)

# Finding mean deaths for each country for all Causes in death dataset
countries<-unique(death$Country)
years<-unique(death$Year)
print(years)
print(countries)
coun<-c()
y<-c()
means<-c()
for(country in countries)
{
  for(year in years)
  {
    # taking out mean of deaths in each year for each country
    subsetData<-subset(death, Country==country & Year==year)
    if(nrow(subsetData)>0)
    {
      rowMean<-apply(subsetData[sapply(subsetData, is.numeric)], 1, mean, na.rm=TRUE)
      coun<-c(coun, country)
      y<-c(y, year)
      means<-c(means, rowMean)
    }
    # We cannot make a data frame as not all countries have year death from 1996 to 2017
  }
}
frameData<-data.frame(
  Country=coun,
  Year=y,
  MeanCause=means
)
frameData
# inputing country name from user
userCountry<-readline("Enter the country name whose bargraph needs to be plot\n")
subsetData <- subset(frameData, Country == userCountry)
subsetData
# making bar-plot for each country year vs mean death
barplot(subsetData$MeanCause, 
        names.arg = subsetData$Year,
        main=paste("Graph for ",userCountry),
         xlab = "Year",
         ylab = "Mean Cause",
         col = "purple",
         horiz = FALSE)


# Calculating the death rates for each country for every cause
coun<-c()
cause<-c()
r<-c()
for(country in countries)
{
  subsetData<-subset(death, Country==country)
  s=0
  for(col in 4:ncol(subsetData))
  {
    if(is.numeric(subsetData[, col]) && nrow(subsetData)>0)
    {
      s=s+sum(subsetData[,col])
    }
  }
  for(column in 4:ncol(subsetData))
  {
    subsetData<-subsetData[!is.na(subsetData[, column]),]
    if(is.numeric(subsetData[, column]) && nrow(subsetData)>0)
    {
      sumDeaths<-sum(subsetData[, column])
      rate<-(sumDeaths/s)*100
      coun<-c(coun, country)
      cause<-c(cause, colnames(subsetData)[column])
      r<-c(r, rate)
    }else{
      next
    }
  }
}

frameData<-data.frame(
  Country=coun,
  Cause=cause,
  Rate=r
)
frameData
# making plots for user input country. The graph will be Cause vs Rate Of Death
userCountry<-readline("Enter the country name : ")
subsetData<-subset(frameData, Country==userCountry)
if(nrow(subsetData)==0)
{
  cat("No such country is present")
}else{
  naValues<-which(is.na(subsetData$Rate))
  if(length(naValues)>0)
  {
    cat("There are some NA values present")
  }
  # Excluding the NA values
  subsetData<-subsetData[!is.na(subsetData$Rate),]
  if(nrow(subsetData)==0)
  {
    cat("No set is present, all are NA values")
  }else{
    subsetData$Rate<-as.numeric(subsetData$Rate)
    ggplot(subsetData, aes(x = Cause, y = Rate, fill = Cause)) +
      geom_bar(stat = "identity") +
      xlab("Cause") +
      ylab("Rate") +
      ggtitle(paste("Death Rates in", userCountry))+
      theme(plot.title = element_text(hjust = 0.5),     # Center the title
            axis.text.x = element_text(angle = 45, hjust = 1))  # Set the width of the plot (adjust the value as needed)
  }
}


# finding out which cause has maximum mean for each country
coun<-c()
storeMeans<-c()
causeName<-c()
for(country in countries)
{
  colName=""
  meanVal=0
  subsetData<-subset(death, Country==country)
  for(column in 4:ncol(subsetData))
  {
    if(is.numeric(subsetData[,column]))
    {
      meanValue<-mean(subsetData[,column])
      subsetData[,column]<-ifelse(is.na(subsetData[,column]), 0, subsetData[,column])
      if(meanValue>meanVal && !(is.na(meanValue)))
      {
        meanVal=meanValue
        colName=colnames(subsetData)[column]
      }
    }
  }
  storeMeans<-c(storeMeans, meanVal)
  coun<-c(coun, country)
  causeName<-c(causeName, colName)
}
frameData<-data.frame(
  coun,
  causeName,
  storeMeans
)
frameData
colnames(frameData)<-c("Country", "Cause", "Mean")
frameData
userCause<-readline("Enter the Cause to get graph plot : ")
subsetData<-subset(frameData, Cause==userCause)
if(nrow(subsetData)==0)
{
  cat("No such cause has the highest death rate.")
}else{
  # plotting histogram graph
  ggplot(subsetData, aes(x=Country, y=Mean, fill=Country))+
    geom_bar(stat="identity", position = "dodge", color = "black", alpha = 0.8)+
    labs(x="Country",
         y="Mean Deaths",
         title=paste("Graph for ",userCause),
         fill="category")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



# Each country's highest death rate Cause
coun<-c()
cause<-c()
rate<-c()

for(country in countries)
{
  if(country!="World")
  {
    Country=""
    deathCause=""
    r=0
    subsetData<-subset(death, Country==country)
    s=0
    for(col in 4:ncol(subsetData))
    {
      if(is.numeric(subsetData[, col]) && nrow(subsetData)>0)
      {
        s=s+sum(subsetData[,col])
      }
    }
    for(column in 4:ncol(death))
    {
      if(length(subsetData[, column])>0 && is.numeric(subsetData[,column]))
      {
        sumData<-sum(subsetData[,column])
        rateData<-(sumData/s)*100
        if(!(is.na(rateData)) && rateData>r)
        {
          r<-rateData
          Country<-country
          deathCause<-colnames(subsetData)[column]
        }
      }
      else
      {
        next
      }
    }
    coun<-c(coun, Country)
    cause<-c(cause, deathCause)
    rate<-c(rate, r)
  }
}
frameData<-data.frame(
  Cause=cause,
  Country=coun,
  Rate=rate
)

# to give more space between the columns of the data frame
options(width.cutoff=150)
frameData
# Using frequency polygon graph
userData<-readline("Enter the cause whose box plot needs to be shown : ")
subsetData<-subset(frameData, Cause==userData)
ggplot(subsetData, aes(x = Country, y = Rate, fill = Rate)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Rate") +
  ggtitle(paste("Death Rates for ", userData))+
  theme(plot.title = element_text(hjust = 0.5),     # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1))  # Set the width of the plot (adjust the value as needed)




# information about some particular causes their peek status and their down fall for each country

# using gather function
# install.packages('tidyr')
library("tidyr")
userCountry<-readline("Enter Country : ")
userCause<-readline("Enter Cause : ")
subsetData<-subset(death, Country==userCountry)
subsetData
deaths<-c()
for(col in colnames(subsetData))
{
  if(col==userCause)
  {
    deaths<-c(subsetData[col])
  }
}
dataFrame<-data.frame(
  Year=subsetData$Year,
  Death=deaths
)
dataFrame
ggplot(dataFrame, aes(x = Year, y=dataFrame[, 2])) +
  geom_point(color="red")+
  geom_line(color="blue")+
  scale_color_viridis_c() +
  theme_bw() +
  labs(title = "Rainbow-colored Line Plot",
       x = "Years",
       y = paste(userCause),
       color = "Graph")
