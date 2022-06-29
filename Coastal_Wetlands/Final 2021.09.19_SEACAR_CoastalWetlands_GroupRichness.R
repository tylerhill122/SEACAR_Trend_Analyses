#FLORIDA SEACAR
#Wetlands Group Richness over time- Region Level
#Script written by Katie May Laumann, klaumann@umces.edu, on 19 November 2020
#Script last modified: 19 Sept 2021
#Script last modified by:Katie May Laumann

#clear the environment
rm(list = ls())

library(hrbrthemes)
#first, load packages
library(data.table)
library(tidyverse)
library(dplyr)

#read in subsetted file that the above code produced and saved
#wetlands_1 <- as.data.frame(fread("Coastal_Wetlands/data/All Parameters but Hecatres-2021-Jul-26.csv"))
wetlands <- read.csv("Coastal_Wetlands/data/All Parameters but Hecatres-2021-Jul-26.csv")
colnames(wetlands)

#colnames(wetlands) [23]<-"perccov"
colnames(wetlands)[colnames(wetlands) == "X.PercentCover.SpeciesComposition_.."] <- "perccov"

unique(wetlands$ProgramID)
unique(wetlands$Region)
#remove NAs from Genus and Species. This is for spp level analysis. 
#*****can do this for group level analysis, but don't need to. All genus NA's are also NA in 'Group' columns
wetlands<-
  wetlands%>%
  filter(GenusName!="NA")
unique(wetlands$ProgramID)

wetlands<-
  wetlands%>%
  filter(SpeciesName!="NA")
unique(wetlands$ProgramID)

wetlands<-
  wetlands%>%
  filter(Month!="NA")

wetlands<-
  wetlands%>%
  filter(Year!="NA")

wetlands<-
  wetlands%>%
  filter(SpeciesGroup1!="NA")

wetlands<-
  wetlands%>%
  filter(SpeciesGroup1!="Invasive")

unique(wetlands$SpeciesGroup1)

#You may decide to remove certain programs, for example here we remove 
#program 5015, which only looks at a specific site, and program 651, at request of SMEs
wetlands<-
  wetlands%>%
  filter(ProgramID!="5015")
unique(wetlands$ProgramID)

wetlands<-
  wetlands%>%
  filter(ProgramID!="651")


#remove duplicates and percent cover 0s
class(wetlands$MADup)
wetlands<-
  wetlands%>%
  filter(MADup==1)

wetlands<-
  wetlands%>%
  filter(perccov!=0)

nrow(wetlands)
#bring together Genus and species in one column so we have correct species names
wetlands$gensp<-paste(wetlands$GenusName, wetlands$SpeciesName, sep=" ")
colnames(wetlands)
class(wetlands$Month)
wetlands$mth<-as.numeric(wetlands$Month)
class(wetlands$Year)
wetlands$yr<-as.numeric(wetlands$Year)

#make a yearmonth column so we can analyze at that level
wetlands$yearmonth<-ifelse(wetlands$mth==10|wetlands$mth==11|wetlands$mth==12, (paste(wetlands$yr, wetlands$mth, sep=".")),
                           (paste(wetlands$yr, wetlands$mth, sep=".0")))
unique(wetlands$yearmonth)
colnames(wetlands)

unique(wetlands$gensp)
#correct misspellings in the dataset
wetlands$gensp2<-ifelse(wetlands$gensp=="Sarcocornia ambigua","Salicornia ambigua",paste(wetlands$gensp))
wetlands$gensp<-wetlands$gensp2

unique(wetlands$SpeciesGroup1)

#We need date to be read, by R, as a date. See what class it is.
#class(wetlands$SampleDate2)
#convert SampleDate to a date
#wetlands$SampleDate <- as.Date(wetlands$SampleDate2, format="%Y-%m-%d")
#class(wetlands$SampleDate)
#We ultimately want Group Richness, so here we will begin getting data ready to
##count the number of unique species by year by managed area
#make a data frame with the required information. 
#************for Group level analyses, omit "Genus" and replace it with "Group___".....

#some Managed areas have the same spp counted by different programs in different years-this is not an issue for spp richness
#but will be important when we move on to % cover

wetlandsMA<-unique(wetlands[,c("Year","SpeciesGroup1","ManagedArea","yearmonth","Month","ProgramID")])
#remove program ID if you determine it does not sig impact spp rich
colnames(wetlandsMA)
#How many Managed Areas do we have?
unique(wetlandsMA$ManagedArea)
wetlandsMA<-
  wetlandsMA%>%
  filter(ManagedArea!="NA")

#remove Managed Areas that don't have at least 5 years of data. 
##First, ID which ones have fewer than 5 yrs of data
nyearsMA<-
  wetlandsMA%>%
  group_by(ManagedArea)%>%
  summarise(length(unique(Year)))
colnames(nyearsMA) [2]<-"nyrsMA"
nyearsMA
#Store names of Managed areas that have <5 years of data in the object "eliminateMA" 
eliminateMA<-
  nyearsMA%>%
  filter(nyrsMA<5)
eliminateMA
#eliminate those MAs from Analyses using antijoin
#use anti_join to include only programs>=10 yrs of data
wetlandsMA<-anti_join(wetlandsMA,eliminateMA,by=c("ManagedArea"))

#What's left?
unique(wetlandsMA$ManagedArea)
unique(wetlandsMA$ProgramID)
#run analyses for these 2 Managed Areas
colnames(wetlandsMA)
#calculate Group Richness by Managed Area by Year
##remember that we have, in "wetlandsMA", all unique combinations of "Year","GenusName","gensp",and "Region".
##sort this by "Region" and "Year"
##THEN count the number of unique species BY "Region" and "Year"

wetlandsgrprichReg<-
  wetlandsMA%>%
  group_by(ManagedArea,yearmonth,Year,Month)%>%
  summarise(length(unique(SpeciesGroup1)))

#wetlandsgrprichReg<-
#wetlandsMA%>%
#group_by(Region,yearmonth,Year,Month)%>%
#summarise(n = n())
#remove program ID if you determine it does not sig impact spp rich

colnames(wetlandsgrprichReg)
#"n" is the number of unique spp per Region and Year, aka the Group Richness
##rename that column
colnames(wetlandsgrprichReg) [5]<-"grouprichness"
#what do we have?
wetlandsgrprichReg
#now we have richness
#write.csv(wetlandsgrprichReg,"wetlandsgrprichReg.csv")

##
#Plot richness
plots_listyrmth <- lapply(unique(wetlandsgrprichReg$ManagedArea), function (i) {
  
  dat <- filter(wetlandsgrprichReg, ManagedArea == i)
  
  ggplot(data = dat, aes(x = yearmonth, y = grouprichness)) +
    geom_point() +
    labs(title = i) +
    scale_x_discrete(breaks = wetlandsgrprichReg$yearmonth[seq(1, length(wetlandsgrprichReg$yearmonth), by = 1)])+
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1,size=6)
    )
  
})
plots_listyrmth

#save as "Coastal Wetlands Group Richness by Year/Month"
pdf("Coastal Wetlands Group Richness by YearMonth, Managed Area.pdf")
print(plots_listyrmth)
dev.off()
#and for Year
plots_listyr <- lapply(unique(wetlandsgrprichReg$ManagedArea), function (i) {
  
  dat <- filter(wetlandsgrprichReg, ManagedArea == i)
  
  ggplot(data = dat, aes(x = Year, y = grouprichness)) +
    geom_point() +
    labs(title = i) +
    scale_x_discrete(breaks = wetlandsgrprichReg$Year[seq(1, length(wetlandsgrprichReg$Year), by = 1)])+
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1,size=6)
    )
  
})
plots_listyr

pdf("Coastal Wetlands Group Richness by Year, Managed Area.pdf.pdf")
print(plots_listyr)
dev.off()

#RUN GLMs, one MA at a time
#start with just one Managed Area: GTMNERR
unique(wetlandsgrprichReg$ManagedArea)
GTMNERR<-
  wetlandsgrprichReg%>%
  filter(ManagedArea=="Guana Tolomato Matanzas NERR")
colnames(GTMNERR)
#
#See if Month influences spp richness
GTMNERRMontheffect=glm(GTMNERR$grouprichness~GTMNERR$Month)
summary(GTMNERRMontheffect)
par(mfrow=c(2,2))
plot(GTMNERRMontheffect)

#if month does have an effect, you will likely want to incorporate it into the model. Here, we are
##modeling Group Richness over time by year

colnames(GTMNERR)
#first let's plot the richness:
##by yearmonth
pdf("GTMNERR Coastal Wetlands Group Richness Data.pdf")
par(mfrow=c(1,1))
plot(GTMNERR$yearmonth, GTMNERR$grouprichness,   
     xlab="Year",ylab="# of species",main="GTMNERR Coastal Wetlands Group Richness by Month & Year")
plot(GTMNERR$Year, GTMNERR$grouprichness,   
     xlab="Year",ylab="# of species",main="GTMNERR Coastal Wetlands Group Richness by Year")
dev.off() 

#histogram of data
library(ggplot2)
par(mfrow=c(1,1))
ggplot(GTMNERR, aes(x=grouprichness)) + 
  geom_histogram(binwidth=1,color="black", fill="grey")+
  ggtitle("GTMNERR Group Richness Data")
ggsave("GTMNERR Spp Richness Histogram.pdf")

#Run the glm- below are samples to run it first by year, then by yearmonth. 
#Following that are sample lines of code to run the same analyses with different families. We are plotting the first example,
#but you will want to run each family and compare the model outputs.
#YEAR
GTMNERRmodelYear=glm(GTMNERR$grouprichness~GTMNERR$Year)
summary(GTMNERRmodelYear)
pdf("GTMNERR Coastal Wetlands Group Richness by Year_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(GTMNERRmodelYear)
dev.off()

#PLOT the model
GTMNERRmodel<-ggplot(GTMNERR, aes(x = Year, y = grouprichness) ) +
  geom_point() +
  geom_smooth(method = "glm", alpha = .1,se=TRUE,color="darkgreen", fill="green") + 
  theme_ipsum() +
  ggtitle("GTMNERR Group Richness by Year Fitted Model")

#export file by clicking #Export on the left and saving as: 
#GTMNERR Coastal Wetlands Group Richness by Year_Model fit


#YEARMONTH
#GTMNERRmodelYearmonth=glm(GTMNERR$grouprichness~GTMNERR$yearmonth)
#summary(GTMNERRmodelYearmonth)

#try other families
#GTMNERR3=glm(GTMNERR$grouprichness~GTMNERR$Year,family=poisson)
#summary(NGTMNERR3)

#GTMNERR4=glm(GTMNERR$grouprichness~GTMNERR$Year,family=Gamma)
#summary(GTMNERR4)

#GTMNERR5=glm(GTMNERR$grouprichness~GTMNERR$Year,family=inverse.gaussian)
#summary(GTMNERR5)

#############################################

#Next MA
#start with just one Managed Area: GuanaRiverMarsh
unique(wetlandsgrprichReg$ManagedArea)
GuanaRiverMarsh<-
  wetlandsgrprichReg%>%
  filter(ManagedArea=="Guana River Marsh")
colnames(GuanaRiverMarsh)
#
#See if Month influences spp richness
GuanaRiverMarshMontheffect=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$Month)
summary(GuanaRiverMarshMontheffect)
par(mfrow=c(2,2))
plot(GuanaRiverMarshMontheffect)

#if month does have an effect, you will likely want to incorporate it into the model. Here, we are
##modeling Group Richness over time by year

colnames(GuanaRiverMarsh)
#first let's plot the richness:
##by yearmonth
pdf("GuanaRiverMarsh Coastal Wetlands Group Richness Data.pdf")
par(mfrow=c(1,1))
plot(GuanaRiverMarsh$yearmonth, GuanaRiverMarsh$grouprichness,   
     xlab="Year",ylab="# of species",main="GuanaRiverMarsh Coastal Wetlands Group Richness by Month & Year")
plot(GuanaRiverMarsh$Year, GuanaRiverMarsh$grouprichness,   
     xlab="Year",ylab="# of species",main="GuanaRiverMarsh Coastal Wetlands Group Richness by Year")
dev.off() 

#histogram of data
library(ggplot2)
par(mfrow=c(1,1))
ggplot(GuanaRiverMarsh, aes(x=grouprichness)) + 
  geom_histogram(binwidth=1,color="black", fill="grey")+
  ggtitle("GuanaRiverMarsh Group Richness Data")
ggsave("GuanaRiverMarsh Spp Richness Histogram.pdf")

#Run the glm- below are samples to run it first by year, then by yearmonth. 
#Following that are sample lines of code to run the same analyses with different families. We are plotting the first example,
#but you will want to run each family and compare the model outputs.
#YEAR
GuanaRiverMarshmodelYear=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$Year)
summary(GuanaRiverMarshmodelYear)
pdf("GuanaRiverMarsh Coastal Wetlands Group Richness by Year_Diagnostics.pdf")
par(mfrow=c(2,2))
plot(GuanaRiverMarshmodelYear)
dev.off()

#PLOT the model
GuanaRiverMarshmodel<-ggplot(GuanaRiverMarsh, aes(x = Year, y = grouprichness) ) +
  geom_point() +
  geom_smooth(method = "glm", alpha = .1,se=TRUE,color="darkgreen", fill="green") + 
  theme_ipsum() +
  ggtitle("GuanaRiverMarsh Group Richness by Year Fitted Model")

#export file by clicking #Export on the left and saving as: 
#GuanaRiverMarsh Coastal Wetlands Group Richness by Year_Model fit


#YEARMONTH
#GuanaRiverMarshmodelYearmonth=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$yearmonth)
#summary(GuanaRiverMarshmodelYearmonth)

#try other families
#GuanaRiverMarsh3=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$Year,family=poisson)
#summary(NGuanaRiverMarsh3)

#GuanaRiverMarsh4=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$Year,family=Gamma)
#summary(GuanaRiverMarsh4)

#GuanaRiverMarsh5=glm(GuanaRiverMarsh$grouprichness~GuanaRiverMarsh$Year,family=inverse.gaussian)
#summary(GuanaRiverMarsh5)

#############################################

