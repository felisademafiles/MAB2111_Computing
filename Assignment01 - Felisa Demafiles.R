#1. WHO Dataset
WHO <- read.csv("WHO.csv")

#1.d. country with the lowest literacy
WHO$Country[(which.min(WHO$LiteracyRate))]

#1.e. Richest country in Europe based on GNI
WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe$Country[(which.max(WHO.Europe$GNI))]

#1.f. Mean Life expectancy of countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy, na.rm=FALSE)

#1.g. Number of countries with population greater than 10,000
WHO.Population = subset(WHO, Population > 10000)
length(WHO.Population$Country)

#1.h. Top 5 countries in the Americas with the highest child mortality
WHO.Americas = subset(WHO, Region == "Americas")
WHO.AmericasSorted <- WHO.Americas[order(WHO.Americas$ChildMortality,decreasing=TRUE),]
head(WHO.AmericasSorted$Country,n=5)



#2 NBA Dataset
NBA <- read.csv("Historical NBA Performance.csv")

#2.a. The year Bulls has the highest winning percentage 
NBA.Bulls = subset(NBA, Team == "Bulls")
NBA.Bulls$Year[(which.max(NBA.Bulls$Winning.Percentage))]


#2.b. Teams with an even win-loss record in a year
NBA.Even <- subset(NBA, Winning.Percentage==0.5)
NBA.Even



#3 Seasons_Stats
Seasons <- read.csv("Seasons_Stats.csv")

#3.a. Player with the highest 3-pt attempt rate in a season. 
Seasons.X3PAr <- aggregate(Seasons["X3PAr"], Seasons["Player"], mean, na.rm=TRUE)
Highest.X3PAr <- subset(Seasons.X3PAr,Seasons.X3PAr$X3PAr==max(Seasons.X3PAr$X3PAr,na.rm=TRUE))
Highest.X3PAr

#3.b. Player with the highest free throw rate in a season. 
Seasons.FTr <- aggregate(Seasons["FTr"], Seasons["Player"], mean, na.rm=TRUE)
Highest.FTr <- subset(Seasons.FTr,Seasons.FTr$FTr==max(Seasons.FTr$FTr,na.rm=TRUE))
Highest.FTr

#3.c. What year/season does Lebron James scored the highest? 
Seasons.Lebron = subset(Seasons, Player == "LeBron James")
Seasons.Lebron$Year[(which.max(Seasons.Lebron$PTS))]

#3.d. What year/season does Michael Jordan scored the highest? 
Seasons.Michael = subset(Seasons, Player == "Michael Jordan*")
Seasons.Michael$Year[(which.max(Seasons.Michael$PTS))]

#3.e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Seasons.Kobe = subset(Seasons, Player == "Kobe Bryant")
Seasons.Kobe$PER[(which.min(Seasons.Kobe$MP))]



#4. National Universities Rankings.csv
NU <- read.csv("National Universities Rankings.csv")

#4.a. University with the most number of undergrads 
NU$Undergrad = gsub(",","",NU$Undergrad.Enrollment)
NU$Name[(which.max(NU$Undergrad))]

#4.b. Average Tuition in the Top 10 University
NU.Top10 <- subset(NU, Rank <= 10)
NU.Top10$Tuition = gsub("[\\$,]", "", NU.Top10$Tuition.and.fees)
mean(as.numeric(NU.Top10$Tuition))

#added to test commits in GitHub.
