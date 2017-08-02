## Download data from web ##

library(data.table)

champ1415 <- fread('http://www.football-data.co.uk/mmz4281/1516/E1.csv')
champ1516 <- fread('http://www.football-data.co.uk/mmz4281/1415/E1.csv')

head(champ1415)
head(champ1516)

## Generate goals for/against for Home/AWay teams from 14/15 season ##

HomeFor <- with(champ1415,tapply(FTHG,HomeTeam,sum))
HomeAg <- with(champ1415,tapply(FTAG,HomeTeam,sum))
AwayFor <- with(champ1415,tapply(FTAG,AwayTeam,sum))
AwayAg <- with(champ1415,tapply(FTHG,AwayTeam,sum))
  
convert <- function(x) { y <- as.data.frame(cbind(names(x),x))
                          names(y) <- c("Team",deparse(substitute(x)))
                          y}

HomeFor <- convert(HomeFor)
HomeAg <- convert(HomeAg)
AwayFor <- convert(AwayFor)
AwayAg <- convert(AwayAg)

## Identify first weekend fixtures for 15/16 season ##

fixtureList <- champ1516[1:12,HomeTeam:FTAG]

fixtureList <- merge(fixtureList,HomeFor,by.x = "HomeTeam",by.y = "Team")
fixtureList <- merge(fixtureList,HomeAg,by.x = "HomeTeam",by.y = "Team")
fixtureList <- merge(fixtureList,AwayFor,by.x = "AwayTeam",by.y = "Team")
fixtureList <- merge(fixtureList,AwayAg,by.x = "AwayTeam",by.y = "Team")

## Run Poisson Regression ##

model <- glm(FTHG~HomeFor+AwayAg,fixtureList,poisson)

summary(model)
