#loading libraries
library(data.table)
library(dplyr)

#reading the data sets
batting <- fread("Batting.csv",header = T, stringsAsFactors = F, sep = ",")
salary <- fread("Salaries.csv",header = T, stringsAsFactors = F , sep = ",")


#printing the head of batting data and looking at the structure of batting data
print(head(batting))
str(batting)


#Feature engineering 
#We need to add three more statistics that were used in Moneyball!
#(1)calculating the batting average
batting$Avgbat <- batting$H / batting$AB


#(2)calculating On base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)


#(3) Creating Slugging Average (SLG)
# Creating X1B (Singles) for slugging average

batting$X1B <- with(batting , batting$H - batting$`2B` - batting$`3B` - batting$HR)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$`2B`) + (3 * batting$`3B`) + (4 * batting$HR) ) / batting$AB
str(batting)

#combining both salary and batting data 
#before combining lets look at the data of salaries
summary(salary)
summary(batting)

# we can observe that the batting data is starting from the year 1871 and salary data from 1985
#so lets make batting data starting from 1985
batting <- subset(batting,batting$yearID >= 1985)

#lets check
summary(batting)

#combining the two datasets by playerID and yearID
dataset <- merge(batting,salary, by = c('playerID','yearID'))
summary(dataset)

#we need to have a dataset with no missing values
dataset <- na.omit(dataset)
colSums(is.na(dataset))
#there were no missing values in our data set

# Extract out the details of the lost players, Giambi, Damon and Isringhausen from the 'dataset' df 
#Given in the book 
players_lost <- subset(dataset,playerID %in% c('giambja01','damonjo01','saenzol01'))
players_lost


#grab the rows only for year 2001
players_lost <- subset(players_lost,yearID == 2001)
players_lost

#now we need to replace these lost players 
better_players <- filter(dataset, yearID == 2001)

better_players <- filter(better_players,salary<8000000,OBP>0,AB >= 500)

#lets sort by OBP 
better_players <- arrange(better_players,desc(OBP))

#grab the top players
better_players <- head(better_players,5)
better_players

# we cant choose giambja again so the replacemenet players are 
three_players <- better_players[2:4,]
three_players
