#money ball project

#Use R to open the Batting.csv file and assign it to a dataframe called batting using read.csv
batting <- read.csv('Batting.csv')

#Use head(batting) to check out the batting

#Use str() to check the structure. Pay close attention to how columns that start with a number get an 'X' in front of them! You'll need to know this to call those columns!

#Call the head(batting$AB) of the first five rows of AB (At Bats) column

#Call the head of the doubles (X2B) column by head(batting$X2B)

#adding more stats
batting$BA <- batting$H / batting$AB

# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

#Merging Salary Data with Batting Data
#Load the Salaries.csv file into a dataframe called sal using read.csv

sal <- read.csv('Salaries.csv')

#Use subset() to reassign batting to only contain data from 1985 and onwards

batting <- subset(batting,yearID >= 1985)

#Use the merge() function to merge the batting and sal data frames by c('playerID','yearID'). Call the new data frame combo

combo <- merge(batting,sal,by=c('playerID','yearID'))

#Use the subset() function to get a data frame called lost_players from the combo data frame consisting of those 3 players. Hint: Try to figure out how to use %in% to avoid a bunch of or statements!

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )

#Use subset again to only grab the rows where the yearID was 2001

lost_players<- subset(lost_players,yearID == 2001)

#Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]


#First only grab available players from year 2001
library(dplyr)
avail.players <- filter(combo,yearID==2001)

library(ggplot2)

#Then I made a quick plot to see where I should cut-off for salary in respect to OBP:

pl<- ggplot(avail.players,aes(x=OBP,y=salary)) +geom_point(color='red')
print(pl)

#Looks like there is no point in paying above 8 million or so (I'm just eyeballing this number). I'll choose that as a cutt off point. There are also a lot of players with OBP==0. Let's get rid of them too.

avail.players <- filter(avail.players,salary<8000000,OBP>0)

#The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB.

avail.players <- filter(avail.players,AB >= 500)

#Now let's sort by OBP and see what we've got!
possible <- head(arrange(avail.players,desc(OBP)),10)

#Grab columns I'm interested in:
possible<- possible[,c('playerID','OBP','AB','salary')]

#Can't choose giambja again, but the other ones look good (2-4). I choose them!


print(possible[2:4,])
