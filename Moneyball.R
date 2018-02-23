# Read the batting data into an object
batting <- read.csv('Batting.csv')

# Check the first 6 rows and columns to get a brief idea about the data
print(head(batting))

# Check the structure of the data for more information
print(str(batting))

# Create a new column for batting average as a new statistic
batting$bat_avg <- batting$H / batting$AB
print(tail(batting$bat_avg,5))

# Create a new column for on-based percentage as a new statistic
batting$on_based_percent <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# To calculate slugging average, we also need to find the singles
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Now, we can calculate slugging average
batting$slug_avg <- ((1*batting$X1B) + (2*batting$X2B) + (3*batting$X3B) + (4*batting$HR))/batting$AB

# Check the structure again to see if all columns were added successfully
print(str(batting))

# Check the summary of the batting dataframe
print(summary(batting))

# In the year column, we can see that we have data from 1871 to 2013, which we do not need. Grab data from 1985 onward (salary data to be used later also begins
# from 1985)
batting <- subset(batting,yearID>=1985)

# Now check if the necessary change has successfully been executed
print(summary(batting$yearID))

# Assign the salary data to an object
sal <- read.csv('Salaries.csv')

# Check the first 6 rows for more insight about the data
print(head(sal))

# Combine the 2 dataframes into a single dataframe
combo <- merge(batting,sal,by=c('playerID','yearID'))

# Check to see if the data has been merged
print(summary(combo))

# Now we will check the information of the players that were lost by the team by making a dataframe of those players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
# Check if the dataframe was successfully created
print(head(lost_players))

# Since the players left the team in 2001, we need only 2001 statistics
lost_players <- subset(lost_players,yearID==2001)

# We only need their performance statistics so we can filter out those columns
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','on_based_percent','slug_avg','bat_avg','AB')]

# Check if we get the desired data
print(head(lost_players))

# Get the mean on based percentage of lost players
print(mean(lost_players$on_based_percent))

# Get the sum of AB of lost players
print(sum(lost_players$AB))

# Our main task is to find 3 replacement players against the 3 lost players with the following constraints:
# 1. Combined salaries should be less than 15 million dollars
# 2. Average on_based_percent of 3 players should be equal or greater than 0.3638
# 3. Sum of At Bats(AB) of players is greater than or equal to 1469

# Set the year as 2001 for all the data to see the list of potential candidates
combo <- subset(combo,yearID==2001)
print(head(combo))

# Lets make a scatter plot of salary vs on_based_percent to get a visual of all the candidates in the pool
library(ggplot2)
print(ggplot(combo,aes(x=on_based_percent,y=salary)) + geom_point(size=3,color='blue'))

# From the scatter plot, we can see that there are a lot of potential players below the 10 million dollar salary mark and below 0.5 on_based_percent
combo <- subset(combo,on_based_percent>0 & salary<8000000)

# Check if the change was executed
print(summary(combo$salary))

# Also, we need the AB of each player to be 450 or above to get to the desired combined AB value
combo <- subset(combo,AB>450)

# Check if the change was executed
print(summary(combo$AB))

# Now, we must have narrowed down the list of candidates
print(str(combo))

# So we have 121 candidates to choose from. We can arrange their statistics in descending order to start choosing players
library(dplyr)

# Let us check the top 10 players based on 'on based percent' rating
candidates <- head(arrange(combo,desc(on_based_percent)),10)

# View the required columns of the top 10 players
print(candidates[,c('playerID','salary','AB','on_based_percent')])

# We cannot use the first entry because that player has quit. Lets check if entries 2,3 and 4 can be selected or not
# Combined salaries equal $10,088,333 which satisfies the salary constraint
# Combined AB score is 1773 which satisfies the constraint
# Average on-based percent of the players is 0.4316, which also satisfies the constraint
# Hence, we have found out the replacement players to be heltoto01, berkmla01 and gonzalu01!!