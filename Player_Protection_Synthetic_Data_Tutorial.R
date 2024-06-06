#####De-identification, De-personalization & Pseudonmization of Sport Data Tutorial

#First, must download and install packages to be used. 

install.packages('dplyr')

devtools::install_github("paulhendricks/anonymizer")

#Attaching Libraries 
library(dplyr)
library(anonymizer)

######Getting NBA Data###########

#Downloading open data for NBA 
devtools::install_github('tylerferguson/NBAinjuries')

#Attaching library
library(NBAinjuries)

#Injury Data 
NBAinjuries::injuries



#Saving NBA Injury Data as a Data Frame 

nba_injuries <- NBAinjuries::injuries


#####Exploring NBA Data#################

View(nba_injuries) #View overall data set
names(nba_injuries) #get variable names

#Notice that PlayerName and PlayerId are connected. PlayerId is connected OPENLY for NBA data across the internet. 
#We cannot use PlayerId for de-identification, must create a unique, de-identified ID. 

#First things first, Anonymizing data for ce-identification
#Will use the 'anonymizer' R package
#Brief overview here: https://www.rdocumentation.org/packages/anonymizer/versions/0.2.0

#Create unique IDs
#Set Seed, important for reproducibility
set.seed(17)
nba_injuries$Unique_ID <- anonymize(nba_injuries$PlayerName, .algo = "crc32")

#Checking Generally
nba_injuries %>% 
  dplyr::select(PlayerName, PlayerId, PlayerDOB, Positions, Unique_ID) %>% 
  View()

#Checking for Matching of Unique ID's across Player
#Taking a former Duke Player... 

nba_injuries %>% 
  filter(PlayerName == "J.J. Redick") %>% 
  dplyr::select(PlayerName, PlayerId, PlayerDOB, Positions, Unique_ID) %>% 
  View()
#You will notice that J.J. Redick's random unique ID is the same for all instances. 


#As discussed in the lecture, you cannot have the player names in the data set for player security, 
#but also need to be able to have a separate file to connect names/ID's. 

#Creating Player Name/Unique ID Data Sheet. 

distinct_connected_name_file <- nba_injuries %>% 
                                distinct(PlayerName, .keep_all = T) %>% #Allows for only one name for the data sheet, don't need repeats. 
                                dplyr::select(PlayerName, PlayerId, Unique_ID)  #Also makes it easier to read & follow 
#Checking
View(distinct_connected_name_file)


#Now we need to 'take out' PlayerName and PlayerId, from the main data frame

nba_injuries_nonames <- nba_injuries %>% 
                        dplyr::select(-PlayerName, -PlayerId)

#Checking 
View(nba_injuries_nonames)

#Moving UniqueID to the first column 
nba_injuries_nonames <- nba_injuries_nonames %>% 
                        relocate(Unique_ID, .before = PlayerDOB)

#Checking 
names(nba_injuries_nonames)


#So, we have no removed the name and 'de-identified' the NBA data. 
#However, as you learned within the lecture, de-identification is not enough. We also need to de-personalize. 
#Thus, we need to 'hide' information that could identify these NBA players. 
#We are going to use the same anonymizer function for date of birth. 

set.seed(1721)
nba_injuries_nonames$PlayerDOB <- anonymize(nba_injuries_nonames$PlayerDOB, .algo = "crc32")

#Checking 
nba_injuries_nonames$PlayerDOB

#Last transaction description  and Team Name Full are also an Identifiers (city as well, but see later on here), 
#as it describes what happened to a player, in that time frame. 
#This is not necessary for analyses, and so is 'dropped' from the main data set. 
#However, we keep these as a separate data set joined with the unique ID, in case of future use. 
#We keep these in full long data, as each one is unique.

team_last_transaction <- nba_injuries_nonames %>% 
                        dplyr::select(Unique_ID, TeamNameFull, TeamName, LastTransactionDescription)

#Dropping these variables from the main data set 

nba_injuries_dropped_variables <- nba_injuries_nonames %>% 
                                  dplyr::select(-TeamNameFull, -TeamName, -LastTransactionDescription)

View(nba_injuries_dropped_variables)


#Now that we have de-personalized the data, we need to pseudonymize the data.
#The city name could be connected to the player. We need to pseudonymize the city. 

#Seeing the distinct number of cities
nba_injuries_dropped_variables %>% 
  distinct(TeamLocation) %>% 
  nrow() #32 cities


#Creating a vector of the 32 Cities
distinct_nba_teams <- nba_injuries_dropped_variables %>% 
                      distinct(TeamLocation)



#Installing City Name Packages 
install.packages('maps')

#UploadinglLibrary 
library(maps)


us_cities <- data("us.cities")
View(us_cities)
names(us.cities)


us.cities$pop <- as.numeric(us.cities$pop )


#Randomly sampling 32 U.S. cities 
set.seed(17211986)
city_location <- sample(us.cities$name, 32, replace = FALSE)

#Joining distinct, this is also your data sheet for joining reference
pseudo_cities <- cbind(distinct_nba_teams, city_location)

#Adding city pseudonym to NBA data set

nba_injuries_dropped_variables <- left_join(nba_injuries_dropped_variables, pseudo_cities, by = c("TeamLocation")) 


#Dropping Team Location 
nba_injuries_dropped_variables <- nba_injuries_dropped_variables %>% 
                      dplyr::select(-TeamLocation)

names(nba_injuries_dropped_variables)


#Moving City Location to near the front of the dataset
nba_injuries_dropped_variables <- nba_injuries_dropped_variables %>% 
                                            relocate(city_location, .before = Height)

#Checking
View(nba_injuries_dropped_variables)


#####
#There are other variables that could be de-personalized & pseudonymized, depending on the question and level of protection from de-identification needed 
#For example, Year started and ended, draft pick, etc. 
#This is all context specific. 


#Final Data set 

nba_injuries_final <- nba_injuries_dropped_variables

#Writing the data set to your computer
write.csv(nba_injuries_final, 'C:\\pathname.csv')




















