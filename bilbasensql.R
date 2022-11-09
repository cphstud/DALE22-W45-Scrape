library(DBI)
library(RMariaDB)



con = dbConnect(MariaDB(),
                db="soccerdb",
                host="localhost",
                port=3306,
                user="root",
                password="root123"
                )


mycarsdf <- readRDS("firsbb.rds")
dbWriteTable(con,"cars",mycarsdf)

myloadedcarsdf <- dbGetQuery(con,"SELECT * FROM cars limit 30")

# read player
playerdf <- read.csv("../Player.csv")
playerattrdf <- read.csv("../Player_Attributes.csv")
matchesdf <- read.csv("../match.csv")
teamdf <- read.csv("../Team.csv")
teamattrdf <- read.csv("../Team_Attributes.csv")
leaguedf <- read.csv("../Leage.csv")
countrydf <- read.csv("../Country.csv")
dbWriteTable(con,"Country",countrydf, append=T)
dbWriteTable(con,"League",leaguedf)
dbWriteTable(con,"Match",matchesdf)
dbWriteTable(con,"Player",playerdf)
dbWriteTable(con,"Player_Attributes",playerattrdf)
dbWriteTable(con,"Team_Attributes",teamattrdf)
dbWriteTable(con,"Team",teamdf)
