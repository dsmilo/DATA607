# load libraries ####
library(RMySQL)
library(dplyr)
library(stringr)


# set up SQL ####

volcano_db = dbConnect(MySQL(), user='root', password='password', host='localhost')
dbSendQuery(volcano_db, 'CREATE SCHEMA volcanoes;')
dbSendQuery(volcano_db, 'USE volcanoes')
dbSendQuery(volcano_db,'CREATE TABLE volcanoes (
            volc_id INT PRIMARY KEY,
            name VARCHAR(50),
            eruption INT,
            latitude FLOAT,
            longitude FLOAT;')
dbSendQuery(volcano_db,'CREATE TABLE cities (
            geonameId INT PRIMARY KEY,
            asciiname VARCHAR(50),
            country_code CHAR(2),
            latitude FLOAT,
            longitude FLOAT,
            population INT);')
dbSendQuery(volcano_db,'CREATE TABLE near (
            near_id INT PRIMARY KEY,            
            geonameId INT NOT NULL,
            volc_id INT NOT NULL,
            distance FLOAT NOT NULL,
            FOREIGN KEY (geonameId) REFERENCES cities(geonameId),
            FOREIGN KEY (volc_id) REFERENCES volcanoes(volc_id));')
dbSendQuery(volcano_db,'CREATE TABLE countries (
            country_code CHAR(2) PRIMARY KEY,     
            Country VARCHAR(50) NOT NULL);')

dbListTables(volcano_db)


# read in volcano info ####
GVP <- read.csv('GVP_Volcano_List.csv', stringsAsFactors = FALSE)[1:10]
names(GVP) <- c("Number", "Name", "Type", "Eruption", names(GVP)[5:10])
GVP <- GVP %>% filter(!grepl(" of ", Type))
GVP$Eruption <- as.numeric(GVP$Eruption)
GVP$Eruption <- ifelse(is.na(GVP$Eruption), -100000, GVP$Eruption)
GVP$Name <- iconv(GVP$Name, to = "ASCII", sub = "")

names(GVP) <- str_to_lower(names(GVP))
names(GVP)[1] <- "volc_id"
GVP <- select(GVP, volc_id, name, eruption, latitude, longitude)
dbWriteTable(volcano_db, "volcanoes", GVP, append = TRUE, row.names = FALSE)


#read in city list ####
cities1000 <- read.table('./cities1000.txt', header = FALSE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, quote = "")
names(cities1000) <- c("geonameId", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature_class", "feature_code", "country_code", "cc2", "admin1_code", "admin2_code", "admin3_code", "admin4_code", "population", "elevation", "dem", "timezone", "modification_date")
cities1000 <- cities1000 %>% select(geonameId, asciiname, country_code, latitude, longitude, population)
cities1000$geonameId <- as.integer(cities1000$geonameId)
cities1000$latitude <- as.double(cities1000$latitude)
cities1000$longitude <- as.double(cities1000$longitude)
cities1000$population <- as.integer(cities1000$population)
dbWriteTable(volcano_db, "cities", cities1000, append = TRUE, row.names = FALSE)


#read in countries ####
countries <- read.table('http://download.geonames.org/export/dump/countryInfo.txt', header = TRUE, skip = 50, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, quote = "", comment.char = "", check.names = FALSE)
names(countries)[1] <- "country_code"
countries[countries$Country == "Namibia", 1] <- "NA"
countries <- countries %>% select(country_code, Country)
dbWriteTable(volcano_db, "countries", countries, append = TRUE, row.names = FALSE)


# set up function ####
get.places <- function(lat, lng, maxRows = 10000, radius = 161) {
  api_query <- paste0('http://api.geonames.org/findNearbyPlaceNameJSON?style=short&formatted=TRUE&cities=cities1000&username=', getOption('geonamesUsername'), '&lat=', lat, '&lng=', lng, '&maxRows=', maxRows, '&radius=', radius)
  places <- fromJSON(api_query)$geonames
  if (class(places) == "data.frame"){
    places <- select(places, geonameId, distance)
  } else{places <- list(NULL)}
  places
}


# find cities near volcanoes ####
near_count <- 0

for (i in 1:nrow(GVP)) {
  nearby <- data.frame(NULL)
  nearby <- get.places(GVP$latitude[i], GVP$longitude[i])
  if(class(nearby) == "list") next
  nearby$near_id <- c(seq(near_count + 1, near_count + nrow(nearby), 1))
  nearby$volc_id <- rep(GVP$volc_id[i], nrow(nearby))
  nearby <- nearby %>% select(near_id, geonameId, volc_id, distance)
  if (i == 1) {
    dbWriteTable(volcano_db, "near", nearby, overwrite = TRUE, row.names = FALSE)
  } else{
    dbWriteTable(volcano_db, "near", nearby, append = TRUE, row.names = FALSE)
  }
  near_count <- near_count + nrow(nearby)
  print(paste(i, nrow(nearby), near_count))
}


# pull from database ####
volcano_db = dbConnect(MySQL(), user='root', password='password', host='localhost', db = 'volcanoes')
res <- dbSendQuery(volcano_db,
                   'SELECT c.asciiname, co.Country, c.latitude, c.longitude, c.population, v.eruption, n.distance
                   FROM near AS n
                   INNER JOIN cities AS c
                   ON c.geonameId = n.geonameId
                   INNER JOIN countries AS co
                   ON c.country_code = co.country_code
                   INNER JOIN volcanoes AS v
                   ON n.volc_id = v.volc_id;')
at_risk <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(volcano_db)


# clean risk data####
at_risk$distance <- as.numeric(at_risk$distance) * 0.621371
at_risk$eruption <- 2016 - at_risk$eruption


# calculate risk####
at_risk <- at_risk %>% mutate(risk = log(population + 2, base = 10) / log(eruption + 2, base = 10) / ((distance + 1)^2))


# group risk ####
city_risk <- at_risk %>% mutate(City = paste(asciiname, Country, sep = ", ")) %>% group_by(City) %>% summarise(lat = mean(latitude), lng = mean(longitude), pop = mean(population), risk = sum(risk)) %>% select(City, lat, lng, risk) %>% arrange(desc(risk))

max_risk <- max(city_risk$risk)
city_risk$risk <- city_risk$risk / max_risk * 1000


# show data table ####
plot(city_risk$risk)
summary(city_risk$risk)

datatable(city_risk, rownames = FALSE, colnames = c('City, Country', 'Latitude', 'Longitude', 'Risk')) %>% formatRound(2:3, 2) %>% formatRound(4, 0)


# ggmap ####
myMap <- get_map(location=c(-180, -78, 180, 78), source="stamen", maptype="watercolor", crop=TRUE)
ggmap(myMap) + 
  geom_point(data = GVP, aes(x = longitude, y = latitude), pch = 2, col = "red", alpha = 0.25) + 
  geom_point(data = city_risk, aes(x = lng, y = lat, size = risk), alpha = 0.5) +
  theme(legend.position = 'none', plot.title = element_text(face = "bold")) +
  ggtitle("Map of Volcanoes and Cities at Risk") +
  scale_x_continuous('', breaks = NULL, labels = NULL) +
  scale_y_continuous('', breaks = NULL, labels = NULL) +
  scale_size(range = c(0, 8))


# ggplot ####
ocean <- data.frame(x = c(-200, 200, 200, -200), y = c(-90, -90, 90, 90))

ggplot() + geom_polygon(data = ocean, aes(x = x, y = y), fill = "skyblue") +
  borders("world", colour="gray50", fill="chartreuse3", alpha = 0.75) + 
  geom_point(data = GVP, aes(x = longitude, y = latitude), pch = 2, col = "red", alpha = 0.25) + 
  geom_point(data = city_risk, aes(x = lng, y = lat, size = risk), alpha = 0.5) +
  theme(legend.position = 'none', plot.title = element_text(face = "bold")) +
  ggtitle("Map of Volcanoes and Cities at Risk") +
  scale_x_continuous('', breaks = NULL, labels = NULL) +
  scale_y_continuous('', breaks = NULL, labels = NULL) +
  scale_size(range = c(0, 8))
