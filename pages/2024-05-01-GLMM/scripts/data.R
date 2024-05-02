# get data
visit <- read.xlsx("data/tidy/visit.xlsx")
dormouse <- read.xlsx("data/tidy/dormouse.xlsx")
iButton <- read.xlsx("data/tidy/iButton.xlsx")

# fix dates
visit$Date <- as.Date(x=visit$Date, origin = "1899-12-30")
dormouse$Date <- as.Date(x=dormouse$Date, origin = "1899-12-30")
iButton$Date <- as.Date(x=iButton$Date, origin = "1899-12-30")

# make proportions
visit$NoBoxesWithDormice <- aggregate(BoxNo ~ Site + Date, 
          data = dormouse,
          function(x) c(length(unique(x))))$BoxNo

visit$propWithDormouse <- visit$NoBoxesWithDormice/50
visit$propWithNest<- visit$NoBoxesWithNests/50

# make torpor factor
dormouse <- dormouse[-which(dormouse$TorpidRef==6),]
torporfac <- vector("character")
torporfac <- ifelse(dormouse$TorpidRef == 1, torporfac <- "torpid", torporfac <- "active")

dormouse$torporfac <- factor(torporfac) 

# join weather data
dormouse <- dplyr::left_join(
  x = dormouse,
  y = visit[,c("Site", "Date", "MetTmax", "MetTmin", "MetRainMm")],
  by = c("Site" = "Site", "Date" = "Date"),
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = FALSE
)

rm(torporfac)
