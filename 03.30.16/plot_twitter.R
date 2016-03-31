libs <- c("twitteR", "ggmap", "ggplot2", "plyr")
lapply(libs, library, character.only=TRUE)


search_terms <- c("trump", "#trump", "Trump")

#Coordinates of somewhere-in-Los-Angeles
location <- "Pennsylvania"
longitude <- c("-77.8599")
latitude <- c("40.7933")
radius <- "300mi"
latlong <- paste(latitude,longitude,radius,sep=",")
latlong <- rep(ll, length(search_terms))
search_terms <- as.data.frame(cbind(latlong, search_terms))
search_terms$search_terms <- as.character(search_terms$search_terms)
search_terms$latlong <- as.character(search_terms$latlong)
search_terms$location <- location

tweets <- data.frame()

for (i in 1:nrow(search_terms)){
  print(paste("Looking for",search_terms$search_terms[i], "in", search_terms[i,]$location))
  tw = searchTwitter(search_terms[i,]$search_terms,n=round(2000/nrow(search_terms)), geocode=search_terms[i,]$latlong, lang="en")
  if (length(tw) == 0){
    print(paste("No tweets found for", search_terms$search_terms[i], "in", location))
  } else {
    tweets=rbind(twListToDF(tw), tweets)
  }
}

df <- as.data.frame(count(tweets, c("longitude","latitude")))
df <- na.omit(df)
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)
df$freq <- as.numeric(df$freq)

#getmap
map <- get_map(location=c(lon=-77.8599,lat=40.7933), zoom=4, maptype="hybrid")
finalmap <- ggmap(map, base_layer = ggplot(aes(x=longitude, y=latitude), data = df))

finalmap +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level.., size=freq),
                 bins = 9, geom = "polygon",alpha=0.2,
                 data = df) +
  scale_fill_gradient(trans = "sqrt", low = "grey5", high = "greenyellow") +
  geom_point(col="grey80", size = (df$freq)/10)

