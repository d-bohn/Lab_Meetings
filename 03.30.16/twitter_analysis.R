library(twitteR);library(RCurl);library(RJSONIO);library(stringr)

setwd("~/Documents/School Work/Lab_Meetings/03.30.16")
#source("scripts/hashtagSearch.R")

###########################
## Let's get some tweets!##
##    A Basic Example    ##
###########################
tweets <- searchTwitter("Statistics OR #statistics", n=200, lang="en", since="2014-08-20")
tweets.df <- twListToDF(tweets)

########################
## A Geocoded Example ##
########################
tweets_geolocated <- searchTwitter("PSU OR 'penn state' OR #PSU OR #pennstate", n=500, lang="en",
                                   geocode="40.7982,-77.8599,200mi", since="2014-08-20")
tweets <- twListToDF(tweets_geolocated)


####################
## Clean the data ##
####################
textdata <- sapply(tweets$text, function(x) gettext(x))

# remove nonessential characters such as punctuation, numbers, 
#web addresses, etc from the text

textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL

# classify emotion
class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(textdata, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion Categories for #PSU", y="Count")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Polarity Categories for #PSU", y="Count")

