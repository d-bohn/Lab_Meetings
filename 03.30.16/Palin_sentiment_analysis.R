# Adapted from http://andybromberg.com/sentiment-analysis/
#library(tm);library(RTextTools);library(stylo); library(tm.plugin.sentiment)
library(sentiment);library(ggplot2)
setwd("~/Documents/School Work/Lab_Meetings/03.30.16")

data <- readLines("Palin_Endorsement.txt")
source("scripts/misc_text_functions.R")
data <- ToSentences(data)
df <- data.frame(data)

textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)

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

# perform the sentiment analysis, classifying comments using a Bayesian analysis.
# A polarity of positive, negative, or neutral is determined.  Finally, the comment, 
# emotion, and polarity are combined in a single dataframe.
class_emo = classify_emotion(df, algorithm = "bayes")
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(df, algorithm="bayes")
polarity = class_pol[,4]

sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion),
                                                              decreasing=TRUE))))

sent_df <- sent_df[which(sent_df$emotion != "unknown"),]
#Now that we have processed the comments, we can graph the emotions and polarities.

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Polarity categories", y="")


## Romney's Plea is another story...

data <- readLines("Romney_plea.txt")
source("misc_text_functions.R")
data <- ToSentences(data)
df <- data.frame(data)

textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)
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

class_emo = classify_emotion(df, algorithm = "bayes")
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(df, algorithm="bayes")
polarity = class_pol[,4]

sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion),
                                                              decreasing=TRUE))))

sent_df <- sent_df[which(sent_df$emotion != "unknown"),]
#Now that we have processed the comments, we can graph the emotions and polarities.

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Polarity categories", y="")
