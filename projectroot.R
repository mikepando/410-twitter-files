#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("tokenizers")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("ggplot2")
#install.packages("XML")
#install.packages("igraph")

library(twitteR)
library(ROAuth)
library(wordcloud)
library(RColorBrewer)
library(tokenizers)
library(tm)
library(ggplot2)
library(XML)
library(igraph)

getwd() #The working directory where R will read files and save files into
load(file="cred611.Rdata") #Assumes that your credntials file is in the current folder
setup_twitter_oauth(cred611$consumerKey, cred611$consumerSecret, 
                    cred611$oauthKey, cred611$oauthSecret)

#If data is already pulled. Load it using these:
load(file="tweetsBase.Rdata")       #loads initial tweets file - 91
load(file="tweetsClean.Rdata")      #loads clean tweets file
load(file="tweetsFullBase.Rdata")   #loads full timeline tweets - 3200
load(file="tweetsFullClean.Rdata")  #loads cleaned up Full data

#Primary twitter account to mine tweets
screenName= "torproject"
user<- getUser(screenName)
str(user)
user$followersCount
user$friendsCount
user$statusesCount
user$created
user$favoritesCount


#lets gather tweets regarding from our user's timeline
tweets<- userTimeline(user, n= 1000, maxID=NULL, sinceID=NULL)
length(tweets)
tweetsDF <- twListToDF(tweets)
View(tweetsDF)
save(tweetsDF, file="tweetsBase.Rdata")
write.csv(tweetsDF, file="tweetsBase.csv")

#notice how it only returns a portion of the tweets we request? Lets get the full timeline
tweetsFull<- userTimeline(user, n= 3200, maxID=NULL, sinceID=NULL, includeRts=TRUE)
length(tweetsFull)
tweetsFullDF <- twListToDF(tweetsFull)
View(tweetsFullDF)
save(tweetsFullDF, file="tweetsFullBase.Rdata")
        #this includes the full timeline, so all of the post the user retweets is included as well.

#So TorProject account retweets a lot of controversial and prominent technology and security news, so the reason for the
#lack of user tweets is due to this. So we must dig deeper.
tweetsDF$id[91] #gets the ID of the last tweet in our data frame
tweets1<- userTimeline(user, n= 1000, maxID=NULL, sinceID=1108025908382842880)
length(tweets1)
tweetsDF1 <- twListToDF(tweets1)
View(tweetsDF1)
#the twitter API doesn't let us go back that far. Therefore it makes sinceID the non-inclusive minimum. So we will just
#use the tweets dating back to March. We will compare the two timelines, the one with retweets and the one without

###lets clean up our text###
#functions
removeNonASCII<- function(txt){   #removes non ascii text
  return(iconv(txt, to="ASCII", sub=""))
}
removeCntrls<- function(x){       #removes cntrl words
  x<- gsub("[[:cntrl:]]",  "",x)
  return(x)
}
removeURLs <- function(x){        #removes urls
  x<- gsub("http(s?)://[[:alnum:]].\\S*", " ", x)
  x<- gsub("http(s?):/.*$", " ",x)
  return(x)
}
removeRT<- function(x){           #removes RT in retweets
  x<- gsub("(^RT|^MRT) @", "@",x, ignore.case=TRUE)
  return(x)
}
removeQuotes<- function(x){       #removes quotes
  return(gsub("\'|\"", " ", x))
}
removeNewLines<- function(x){     #removes newlines functions
  x<- gsub("[\r\n]",  " ",x)
  return(x)
}
removeColons<- function(x){       #removes colons
  x<- gsub(":",  " ",x)
  return(x)
}
removePeriods<- function(x){      #removes period from text
  x<- gsub("[.]",  " ",x)
  return(x)
}
removeMany<- function(x){         #remove other text mess, & / http
  x<- gsub("&.*;",  " ",x)
  x<- gsub("/",  " ",x)
  x<- gsub(",",  " ",x)
  x<- gsub(" http",  " ",x)
  x<- gsub("http ",  " ",x)
}
removeExtraSpaces<- function(x){    #removes any blank spaces
  x<- gsub("[[:space:]]+",  " ",x)
  return(x)
}
removeLeadingTrailingSpaces<- function(x){    #removes any spaces at begging or end of text
  x<- gsub("^[[:space:]]",  "",x)
  x<- gsub("[[:space:]]$",  "",x)
  return(x)
}

#cleaning up tweetsDF
tweetsDF$text <- removeNonASCII(tweetsDF$text)
tweetsDF$text <- removeCntrls(tweetsDF$text)
tweetsDF$text <- tolower(tweetsDF$text)

grep("http(s?)://[[:alnum:]].\\S*", tweetsDF$text, value=TRUE)
m<- regexpr("http(s?)://[[:alnum:]].\\S*", tweetsDF$text)
regmatches(tweetsDF$text, m)        #Run this to check if any URLS are in the text
tweetsDF$text <- removeURLs(tweetsDF$text)

grep("(^RT|^MRT) @", tweetsDF$text, value=TRUE)
m<- regexpr("(^RT|^MRT) @", tweetsDF$text)
regmatches(tweetsDF$text, m)        #run this to check if any @ (retweets) are in the text
tweetsDF$text <- removeRT(tweetsDF$text)

grep("\'|\"", tweetsDF$text, value=TRUE)
m<- regexpr("\'|\"", tweetsDF$text)
regmatches(tweetsDF$text, m)        #Run this to see if any slashes are in the text
tweetsDF$text <- removeQuotes(tweetsDF$text)

grep("[\r\n]", tweetsDF$text, value=TRUE)
m<- regexpr("[\r\n]", tweetsDF$text)
regmatches(tweetsDF$text, m)        #checks if any new lines are in the text
tweetsDF$text <- removeNewLines(tweetsDF$text)

tweetsDF$text <- removeColons(tweetsDF$text)
tweetsDF$text <- removePeriods(tweetsDF$text)

grep("&.*;", tweetsDF$text, value=TRUE)
m<- regexpr("&.*;", tweetsDF$text)
regmatches(tweetsDF$text, m)
tweetsDF$text <- removeMany(tweetsDF$text)

tweetsDF$text <- removeExtraSpaces(tweetsDF$text)
tweetsDF$text <- removeLeadingTrailingSpaces(tweetsDF$text)

View(tweetsDF)
save(tweetsDF, file="tweetsClean.Rdata")
write.csv(tweetsDF$text, file="tweetsClean.csv")

#Cleaning up tweetsFullDF (our full tweet list, with RTs)
tweetsFullDF$text <- removeNonASCII(tweetsFullDF$text)
tweetsFullDF$text <- removeCntrls(tweetsFullDF$text)
tweetsFullDF$text <- tolower(tweetsFullDF$text)

grep("http(s?)://[[:alnum:]].\\S*", tweetsFullDF$text, value=TRUE)
m<- regexpr("http(s?)://[[:alnum:]].\\S*", tweetsFullDF$text)
regmatches(tweetsFullDF$text, m)        #Run this to check if any URLS are in the text
tweetsFullDF$text <- removeURLs(tweetsFullDF$text)

grep("(^RT|^MRT) @", tweetsFullDF$text, value=TRUE)
m<- regexpr("(^RT|^MRT) @", tweetsFullDF$text)
regmatches(tweetsFullDF$text, m)        #run this to check if any @ (retweets) are in the text
tweetsFullDF$text <- removeRT(tweetsFullDF$text)

grep("\'|\"", tweetsFullDF$text, value=TRUE)
m<- regexpr("\'|\"", tweetsFullDF$text)
regmatches(tweetsFullDF$text, m)        #Run this to see if any slashes are in the text
tweetsFullDF$text <- removeQuotes(tweetsFullDF$text)

grep("[\r\n]", tweetsFullDF$text, value=TRUE)
m<- regexpr("[\r\n]", tweetsFullDF$text)
regmatches(tweetsFullDF$text, m)        #checks if any new lines are in the text
tweetsFullDF$text <- removeNewLines(tweetsFullDF$text)

tweetsFullDF$text <- removeColons(tweetsFullDF$text)
tweetsFullDF$text <- removePeriods(tweetsFullDF$text)

grep("&.*;", tweetsFullDF$text, value=TRUE)
m<- regexpr("&.*;", tweetsFullDF$text)
regmatches(tweetsFullDF$text, m)
tweetsFullDF$text <- removeMany(tweetsFullDF$text)

tweetsFullDF$text <- removeExtraSpaces(tweetsFullDF$text)
tweetsFullDF$text <- removeLeadingTrailingSpaces(tweetsFullDF$text)

View(tweetsFullDF)
save(tweetsFullDF, file="tweetsFullClean.Rdata")
write.csv(tweetsFullDF, file="tweetsFullClean.csv")

#wordcloud for tweetsDF (our base tweet list going back to March)
windows(height=16, width=16)
set.seed(9999)
wordcloud(tweetsDF$text,scale=c(5, .4), random.order=FALSE, colors=brewer.pal(8,"Set1"))

windows(height=16, width=16)
set.seed(9999)
wordcloud(tweetsDF$text,scale=c(5, .4), random.order=FALSE, colors=brewer.pal(8,"Set1"),
          min.freq=2, max.words=Inf)
      #you can see that the lack of tweets really hurts the size of it. We'll try a 3 gram wordcloud for a 
      #better idea and more substance.
grams_3 <- tokenize_ngrams(tweetsDF$text, n = 3, n_min = 3,lowercase=TRUE)
unlist(grams_3)
g3<- table(unlist(grams_3))

windows()
set.seed(9999)
wordcloud(words=names(g3), freq=g3, scale=c(2, .4), random.order=FALSE, 
          colors=brewer.pal(8, "PuBuGn"), random.color=FALSE, rot.per=0.22,
          min.freq=2, max.words=Inf)

#wordcloud for tweetsFullDF (our list of ~3200 tweets, including retweets)
windows()
set.seed(666)
wordcloud(tweetsFullDF$text,scale=c(5, .4), random.order=FALSE, colors=brewer.pal(8,"Paired"),
          minfreq=3, max.words=500)
      #lets try a 3gram wordcloud for our full list of tweets
grams_3 <- tokenize_ngrams(tweetsFullDF$text, n = 3, n_min = 3,lowercase=TRUE)
unlist(grams_3)
g3<- table(unlist(grams_3))

windows()
set.seed(666)
wordcloud(words=names(g3), freq=g3, scale=c(2, .4), random.order=FALSE, 
          colors=brewer.pal(8, "Paired"), random.color=FALSE, rot.per=0.22,
          min.freq=3, max.words=350)

##Word cloud for only april
apriltweets <- read.csv("april_tweets.csv")
View(apriltweets)

windows()
set.seed(666)
wordcloud(apriltweets$text,scale=c(5, .4), random.order=FALSE, colors=brewer.pal(8,"Paired"),
          minfreq=2, max.words=Inf)

########################Getting users who interact with the tor project
totor<-searchTwitter("to:torproject",resultType="recent", n=180)
length(totor)
totorDF<-twListToDF(totor)
View(totorDF)

totor2<-searchTwitter("to:torproject",resultType="recent", n=300)
totor2DF<-twListToDF(totor2)
totor3<-searchTwitter("to:torproject",resultType="popular", n=300)
totor3DF<-twListToDF(totor3)

#combine the tweet lists

totorfull<-rbind(totorDF,totor2DF,totor3DF)
totorfull<-totorfull[!duplicated(totorfull$id),] #removes duplicate IDs
View(totorfull)

save(totorfull, file="TweetsToTor.Rdata")
load(file="TweetsToTor.Rdata")

d <- as.data.frame(table(totorfull$screenName))
names(d) <- c("User","Tweets")
d <- d[order(d$Tweets, decreasing=TRUE), ]
head(d)
View(d)

d <- d[!d$Tweets == 1,] #Removes all users who only tweeted at our user once
d <- d[!d$Tweets == 2,] #removes those who tweeted twice
d <- d[!d$User == "torproject",] #Removes our own user tweeting at themselves

windows(width=1000, height=700)
par(mar=c(5,10,2,2))
barplot(d$Tweets, names.arg=d$User, col=c("#c671fe","#d59bfb","#e5befe","#f1ddff","#f2ffda"), 
        horiz=TRUE, las=2, xlim=c(0,max(d$Tweets)), cex.names=0.7)

###analyzing our tweeters
igorian900<-getUser("igorian900")
str(igorian900)
igorian900$name
igorian900$description
igorian900$lastStatus$latitude    #returns 0, geo-location turned off

igor<-userTimeline(igorian900, n= 20, maxID=NULL, sinceID=NULL)
igorD<-twListToDF(igor)
View(igorD)


Ai<-getUser("Ai_Love_Veu")
str(Ai)
Ai$name
Ai$description
Ai$lastStatus$latitude    #returns 0, geo-location turned off

Ail<-userTimeline(Ai, n= 20, maxID=NULL, sinceID=NULL)
AilD<-twListToDF(igor)
View(AilD)


FIW<-getUser("FeelitWorking")
str(FIW)
FIW$name
FIW$description
FIW$lastStatus$latitude    #returns 0, geo-location turned off

FIWt<-userTimeline(FIW, n= 20, maxID=NULL, sinceID=NULL)
FIWDF<-twListToDF(igor)
View(FIWDF)


#####gathering tweets from our accounts
firefox<-getUser("firefox")
ffTweets<- userTimeline(firefox, n= 3200, maxID=NULL, sinceID=NULL, includeRts=TRUE)
length(ffTweets)
ffTweetsDF <- twListToDF(ffTweets)
View(ffTweetsDF)

tails<-getUser("tails_live")
tailsTweets<- userTimeline(tails, n= 500, maxID=NULL, sinceID=NULL, includeRts=TRUE)
length(tailsTweets)
tailsDF <- twListToDF(tailsTweets)
View(tailsDF)

ovpn<-getUser("openvpn")
ovpnTweets<- userTimeline(ovpn, n= 850, maxID=NULL, sinceID=NULL, includeRts=TRUE)
length(ovpnTweets)
ovpnDF <- twListToDF(ovpnTweets)
View(ovpnDF)

#now we need to clean up the data
ffTweetsDF$text <- removeNonASCII(ffTweetsDF$text)
ffTweetsDF$text <- removeCntrls(ffTweetsDF$text)
ffTweetsDF$text <- tolower(ffTweetsDF$text)
ffTweetsDF$text <- removeURLs(ffTweetsDF$text)
ffTweetsDF$text <- removeRT(ffTweetsDF$text)
ffTweetsDF$text <- removeQuotes(ffTweetsDF$text)
ffTweetsDF$text <- removeNewLines(ffTweetsDF$text)
ffTweetsDF$text <- removeColons(ffTweetsDF$text)
ffTweetsDF$text <- removePeriods(ffTweetsDF$text)
ffTweetsDF$text <- removeMany(ffTweetsDF$text)
ffTweetsDF$text <- removeExtraSpaces(ffTweetsDF$text)
ffTweetsDF$text <- removeLeadingTrailingSpaces(ffTweetsDF$text)

View(ffTweetsDF)
save(ffTweetsDF, file="firefoxtweets.Rdata")
write.csv(ffTweetsDF, file="firefoxtweets.csv")
#load(file="firefoxtweets.Rdata")


tailsDF$text <- removeNonASCII(tailsDF$text)
tailsDF$text <- removeCntrls(tailsDF$text)
tailsDF$text <- tolower(tailsDF$text)
tailsDF$text <- removeURLs(tailsDF$text)
tailsDF$text <- removeRT(tailsDF$text)
tailsDF$text <- removeQuotes(tailsDF$text)
tailsDF$text <- removeNewLines(tailsDF$text)
tailsDF$text <- removeColons(tailsDF$text)
tailsDF$text <- removePeriods(tailsDF$text)
tailsDF$text <- removeMany(tailsDF$text)
tailsDF$text <- removeExtraSpaces(tailsDF$text)
tailsDF$text <- removeLeadingTrailingSpaces(tailsDF$text)

View(tailsDF)
save(tailsDF, file="tailstweets.Rdata")
write.csv(tailsDF, file="tailstweets.csv")
#load(file="tailstweets.Rdata")

ovpnDF$text <- removeNonASCII(ovpnDF$text)
ovpnDF$text <- removeCntrls(ovpnDF$text)
ovpnDF$text <- tolower(ovpnDF$text)
ovpnDF$text <- removeURLs(ovpnDF$text)
ovpnDF$text <- removeRT(ovpnDF$text)
ovpnDF$text <- removeQuotes(ovpnDF$text)
ovpnDF$text <- removeNewLines(ovpnDF$text)
ovpnDF$text <- removeColons(ovpnDF$text)
ovpnDF$text <- removePeriods(ovpnDF$text)
ovpnDF$text <- removeMany(ovpnDF$text)
ovpnDF$text <- removeExtraSpaces(ovpnDF$text)
ovpnDF$text <- removeLeadingTrailingSpaces(ovpnDF$text)

View(ovpnDF)
save(ovpnDF, file="openvpnTweets.Rdata")
write.csv(ovpnDF, file="openvpnTweets.csv")
#load(file="openvpnTweets.Rdata")


tor = paste(tweetsFullDF$text, collapse=" ")
ff = paste(ffTweetsDF$text, collapse=" ")
tailss = paste(tailsDF$text, collapse=" ")
ovpnn = paste(ovpnDF$text, collapse=" ")
    # put everything in a single vector
all = c(tor, ff, tailss, ovpnn)
    # remove stop-words
all = removeWords(all,c(stopwords("english"), "torproject", "tails", "firefox", "openvpn","@tails_live","-cv","-mu",
                        "tor","@mozilla"
                        ))
all = removeWords(all,c(stopwords("english"), "tails_live","mozilla","mu","cv","privatetunnel",
                        "firefox_de"))  #cleaning up text to remove usernames and other undesireables
                                                                                              

    # create corpus
corpus = Corpus(VectorSource(all))
    # create term-document matrix
tdm = TermDocumentMatrix(corpus)
    # convert as matrix
tdm = as.matrix(tdm)
    # add column names
colnames(tdm) = c("Tor", "Firefox", "Tails", "OpenVPN")
    
# comparison cloud
windows()
comparison.cloud(tdm,scale=c(5, .4), random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"), min.freq=2,
                 title.size=1.5, max.words=200)

# commonality cloud
windows()
commonality.cloud(tdm, random.order=FALSE, 
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)

######################### Word Graph for Privacy and Security ###########
library(XML)
library(tm)
library(igraph)
library(RColorBrewer)

#pulling tweets for privay and security then cleaning it up
pstweets<-searchTwitter("privacy AND security", n=50)
length(pstweets)
pstweetsDF<-twListToDF(pstweets)
View(pstweetsDF)

save(pstweetsDF, file="pstweets.Rdata")
#load(file="pstweets.Rdata")

pstweetsDF$text <- removeNonASCII(pstweetsDF$text)
pstweetsDF$text <- removeCntrls(pstweetsDF$text)
pstweetsDF$text <- tolower(pstweetsDF$text)
pstweetsDF$text <- removeURLs(pstweetsDF$text)
pstweetsDF$text <- removeRT(pstweetsDF$text)
pstweetsDF$text <- removeQuotes(pstweetsDF$text)
pstweetsDF$text <- removeNewLines(pstweetsDF$text)
pstweetsDF$text <- removeColons(pstweetsDF$text)
pstweetsDF$text <- removePeriods(pstweetsDF$text)
pstweetsDF$text <- removeMany(pstweetsDF$text)
pstweetsDF$text <- removeExtraSpaces(pstweetsDF$text)
pstweetsDF$text <- removeLeadingTrailingSpaces(pstweetsDF$text)
pstweetsDF$text = gsub("@\\w+", "", pstweetsDF$text)  #removing any mentions so @.......
pstweetsDF$text = gsub("#\\w+", "", pstweetsDF$text)  #removing any hashtags

View(pstweetsDF)

# create corpus
corpus = Corpus(VectorSource(pstweetsDF$text))

# remove stopwords
skipwords = c(stopwords("english"), 
              "privacy", "security")
corpus = tm_map(corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert tdm to matrix
m = as.matrix(tdm)
# word counts
wc = rowSums(m)

# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]

# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]

# adjacency matrix
M = good %*% t(good)

# set zeroes in diagonal
diag(M) = 0

# graph
g = graph.adjacency(M, weighted=TRUE, mode="undirected",
                    add.rownames=TRUE)
# layout
glay = layout.fruchterman.reingold(g)

# let's superimpose a cluster structure with k-means clustering
kmg = kmeans(M, centers=8)
gk = kmg$cluster

# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}

# prepare ingredients for plot
V(g)$size = 10
V(g)$label = V(g)$name
V(g)$degree = degree(g)
#V(g)$label.cex = 1.5 * log10(V(g)$degree)
V(g)$label.color = hsv(0, 0, 0.2, 0.55)
V(g)$frame.color = NA
V(g)$color = gcols
E(g)$color = hsv(0, 0, 0.7, 0.3)

# plot
windows()
plot(g, layout=glay)
title("\nGraph of tweets about privacy and security",
      col.main="gray40", cex.main=1.5, family="serif")
