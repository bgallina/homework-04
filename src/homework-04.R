#######################
#Házi Feladat IV.     #
#Programozás I.       #
#2016/2017 II. félév  #
#Gallina Beáta        #
#2017.05.01.          #
#######################

#### II. Feladat
###1.)

tweets <- read.csv(file = "data/clinton_trump_tweets.csv", sep = ";", 
                   header=T)

###2.)

tweets$name <- "name"
tweets$name[tweets$handle == "HillaryClinton"] <- "Hillary Clinton"
tweets$name[tweets$handle == "realDonaldTrump"] <- "Donald Trump"

tweets_freq <- sort((table(tweets$name)), decreasing =T)

mycols <- c("blue", "red")

barplot(tweets_freq, main = "Candidate tweets", ylab = "Tweet frequency", xlab =
          NULL, space = 1, width = 1, col = mycols, border=F)

legend("center", row.names(tweets_freq), cex=0.8, bty="n", pch = 15, 
       col = mycols)

#Elmentem:

png("fig/tweet1.png", width = 1000, height = 560, res = 120)
barplot(tweets_freq, main = "Candidate tweets", ylab = "Tweet frequency", xlab =
          NULL, space = 1, width = 1, col = mycols, border=F)

legend("center", row.names(tweets_freq), cex=0.8, bty="n", pch = 15, 
       col = mycols)

dev.off()

###3.)

table(tweets$lang)

et <- subset(tweets, lang == "et")
et$text #Inkább angol
tweets$lang[tweets$lang == "et"] <- "en"

fr <- subset(tweets, lang == "fr")
fr$text #Inkább angol furcsa karakterekkel
tweets$lang[tweets$lang == "fr"] <- "en"

da <- subset(tweets, lang == "da")
da$text #Inkább angol
tweets$lang[tweets$lang == "da"] <- "en"

fi <- subset(tweets, lang == "fi")
fi$text #Inkább angol
tweets$lang[tweets$lang == "fi"] <- "en"

tl <- subset(tweets, lang == "tl")
tl$text #Inkább angol
tweets$lang[tweets$lang == "tl"] <- "en"

my_cols2 <- c("darkgrey", "cornflowerblue")

lang_freq <- table(droplevels(tweets)$lang, (tweets)$name)
barplot(lang_freq, beside = TRUE, border = NA, main = "Language of tweets", 
        ylab = "Tweet frequency", col = my_cols2)

legend("right", row.names(lang_freq) <- c("English", "Spanish"), border=NA,
       pch = 15, title="Language", bty="n", col = my_cols2)

#Elmentem

png("fig/tweet2.png", width = 960, height = 560, res = 120)
barplot(lang_freq, beside = TRUE, border = NA, main = "Language of tweets", 
        ylab = "Tweet frequency", col = my_cols2)

legend("right", row.names(lang_freq) <- c("English", "Spanish"), border=NA,
       pch = 15, title="Language", bty="n", col = my_cols2)

dev.off()

#Törlöm, ami már felesleges:

rm(et, fr, da, fi, tl)

###4.) 

tweets$ossz <- 0

tweets$ossz <- tweets$retweet_count+tweets$favorite_count
tweets$ossz <- as.vector(tweets$ossz)

#Behívom a függvényt:

source("~/homework-04/src/homework-04-functions.R")

#Ellenőrzöm, hogy jól működik-e:

get_tweets ("Hillary Clinton", 500000000)
get_tweets ("Donald Trump")

####III. Feladat
###1.)

#Adatok behívása:

if (!("fivethirtyeight" %in% installed.packages())) {
  install.packages("fivethirtyeight")
}
library(fivethirtyeight)

data(hiphop_cand_lyrics)

head(hiphop_cand_lyrics)

#Adatok előkészítése vizualizációhoz:
#Először használtam a melt függvényt, de nem ugyanaz a skála jött ki
#az y tengelyen, mint a mintaábrákon, ezért enélkül folytattam.

library(reshape2)

cleardf <- melt(hiphop_cand_lyrics, id.vars = c("candidate", 
                                                "album_release_date"))
rm(cleardf)

#ggplot2 behívása:

install.packages("ggplot2", dependencies = TRUE)

library(ggplot2)

x <- table(hiphop_cand_lyrics$candidate, hiphop_cand_lyrics$album_release_date)
x <- data.frame(x)
x

#Plot:

ggplot(data = x, aes(x = Var2,Freq, fill = Var1)
) + geom_bar(stat = "identity", position = position_stack()) +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Year", y="Frequency")+
  scale_fill_manual(values=c("lightgreen", "green3", "yellow", "orange", 
                             "skyblue1", "red","pink", "maroon2")) +
  scale_x_discrete(breaks= c(1995,2000,2005,2010,2015))

#Elmentem

ggsave("fig/hiphop1.png", width = 10, height = 5, dpi = 100)

##By sentiment:
#Positive:

positive <- hiphop_cand_lyrics[hiphop_cand_lyrics$sentiment == "positive", ]

clearpositive2 <- table(positive$candidate, 
                        positive$album_release_date)
clearpositive2 <- data.frame(clearpositive2)


#Plot1:

pl1 <- ggplot(data =clearpositive2, aes(x = Var2, Freq, 
                                        fill = Var1)) + 
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle("Positive") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  labs(x="Year", y="Frequency")+
  scale_fill_manual(values=c("lightgreen", "green3", "orange", "skyblue1"))+
  scale_x_discrete(breaks= c(1990,1995,2000, 2005,2010,2015))

#Negative                                                          
negative <- hiphop_cand_lyrics[hiphop_cand_lyrics$sentiment == "negative", ]

clearnegative2 <- table(negative$candidate, 
                        negative$album_release_date)
clearnegative2 <- data.frame(clearnegative2)

#Plot2:

pl2 <- ggplot(data =clearnegative2, aes(x = Var2, Freq, 
                                        fill = Var1)) + 
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle("Negative") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  labs(x="Year", y="Frequency")+
  scale_fill_manual(values=c("green3", "orange", "skyblue1", "red", 
                             "pink", "maroon2"))+
  scale_x_discrete(breaks= c(2000, 2005,2010,2015))

#Neutral
neutral <- hiphop_cand_lyrics[hiphop_cand_lyrics$sentiment == "neutral", ]

clearneutral2 <- table(neutral$candidate, 
                       neutral$album_release_date)
clearneutral2 <- data.frame(clearneutral2)

#Plot3:

pl3 <- ggplot(data =clearneutral2, aes(x = Var2, Freq, 
                                       fill = Var1)) + 
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle("Neutral") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  labs(x="Year", y="Frequency")+
  scale_fill_manual(values=c("yellow", "orange", "skyblue1", "red", 
                             "pink"))+
  scale_x_discrete(breaks= c(1990,1995,2000, 2005,2010,2015))

library(grid)
library(gridExtra)

#Összevonom a 3 ábrát:

grid.arrange(pl1, pl2, pl3, nrow=1, ncol = 3, 
             top = "Candidate mentions, by sentiment")

#Elmentem:

png("fig/hiphop2.png", width = 960, height = 560, res = 120)
grid.arrange(pl1, pl2, pl3, nrow=1, ncol = 3, 
             top = "Candidate mentions, by sentiment")
dev.off()

##Ábra 3.: Politics
#Melyik évben melyik politikus kapta a legtöbb politikai említést?

politics <- hiphop_cand_lyrics[hiphop_cand_lyrics$theme == "political", ]

politics2 <- table(politics$candidate, 
                   politics$album_release_date)
politics2 <- data.frame(politics2)

ggplot(data =politics2, aes(x = Var2, Freq, 
                            fill = Var1)) + 
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle("Which politician was mentioned in political themes per year") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Year", y="Frequency")+
  scale_fill_manual(values=c("green3", "orange", "skyblue1", "red", 
                             "maroon2"))+
  scale_x_discrete(breaks= c(1993, 1998, 2004, 2008, 2013, 2016))

#Elmentem:

ggsave("fig/hiphop3.png", width = 10, height = 5, dpi = 100)

#Törlöm, ami felesleges:

rm(politics, politics2, clearnegative, clearnegative2, clearneutral,
   clearneutral2, clearpositive, clearpositive2)

###IV. Feladat
##1.)

szenti <- table(droplevels(tweets)$text_sentiment, (tweets)$name)
emotion <- table(droplevels(tweets)$text_emotion, (tweets)$name)

my_cols3 <- c("firebrick", "coral", "bisque1")

barplot(szenti, beside = TRUE, border = NA, main = "Tweetek szentimentjei", 
        ylab = "Tweet frequency", width=5, col = my_cols3)

legend("topleft", row.names(szenti) <- c("Negative","Neutral", "Positive"), 
       pch = 15, bty="n", col = my_cols3)

my_cols4 <- c("palegreen", "paleturquoise", "peachpuff", "rosybrown1", "plum1",
              "seashell", "wheat2")

barplot(emotion, beside = TRUE, border = T, main = "Tweetek emóciói", 
        ylab = "Tweet frequency", col = my_cols4, width = 5)

legend("topleft", row.names(emotion) <- c("Anger", "Disgust", "Fear",
                                          "Joy", "Sadness","Surprise", "Unknown"), cex=1,pch = 15, bty="n", 
       col = my_cols4)

#Statisztikailag szignifikáns a különbség?

chisq.test(tweets$text_sentiment, tweets$name)
chisq.test(tweets$text_emotion, tweets$name)

#Igen

#Idősoros ábra:
#Kell hozzá az idő változó

szept <- subset(tweets, (grepl('2016-09', tweets$time)))
szept$time2 <- "9"

aug <- subset(tweets, (grepl('2016-08', tweets$time)))
aug$time2 <- "8"

juli <- subset(tweets, (grepl('2016-07', tweets$time)))
juli$time2 <- "7"

juni <- subset(tweets, (grepl('2016-06', tweets$time)))
juni$time2 <- "6"

maj <- subset(tweets, (grepl('2016-05', tweets$time)))
maj$time2 <- "5"

apr <- subset(tweets, (grepl('2016-04', tweets$time)))
apr$time2 <- "4"

marc <- subset(tweets, (grepl('2016-03', tweets$time)))
marc$time2 <- "3"

feb <- subset(tweets, (grepl('2016-02', tweets$time)))
feb$time2 <- "2"

jan <- subset(tweets, (grepl('2016-01', tweets$time)))
jan$time2 <- "1"

osszevont <- rbind(szept, aug, juli, juni, maj, apr, marc, feb, jan)

Trump <- subset(osszevont, name == "Donald Trump")

Hillary <- subset(osszevont, name =="Hillary Clinton")

seged_T <- melt(Trump, id.vars = c("name", "time2", "text_sentiment", 
                                   "text_emotion"))

seged_H <- melt(Hillary, id.vars = c("name", "time2", "text_sentiment", 
                                     "text_emotion"))

#By sentiment:

Trump2 <- table(seged_T$time2, seged_T$text_sentiment)
Hillary2 <- table(seged_H$time2, seged_H$text_sentiment)

Trump2 <- data.frame(Trump2)
Hillary2 <- data.frame(Hillary2)

p_T <- ggplot(Trump2, aes(x = Var1, Freq, group=Var2,
                          fill=Var2)) + 
  geom_bar(stat = "identity", position = position_stack())+
  ggtitle("Sentiments in Trump's tweets by months")+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  theme(plot.title = element_text(size = 12))+
  xlab("2016/Month")+
  ylab("Frequency")+
  scale_fill_manual(values=c("orangered", "sandybrown", "wheat2"))

p_H <- ggplot(Hillary2, aes(x = Var1, Freq, group=Var2,
                            fill=Var2)) + 
  geom_bar(stat = "identity", position = position_stack())+
  ggtitle("Sentiments in Hillary's tweets by months")+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  theme(plot.title = element_text(size = 12))+
  xlab("2016/Month")+
  ylab("Frequency")+
  scale_fill_manual(values=c("orangered", "sandybrown", "wheat2"))

#Összevonom az ábrákat:

grid.arrange(p_T, p_H, nrow=1, ncol = 2, 
             top = "Candidate mentions, by sentiment and months")

#By emotion:

Trump3 <- table(seged_T$time2, seged_T$text_emotion)
Hillary3 <- table(seged_H$time2, seged_H$text_emotion)

Trump3 <- data.frame(Trump3)
Hillary3 <- data.frame(Hillary3)

p_T_e <- ggplot(Trump3, aes(x = Var1, Freq, group=Var2,
                            fill=Var2)) + 
  geom_bar(stat = "identity", position = position_stack())+
  ggtitle("Sentiments in Trump's tweets by months")+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  theme(plot.title = element_text(size = 12))+
  xlab("2016/Month")+
  ylab("Frequency")+
  scale_fill_manual(values=c("lightgreen", "green3", "yellow", "orange", 
                             "skyblue1", "red","pink"))

p_H_e <- ggplot(Hillary3, aes(x = Var1, Freq, group=Var2,
                              fill=Var2)) + 
  geom_bar(stat = "identity", position = position_stack())+
  ggtitle("Emotions in Hillary's tweets by months")+
  theme(legend.text=element_text(size=5),
        legend.position="top")+
  theme(plot.title = element_text(size = 12))+
  xlab("2016/Month")+
  ylab("Frequency")+
  scale_fill_manual(values=c("lightgreen", "green3", "yellow", "orange", 
                             "skyblue1", "red","pink"))+
  scale_x_discrete(breaks= c(1, 2, 3, 4, 5, 6, 7, 8, 9))

#Összevonom a két ábrát:

grid.arrange(p_T_e, p_H_e, nrow=1, ncol = 2, 
             top = "Candidate mentions, by emotions and months")

#Törlöm, ami felesleges:

rm(Hillary2, Hillary3, seged_H, Trump2, Trump3, seged_T, jan, febr, marc, apr,
   maj, juni, juli, aug, szept)

##2.)

Trump <- subset(tweets, name == "Donald Trump")

#Leszűröm a datasetet az Androidos éa Iphone-os tweetekre:

Trump2 <- subset(tweets, source_url == "http://twitter.com/download/iphone"
                 | source_url == "http://twitter.com/download/android")

#Tweetek szentimentjei forrás szerint:

szenti_Trump <- table(droplevels(Trump2)$source_url, (Trump2)$text_sentiment)

mycols5 <- c("darksalmon", "darkred")

barplot(szenti_Trump, beside = TRUE, border = T, 
        main = "Tweetek szentimentjei forrás szerint", 
        ylab = "Tweet frequency", col = mycols5)
legend("topleft", row.names(szenti_Trump) <- c("Android", "Iphone"), 
       pch = 15, col = mycols5)

#Tweetek emóciói forrás szerint:

emot_Trump <- table(droplevels(Trump2)$source_url, (Trump2)$text_emotion)


barplot(emot_Trump, beside = TRUE, border = T, 
        main = "Tweetek emóciói forrás szerint", 
        ylab = "Tweet frequency", col = mycols5)
legend("topleft", row.names(emot_Trump) <- c("Android", "Iphone"), 
       pch = 15, col = mycols5)

#Android esetében jobban beazonosíthatók az emóciók

chisq.test(Trump2$text_sentiment, Trump2$source_url)
chisq.test(Trump2$text_emotion, Trump2$source_url)

#Van szignifikáns különbség

##3.)

install.packages('rmarkdown')
library(knitr)

