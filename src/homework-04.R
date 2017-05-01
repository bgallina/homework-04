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

