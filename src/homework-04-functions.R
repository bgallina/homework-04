###Functions###

get_tweets <- function(name = "Hillary Clinton", szam = "10") {
  v = (match(name, tweets$name))
  if (tweets$name[v] == "Hillary Clinton"){
    hillary <- subset(tweets,tweets$name == "Hillary Clinton")
    hillary <- hillary[order(hillary$ossz, decreasing=T), ]
    if (szam <=nrow(hillary)){
      print (head(hillary$text,szam))
      print(name)
      print(szam)
    } else {
      stop("Exceeded maximum")
    }
  }else if (tweets$name[v] == "Donald Trump"){
    trump <- subset(tweets,tweets$name == "Donald Trump")
    trump <- trump[order(trump$ossz, decreasing=T), ]
    if (szam <=nrow(trump)){
      print (head(trump$text,szam))
      print(name)
      print(szam)
    } else {
      stop("Exceeded maximum")
    }
  } else if ((tweets$name[v] != "Donald Trump")
             |(tweets$name[v] != "Hillary Clinton")) {
    stop("Error, invalid name")
  }
}