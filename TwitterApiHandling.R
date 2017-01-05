
# Libraries
#
# install.packages(c("twitteR","dplyr","ggplot2","devtools"))
library("twitteR")
library("dplyr")
library("ggplot2")
library("devtools")

# To install, first do devtools::install_github("hrbrmstr/darksky")
library("darksky")


consumer_key <- "loCFn6W6Eb8XNJ6r4nj7KQzSx"
access_token <- "72432521-Rk5KqGMMZfz5epYHQWUVIt2hSwRP2BePC8Z8JLR3M"
consumer_secret <- "GjBBzAGR04HNNdN8UKPqHUAxaLPGqTBZ84JW7Aqtc5ooYbpFZ2"
access_secret <- "hlsEAGStybvWlJRtnEPoZjEKWPukwDRk5lA0GCDwam7bw"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- twListToDF(userTimeline("SFBARTalert",n=3200))

