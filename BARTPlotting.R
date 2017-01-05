
setwd("~/Documents/Dev/Data/Rproj/BartTwitter")

source("BARTAnalysis.R")
source("WeatherAnalysis.R")

plotProbTypes <- function(){
  curr_df = tweets_spec_df %>%
    group_by(Problem_Types) %>%
    filter(n() > 30) %>%
    filter(!is.na(Problem_Types))

  ggplot(curr_df, aes(x=Problem_Types)) + geom_bar()
}

plotDelayTimes <- function(){
  curr_df = tweets_spec_df %>%
    group_by(Delay.min) %>%
    filter(n() > 15) %>%
    filter(!is.na(as.integer(Delay.min)) || Delay.min == "major")

  ggplot(curr_df, aes(x=Delay.min)) + geom_bar()
}

plotRecoveryMessages <- function(){
  curr_df = tweets_spec_df

  print(typeof(curr_df$Date))
  curr_df %>%
    mutate(MonthYear = format(Date,"%Y-%m")) %>%
    filter(Recovering == F) %>%
    group_by(MonthYear) %>%
    summarize(Failures = n()) %>%
    ggplot(aes(MonthYear,Failures)) + geom_point()
}

plotCloudCoverWithEvents <- function(){
  weather %>%
    ggplot(aes(PST,CloudCover)) +
    geom_point(aes(color=weather$Events)) +
    geom_line() + scale_x_date("%b-%Y")
}

plotCloudCoverByMonth <- function(){
  weather %>%
    mutate(MonthYear = format(PST,"%Y-%m")) %>%
    group_by(MonthYear) %>%
    summarize(TotalCloudCover = sum(CloudCover)) %>%
    ggplot(aes(MonthYear,TotalCloudCover)) +
    geom_point()
}

plotCloudCoverAndFailures <- function(){
  weather %>%
    mutate(MonthYear = format(PST,"%Y-%m")) %>%
    group_by(MonthYear) %>%
    summarize(TotalCloudCover = sum(CloudCover)) %>%
    ggplot(aes(MonthYear,TotalCloudCover)) +
    geom_point()


}

tweets_spec_df = textDivide()

plotProbTypes()

plotDelayTimes()

plotRecoveryMessages()

plotCloudCoverWithEvents()

plotCloudCoverByMonth()

