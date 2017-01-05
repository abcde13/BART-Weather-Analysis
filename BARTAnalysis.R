

setwd("~/Documents/Dev/Data/Rproj/BartTwitter")

devtools::install.packages("darksky")

source("TwitterApiHandling.R")

textDivide <- function(){

  divided_text = strsplit(gsub("[[:punct:]*]","",tweets$text),"\\s+")

  cols = lapply(divided_text,walkThrough)
  delay_times = c()
  problem_types = c()
  problem_situations = c()
  problem_locations = c()
  affected_dirs = c()
  recovering = c()
  to_remove = c()

  for (i in 1:length(cols)) {
    if(sum(is.na(cols[[i]])) > 0){
      to_remove = c(to_remove,i)
    }
    delay_times = c(delay_times,cols[[i]][1])
    problem_types = c(problem_types,cols[[i]][2])
    problem_situations = c(problem_situations,cols[[i]][3])
    problem_locations = c(problem_locations,cols[[i]][4])
    affected_dirs = c(affected_dirs,cols[[i]][5])
    recovering = c(recovering,cols[[i]][6])
  }
  recovering = as.logical(recovering)
  delay_times = tolower(delay_times)

  tweets_spec_df = select(tweets,text,created)
  tweets_spec_df = cbind(tweets_spec_df,delay_times,problem_types,problem_situations,problem_locations,affected_dirs,recovering)
  names(tweets_spec_df) = c("Text", "Timestamp", "Delay.min", "Problem_Types", "Problem_Situations", "Problem_Locations", "Affected_Dirs", "Recovering")
  tweets_spec_df$Delay.min = as.character(tweets_spec_df$Delay.min)
  tweets_spec_df$Date = as.Date(tweets_spec_df$Timestamp,"%d-$m-$Y")

  tweets_spec_df
}

walkThrough <- function(text_vec){

  recovering = FALSE
  delay_time = "10"
  problem_type = ""
  problem_situation = ""
  finding = 0
  decrement_step = FALSE
  finding_problem = FALSE
  problem_locations = c()
  affected_dirs = c()
  for(i in 1:length(text_vec)){

    if(i == 1 && text_vec[i] == "BART" && text_vec[i+1] == "recovering"){
      recovering = TRUE
    }

    if(recovering && !decrement_step){
      step = 2
    } else if(recovering && decrement_step) {
      step = 1
    } else if (decrement_step) {
      step = -1
    } else {
      step = 0
    }

    if(i == 1+step){
      delay_time = text_vec[i]
      if(tolower(delay_time) == "major"){
        decrement_step = TRUE
      }
    } else if(i == 4+step){
      if(text_vec[i] == "between" || text_vec[i] == "at"){
        problem_situation = "station"
        finding = 1
      } else if (text_vec[i] == "on") {
        finding = 1
        problem_situation = "line"
      }

    }

    if(finding == 1){
      if(grepl("^[[:upper:]]+$",text_vec[i])){
        problem_locations = trimws(paste(problem_locations,text_vec[i]))
      }
    } else if(finding == 2) {
      if(grepl("^[[:upper:]]+$",text_vec[i])){
        affected_dirs = trimws(paste(affected_dirs,text_vec[i]))
      }
    }

    if(text_vec[i] == "in"){
      finding = 2
    }

    if(text_vec[i] == "due"){
      if(text_vec[i+2] == "an" && text_vec[i+3] == "earlier") {
        if(i+5 > length(text_vec)){
          problem_type = paste(text_vec[i+4])
        } else {
          problem_type = paste(text_vec[i+4], text_vec[i+5])
        }
      } else if(text_vec[i+2] == "a" || text_vec[i+2] == "earlier"){
        if(i+4 > length(text_vec)){
          problem_type = paste(text_vec[i+3])
        } else {
          problem_type = paste(text_vec[i+3], text_vec[i+4])
        }
      } else {
        if(i+3 > length(text_vec)) {
          problem_type = paste(text_vec[i+2])
        } else {
          problem_type = paste(text_vec[i+2], text_vec[i+3])
        }
      }
      if(grepl("^[[:upper:]]+\\s",problem_type)){
        problem_type = NA
      } else {
        problem_type = trimws(problem_type)
      }
      break
    }


    if(FALSE){
      if(i == length(text_vec)-1){
        if(text_vec[i] == "to"){
          problem_type = text_vec[i+1]
        } else {
          problem_type = trimws(paste(text_vec[i],text_vec[i+1]))
        }
        if(grepl("^[[:upper:]]+\\s",problem_type)){
          problem_type = NA
        }
        break
      }
    }

  }
  if(is.null(delay_time)){
    delay_time = NA
  }
  if(is.null(affected_dirs)){
    affected_dirs = NA
  }
  if(is.null(problem_type) || problem_type == ""){
    problem_type = NA
  } else if (problem_type == "equip prob") {
    problem_type = "equipment prob"
  } else if (grepl("medical",problem_type)) {
    problem_type = "medical emergency"
  }
  if(is.null(problem_locations)){
    problem_locations = NA
  }
  if(is.null(problem_situation) || problem_situation == ""){
    problem_situation = NA
  }
  if(is.null(recovering)){
    recovering = NA
  }

  c(delay_time,problem_type,problem_situation,problem_locations,affected_dirs,recovering)
}

# Note rules of language (automated, so easier to parse)
#
# 1) Message contains "at" indicating subsequent term is location of delay
# 2) Message contains "on" indicating subsequent term is a line impacted by delay
# 3) Message contains "between" indicating subsequent terms are stations impacted
# 4) Message captilizations are always either locations or lines
# 5) Types of problems are
#    a) "medical emergency"
#    b) "equip prob"
# 6) "and" is used two mention multiple lines, almost always followed by "dirs", and then the reason for the delay
# 7) "10" or "20" or "Major" are used to delineate types of delays. major > 20 min
# 8) If "BART recovering" precedes all other text, then BART is experiencing improvements in delay time

