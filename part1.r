## ------------------------------------------------------------------------
##install.packages("tidyverse")
library("tidyverse")

survey <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/music-survey.csv")

preferences <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/preferences-survey.csv")


## ------------------------------------------------------------------------

 colnames(survey)[colnames(survey)=='First, we are going to create a pseudonym for you to keep this survey anonymous (more or less). Which pseudonym generator would you prefer?'] <- "pseudonym_generator"

colnames(survey)[colnames(survey)=="What is your pseudonym?"] <- "pseudonym"

colnames(survey)[colnames(survey)=="Timestamp"] <- "time_submitted"

colnames(survey)[colnames(survey)=="Sex"] <- "sex"

colnames(survey)[colnames(survey)=="Major"] <- "academic_major"

colnames(survey)[colnames(survey)=="Academic Year"] <- "academic_level"
  
colnames(survey)[colnames(survey)=="Year you were born (YYYY)"] <- "year_born"

colnames(survey)[colnames(survey)=="Which musical instruments/talents do you play? (Select all that apply)"] <- "instrument_list"

colnames(survey)[colnames(survey)=="Artist"] <- "favorite_song_artist"

colnames(survey)[colnames(survey)=="Song"] <- "favorite_song"

colnames(survey)[colnames(survey)=="Link to song (on Youtube or Vimeo)"] <- "favorite_song_link"

suppressMessages(library("dplyr"))
suppressMessages(library("tidyr"))
suppressMessages(library("readr"))

## ?strsplit to split data
## ?parse_datetime
##dplyr
##tidyr
##do library command to load them
##tibble is a df with extra constraints and features

df1 <- dplyr::tibble(survey[,1:7])

Person <- dplyr::tibble(time_submitted=survey$time_submitted,
pseudonym_generator=survey$pseudonym_generator,
pseudonym=survey$pseudonym,
sex=survey$sex,academic_major=survey$academic_major,
academic_level=survey$academic_level,
year_born=survey$year_born)

Favorite_Song <- dplyr::tibble(pseudonym=survey$pseudonym,
favorite_song_artist=survey$favorite_song_artist,
favorite_song=survey$favorite_song,
favorite_song_link=survey$favorite_song_link)

sum(survey$pseudonym_generator == "Fake band name generator")

sum(survey$pseudonym_generator == "Fake rapper name generator")

##gather(cases{df}, "year"{new key of col}, "n"{new val of col},2:4{numeric vals of columns to collapse})

##spread(pollution{df to reshape}, size{col for keys}, amount{col for new values})

Person$time_submitted <- parse_datetime(Person$time_submitted, format = "%m/%d/%y %H:%M")

##creating levels for major/level

Person$academic_major <- as.factor(Person$academic_major)
Person$academic_level <- as.factor(Person$academic_level)
Person$sex <- as.factor(Person$sex)




levels(Person$academic_level)
levels(Person$academic_major)

levels(Person$academic_major)[levels(Person$academic_major)=="Computer information systems"] <- "Computer Information Systems"

levels(Person$academic_major)

##gathering ratings into a more readable way

Ratings <- gather(preferences, key = "artist_song", value = "rating", 3:45)

Ratings <- dplyr::tibble(time = Ratings$Timestamp, pseudonym = Ratings$`What was your pseudonym?`, artist_song=Ratings$artist_song, rating=Ratings$rating                         )

newdf <- data.frame(pseudonym = Ratings$pseudonym, avg=0)

for(i in 1:nrow(newdf)){
  newdf$avg[i] = mean(Ratings$rating[Ratings$pseudonym == newdf$pseudonym[i]])
}
newdf <- unique(newdf)
library(ggplot2)
ggplot(data = newdf, 
       aes(x=pseudonym,y=avg)) + geom_col()



## ------------------------------------------------------------------------
##Kevin's way
earliest_time <- min(Ratings$time[Ratings$pseudonym == "Angel Angel"])

Ratings <- Ratings %>% filter(!(pseudonym=="Angel Angel" & time != earliest_time))

Ratings$artist_song <- str_replace_all(Ratings$artist_song, pattern = "\t", replacement = " ")

favorite_rating <- Ratings %>% left_join(Favorite_Song, by="pseudonym") %>% filter(artist_song == paste(favorite_song_artist, favorite_song)) %>% select(pseudonym, artist_song, rating)
                               
                               





