
# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes it reproducable also if random number generators are used
options (mc.cores=parallel::detectCores ()) # Run on multiple cores
#-------------------------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
source("src/Functions.r")

#initiatinggg!! #Throws in an error message that I think is fine to ignore
d  <- read_and_clean("data/cat2stduy1_data.csv") 


##Free text & binary & multiple categories#######

d %>%
  filter(condition == "ft" | condition == "xb" | condition == "mc") %>% 
  group_by(masc, race, condition) %>% 
  mutate(categorization = recode(categorization, 
                                 "1" = "f", "F" = "f", "kvinna" = "f", "female" = "f", "female " = "f", "Female"= "f", "Fenale"= "f", "women" = "f", "woman " = "f", "femLE" = "f", "FEmale" = "f", "Femalw" = "f", "Fwmalw" = "f", "Female " = "f", "woman" = "f", "Woman" = "f", "feMale" = "f", "fermale" = "f", "wman" = "f", "Femae" = "f",
                                 "2" = "m", "man" = "m","Male " = "m", "make" = "m", "Male"= "m", "male" = "m", "man " = "m",  "male " = "m", "guy" = "m", "boy" = "m", "Make" = "m", "M"  = "m", "Man" = "m", "Bottom half male; above nose female., Would have to say Male" = "m", " male" = "m", "male  " = "m", "ale" = "m", "nmale" = "m", "MALE"= "m", "nale"= "m", " Male" = "m",
                                 "3" = "o", "Nonbinary" = "o", "Non Binary " = "o", "Unsure" = "o", "Non binary " = "o", "good" = "o", "Neutral" = "o", "neutral" = "o", "nonbinary" = "o", "bigender" = "o", "hen" = "o", "don't know"  = "o", "Bottom half male, nose upwards female" = "o",
                                 "4" = "unknown", ),
         condition = recode(condition, "ft" = "Free text", "xb" = "Binary Categories", "mc" = "Multiple Categories"))%>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill") + 
  ggtitle("Gender Categorizations by Participants")+
  theme_minimal()+ 
  facet_wrap(~condition)

#subsetting by race
d %>%
  filter(condition == "ft") %>% 
  group_by(masc, race) %>% 
  mutate(categorization = recode(categorization, 
                                 "F" = "f", "kvinna" = "f", "female" = "f", "female " = "f", "Female"= "f", "Fenale"= "f", "women" = "f", "woman " = "f", "femLE" = "f", "FEmale" = "f", "Femalw" = "f", "Fwmalw" = "f", "Female " = "f", "woman" = "f", "Woman" = "f", "feMale" = "f", "fermale" = "f", "wman" = "f", "Femae" = "f",
                                 "man" = "m","Male " = "m", "make" = "m", "Male"= "m", "male" = "m", "man " = "m",  "male " = "m", "guy" = "m", "boy" = "m", "Make" = "m", "M"  = "m", "Man" = "m", "Bottom half male; above nose female., Would have to say Male" = "m", " male" = "m", "male  " = "m", "ale" = "m", "nmale" = "m", "MALE"= "m", "nale"= "m", " Male" = "m",
                                 "Nonbinary" = "o", "Non Binary " = "o", "Unsure" = "o", "Non binary " = "o", "good" = "o", "Neutral" = "o", "neutral" = "o", "nonbinary" = "o", "bigender" = "o", "hen" = "o", "don't know"  = "o", "Bottom half male, nose upwards female" = "o" ))%>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal() +
  facet_wrap(~race)

#subsetting by id
d %>%
  filter(condition == "ft") %>% 
  group_by(masc, race, id) %>% 
  mutate(categorization = recode(categorization, 
                                 "F" = "f", "kvinna" = "f", "female" = "f", "female " = "f", "Female"= "f", "Fenale"= "f", "women" = "f", "woman " = "f", "femLE" = "f", "FEmale" = "f", "Femalw" = "f", "Fwmalw" = "f", "Female " = "f", "woman" = "f", "Woman" = "f", "feMale" = "f", "fermale" = "f", "wman" = "f", "Femae" = "f",
                                 "man" = "m","Male " = "m", "make" = "m", "Male"= "m", "male" = "m", "man " = "m",  "male " = "m", "guy" = "m", "boy" = "m", "Make" = "m", "M"  = "m", "Man" = "m", "Bottom half male; above nose female., Would have to say Male" = "m", " male" = "m", "male  " = "m", "ale" = "m", "nmale" = "m", "MALE"= "m", "nale"= "m", " Male" = "m",
                                 "Nonbinary" = "o", "Non Binary " = "o", "Unsure" = "o", "Non binary " = "o", "good" = "o", "Neutral" = "o", "neutral" = "o", "nonbinary" = "o", "bigender" = "o", "hen" = "o", "don't know"  = "o", "Bottom half male, nose upwards female" = "o" ))%>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal() +
  facet_wrap(~id)

##Binary#######
d %>% filter(condition == "xb") %>%
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()

#subsetting by race
d %>% filter(condition == "xb") %>%
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  facet_wrap(~race) +
  theme_minimal()

##multiple categories######
d %>% 
  filter(condition == "mc") %>% 
  mutate(categorization = recode(categorization, #kludge-y fix to make the categories appear in the same order as in the previous plots
                                 "1" = "1. Woman",
                                 "2" = "2. Man", 
                                 "3" = "3. Other", 
                                 "4" = "4. Unknown")) %>% 
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()

d %>% 
  filter(condition == "mc") %>% 
  mutate(categorization = recode(categorization, "1" = "1. Woman", "2" = "2. Man", "3" = "3. Other", "4" = "4. Unknown")) %>% 
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal() +
  facet_wrap(~race)

d %>% 
  filter(condition == "mc") %>% 
  mutate(categorization = recode(categorization, "1" = "1. Woman", "2" = "2. Man", "3" = "3. Other", "4" = "4. Unknown")) %>% 
  group_by(masc, race, id) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal() +
  facet_wrap(~id)


## single dimension #######

d %>% 
  filter(condition == "sd") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc) %>% 
  summarise(mean_rating = mean(categorization))  %>% 
  ggplot(aes(x=masc, y=mean_rating, group = 1)) +
  geom_line()+
  geom_point()+
  theme_minimal()

####subset by race###
d %>%
  filter(condition == "sd") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, race) %>% 
  summarise(mean_rating = mean(categorization))  %>% 
  ggplot(aes(x=masc, y=mean_rating, col = race, group = 1)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  facet_wrap(~race)

d %>%
  filter(condition == "sd") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, id) %>% 
  summarise(mean_rating = mean(categorization))  %>% 
  ggplot(aes(x=masc, y=mean_rating, group = 1)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  facet_wrap(~id)

##multiple dimension #####
d %>% 
  filter(condition == "md") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, scale) %>% 
  summarise(mean_rating = mean(categorization)) %>% 
  ggplot(aes(x=masc, y=mean_rating, group = scale)) +
  geom_line(aes(color = scale))+
  geom_point(aes(color = scale))+
  theme_minimal()

###subset by race
d %>% 
  filter(condition == "md") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, scale, race) %>% 
  summarise(mean_rating = mean(categorization)) %>% 
  ggplot(aes(x=masc, y=mean_rating, group = scale)) +
  geom_line(aes(color = scale))+
  geom_point(aes(color = scale))+
  theme_minimal() +
  facet_wrap(~race)

### by id
d %>% 
  filter(condition == "md") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, scale, id) %>% 
  summarise(mean_rating = mean(categorization)) %>% 
  ggplot(aes(x=masc, y=mean_rating, group = scale)) +
  geom_line(aes(color = scale))+
  geom_point(aes(color = scale))+
  theme_minimal() +
  facet_wrap(~id)


