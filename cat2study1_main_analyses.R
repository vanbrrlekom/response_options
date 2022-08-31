library(tidyverse)
library(brms)
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

#initiatinggg!!
d  <- read.csv("data/cat2stduy1_data.csv") %>% 
  mutate_all(as.character) %>% 
  mutate(id = 1:nrow(.)) %>% 
  arrange(condition.1) %>% 
  pivot_longer(
    cols = !c(condition.1, id, Age.1, starts_with("Gender"), starts_with("choose"), contains("assign"), starts_with("Prolific"), starts_with("TIME")),
    names_to = "face",
    values_to = "categorization") %>% 
  #separate(face, c("condition", "face", "morph", "scale")) %>% 
  select(id, face, categorization, Age.1, Gender.1) %>% 
  filter(!categorization == "") %>%   
  separate(face, c("condition", "face", "morph", "scale")) %>% 
  mutate(race = substr(face, 1,1),
         masc = recode(morph, "0" = "0", "1" = "16.67", "2"= "33.33","3" = "50",
                       "4" = "66.67", "5" ="83.33", "6" =  "100")) %>% 
  mutate(masc = as.numeric(masc)) %>% 
  mutate(masc = ifelse(race != "w", masc, 100 - masc))

##Plotting the results for free text #######

ft <- d %>%
  filter(condition == "ft") %>% 
  group_by(masc, race) %>% 
  mutate(categorization = recode(categorization, 
                                 "F" = "f", "kvinna" = "f", "female" = "f", "female " = "f", "Female"= "f", "Fenale"= "f", "women" = "f", "woman " = "f", "femLE" = "f", "FEmale" = "f", "Femalw" = "f", "Fwmalw" = "f", "Female " = "f", "woman" = "f", "Woman" = "f", "feMale" = "f", "fermale" = "f", "wman" = "f", "Femae" = "f",
                                 "man" = "m","Male " = "m", "make" = "m", "Male"= "m", "male" = "m", "man " = "m",  "male " = "m", "guy" = "m", "boy" = "m", "Make" = "m", "M"  = "m", "Man" = "m", "Bottom half male; above nose female., Would have to say Male" = "m", " male" = "m", "male  " = "m", "ale" = "m", "nmale" = "m", "MALE"= "m", "nale"= "m", " Male" = "m",
                                 "Nonbinary" = "o", "Non Binary " = "o", "Unsure" = "o", "Non binary " = "o", "good" = "o", "Neutral" = "o", "neutral" = "o", "nonbinary" = "o", "bigender" = "o", "hen" = "o", "don't know"  = "o", "Bottom half male, nose upwards female" = "o" )) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  #scale_fill()+
  theme_minimal()

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


#Plotting the results for binary options#######
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

#plotting for multiple categories######
mc <- d %>% 
  filter(condition == "mc") %>% 
  mutate(categorization = recode(categorization, "1" = "1. Woman", "2" = "2. Man", "3" = "3. Other", "4" = "4. Unknown")) %>% 
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()

mc_race <- d %>% 
  filter(condition == "mc") %>% 
  mutate(categorization = recode(categorization, "1" = "1. Woman", "2" = "2. Man", "3" = "3. Other", "4" = "4. Unknown")) %>% 
  group_by(masc, race) %>% 
  count(categorization) %>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal() +
  facet_wrap(~race)


# single dimension

d %>% 
  filter(condition == "sd") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc) %>% 
  summarise(mean_rating = mean(categorization))  %>% 
  ggplot(aes(x=masc, y=mean_rating, group = 1)) +
  geom_line()+
  geom_point()+
  theme_minimal()

#subset by race
d %>%
  filter(condition == "sd") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, race) %>% 
  summarise(mean_rating = mean(categorization))  %>% 
  ggplot(aes(x=masc, y=mean_rating, group = 1)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  facet_wrap(~race)


# multiple dimension
d %>% 
  filter(condition == "md") %>% 
  mutate(categorization = as.numeric(categorization)) %>% 
  group_by(masc, scale) %>% 
  summarise(mean_rating = mean(categorization)) %>% 
  ggplot(aes(x=masc, y=mean_rating, group = scale)) +
  geom_line(aes(color = scale))+
  geom_point(aes(color = scale))+
  theme_minimal()

#subset by race
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


