
library(brms)
# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes it reproducable also if random number generators are used
options (mc.cores=parallel::detectCores ()) # Run on multiple cores
#-------------------------------------------------------------------------------

library(tidyr)
library(brms)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
source("src/functions.r")

#initiatinggg!!

#maniplation check
he <- c("he", "his")
they <- c("they", "their")

prounoun_check <- function(x, pronoun){
  sum(pronoun %in% x)>0
}

#read data and do a bunch of stuff
d  <- read.csv("data/cat2study1_pilot4.csv") %>% 
  mutate(id = 1:nrow(.),
         they = paste(they.1, they.2, they.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, they) %>% 
           ifelse("they", ""),
         he = paste(han.1, han.2, han.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, he) %>% 
           ifelse("he", "")) %>% 
  mutate(pronoun = paste(they, he) %>% 
           na_if(" ")) %>% 
  select(!Age.1) %>% 
  pivot_longer(
    cols = starts_with(c("a", "b", "w")),
    names_to = "face",
    values_to = "categorization") %>% 
 select(face, pronoun, id,  categorization, Gender.1) 


d1  <- read.csv("data/cat2study1_pilot3_2.csv") %>% 
  mutate(id = nrow(d)+1:nrow(.)+1+nrow(d),
         they = paste(they.1, they.2, they.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, they) %>% 
           ifelse("they", ""),
         he = paste(han.1, han.2, han.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, he) %>% 
           ifelse("he", "")) %>% 
  mutate(pronoun = paste(they, he) %>% 
           na_if(" ")) %>% 
  select(!Age.1) %>% 
  pivot_longer(
    cols = starts_with("mc"),
    names_to = "face",
    values_to = "categorization") %>% 
  select(face, pronoun, id,  categorization, Gender.1) 

d2  <- read.csv("data/cat2study1_pilot5.csv") %>% 
  mutate(id = nrow(d)+1:nrow(.)+1+nrow(d),
         they = paste(they.1, they.2, they.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, they) %>% 
           ifelse("they", ""),
         he = paste(han.1, han.2, han.3) %>% 
           tolower() %>% 
           strsplit(" ") %>% 
           map(prounoun_check, he) %>% 
           ifelse("he", "")) %>% 
  mutate(pronoun = paste(they, he) %>% 
           na_if(" ")) %>% 
  select(!Age.1) %>% 
  pivot_longer(
    cols = starts_with("a")|starts_with("b")|starts_with("w"),
    names_to = "face",
    values_to = "categorization") %>% 
  select(face, pronoun, id,  categorization) 


d3  <- read.csv("data/cat2_pronounpilot.csv") %>% 
  mutate(id = nrow(d)+1:nrow(.)+1+nrow(d)) %>% 
  #select(!Age.1) %>% 
  pivot_longer(
    cols = starts_with("mc"),
    names_to = "face",
    values_to = "categorization") %>% 
  select(face, id,  categorization) 

#overall, what categorizations do people use?
d2 %>% group_by(pronoun, categorization) %>% 
  count()


#make a plot for all the faces
d %>% select(face, categorization) %>% na.omit() %>% group_by(face, categorization) %>% 
  count() %>%
  ggplot(aes(x = face, y = n, fill = categorization)) +
  geom_bar(stat = "identity", position = "fill")



d3 %>% select(face, categorization) %>% na.omit() %>% group_by(face, categorization) %>% 
  count() %>%
  ggplot(aes(x = face, y = n, fill = categorization)) +
  geom_bar(stat = "identity", position = "fill")

SS#just asian faces
d %>%
  mutate(first_face = substr(face, 1,1)) %>% 
  #filter(first_face == "a") %>% 
  group_by( categorization, first_face) %>%
  count() %>% 
  ggplot(aes(x = first_face, y = n, fill = categorization)) +
  geom_bar(stat = "identity", position = "fill")


### exploration for binary, fitting to bernoulli models
dx <- d %>%
  mutate(resp = categorization == 3 | categorization == 4) %>% 
  mutate(masc = as.factor(masc))

fit1 <- brm(data = dx,
            family = bernoulli(link = logit),
            resp ~ 0 + pronoun + masc + (1|id),
            prior = c(prior(normal(0, 3), class = b),
                      prior(normal(0, 3), class = sd)),
            iter = 2000, warmup = 1000, chains = 2, cores = 2,
            seed = 13,
            file = "Models/fit_xb_")

fit2 <- brm(data = dx,
            family = bernoulli(link = logit),
            resp ~ 0 + pronoun  + (1|id) + (1|face),
            prior = c(prior(normal(0, 3), class = b),
                      prior(normal(0, 3), class = sd)),
            iter = 2000, warmup = 1000, chains = 2, cores = 2,
            seed = 13,
            file = "Models/fit2")

fit3 <- brm(data = dx,
            family = bernoulli(link = logit),
            resp ~ 0 + pronoun:masc  + (1|id) + (1|trial),
            prior = c(prior(normal(0, 3), class = b),
                      prior(normal(0, 3), class = sd)),
            iter = 2000, warmup = 1000, chains = 2, cores = 2,
            seed = 13,
            file = "Models/fit3")

fit4 <- brm(data = dx,
            family = bernoulli(link = logit),
            resp ~ 0 + pronoun:pair  + (1|id) + (1|trial),
            prior = c(prior(normal(0, 3), class = b),
                      prior(normal(0, 3), class = sd)),
            iter = 2000, warmup = 1000, chains = 2, cores = 2,
            seed = 13,
            file = "Models/fit4")



### testing the whole pilot data set
e <-rbind(d, d1) %>%
  mutate(resp = categorization == 3 | categorization == 4) 


fit2_more_p <- brm(data = e,
            family = bernoulli(link = logit),
            resp ~ 0 + pronoun  + (1|id) + (1|face),
            prior = c(prior(normal(0, 3), class = b),
                      prior(normal(0, 3), class = sd)),
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 13,
            file = "Models/fit2_more_p")
