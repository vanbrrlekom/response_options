
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
source("src/functions.r")

#initiatinggg!!

d  <- read.csv("data/cat2study1_pilot3_2.csv") %>% 
  mutate_all(as.character) %>%
 mutate(id = 1:nrow(.)) %>% 
        # pronoun = ifelse(d$they.1 == "", "he", "they")) %>% 
  pivot_longer(
    cols = starts_with("mc"),
    names_to = "face",
    values_to = "categorization") %>% 
  separate(face, c("condition", "trial"), sep = "_") %>% 
  select( id, trial, categorization, Age.1, Gender.1) %>% 
  filter(!categorization == "") %>% 
  mutate(trial = recode(trial, "1.1" = "01.1", "2.1" = "02.1", "3.1" = "03.1", "4.1" = "04.1", 
                        "5.1" = "05.1", "6.1"="06.1", "7.1"="07.1", "8.1"="08.1", "9.1"="09.1"))


#Okay wow. This is some bullshit code. 
d$fem <- 1:nrow(d)
d$pair <- 1: nrow(d)

# first, get the key to what the various trials mean
key <- tibble(trial = unique(d$trial),
              masc = rep(c(0, 16.66, 33.33, 50, 63.33, 86.67, 100), 10),
              pair = rep(c("pair01", "pair02", "pair03", "pair04", "pair05", 
                           "pair06", "pair07", "pair08", "pair09", "pair10"), each = 7)
)
              
             
for(i in 1:nrow(d)){
  d$masc[i] =  key$masc[key$trial == d$trial[i]]
  d$pair[i] = key$pair[key$trial == d$trial[i]]
}


##Plotting the results for ft



d %>%  
  mutate(masc = as.factor(masc)) %>% 
  group_by(masc, pair) %>% 
  count(categorization)%>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  facet_wrap(~pair) +
  theme_minimal()

d %>%  
  #filter(pair != c("pair10", "pair03")) %>% 
  mutate(masc = as.factor(masc)) %>% 
  group_by(masc, pair) %>% 
  count(categorization)%>% 
  ggplot(aes(x=masc, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()

d %>%  
  #filter(pair != c("pair10", "pair03")) %>% 
  #mutate(masc = as.factor(masc)) %>% 
  group_by(pronoun, pair) %>% 
  count(categorization)%>% 
  ggplot(aes(x=pronoun, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill")+ 
  #facet_wrap(~pronoun)+
  theme_minimal()


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
            resp ~ 0 + pronoun  + (1|id) + (1|trial),
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

