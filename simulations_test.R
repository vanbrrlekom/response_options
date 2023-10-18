#This is for trying out response options ideas.


library("papaja")
library(brms)
library(tidyverse)
citations <- r_refs("r-references.bib")

source("src/functions.r")
d  <- read_and_clean("data/cat2stduy1_data.csv") %>% 
  mutate(fem = 100-masc) 

tmp <- d %>% 
  filter(condition == "sd" | condition == "md") %>% 
  mutate(f_rating = as.numeric(categorization) %>%  ifelse(scale == "f", ., 100- .),
         scale_new = ifelse(scale == "f" | scale =="m", scale, "sd"),
         fem = as.factor(fem)) 

fem <- tmp$categorization[tmp$scale == "f"] %>% as.numeric()
masc <- tmp$categorization[tmp$scale == "m"] %>% as.numeric()

plot(tmp$categorization[tmp$scale == "f"], tmp$categorization[tmp$scale == "m"])
tibble(fem,masc) %>% 
  ggplot(aes(x = masc, y = fem))+
  geom_point(alpha = 0.5) +
  theme_apa()


cor(fem,masc)
#simulating  types of outcomes. 
hist(tmp$categorization[tmp$condition == "sd"] %>% as.numeric())


library(mvtnorm) 

library(MASS)
library(faux)

sim_categorizations <- function(alpha, beta, cor){
  # Set the parameters for the beta distributions
  alpha1 <- alpha
  beta1 <- beta
  alpha2 <- alpha
  beta2 <- beta
  
  #create mean vector 
  mu = c(0, 0)
  
  #create variance covariance matrix 
  sigma <- rbind(c(1, cor), c(cor, 1))
  
  #generate 1000 random numbers 
  df <- mvrnorm(n = 20*126, mu = mu, Sigma = sigma)
  
  df.beta <- matrix(nrow = nrow(df), ncol = ncol(df))
  #normal to uniform 
  df.beta[,1] = norm2beta(df[,1], alpha1, beta1)*100
  df.beta[,2] = norm2beta(df[,2], alpha2, beta2)*100
  
  df.beta <- as.data.frame(df.beta) %>% 
    ggplot(aes(x = V1, y = V2))+
    geom_point(alpha = 0.5)+
    xlab("Masculinity")+
    ylab("Femininity") +
    theme_apa()
  
  df.beta
}

sim_categorizations(alpha = 0.2, beta = 0.2, cor = -0.95)

sim_categorizations(alpha = 2, beta = 2, cor = -0.9)

sim_categorizations(alpha = 2, beta = 2, cor = 0)

plot(tmp$categorization[tmp$scale == "f"], tmp$categorization[tmp$scale == "m"])
tibble(fem,masc) %>% 
  ggplot(aes(x = masc, y = fem))+
  geom_point(alpha = 0.5) +
  theme_classic()

tmp %>% 
  mutate(cat = as.integer(categorization)) %>% 
  arrange(condition, .by_group = TRUE) %>% 
  ggplot(aes(x = cat, fill = condition))+
  geom_histogram( binwidth = 5 )+
  facet_wrap(~id) 


data <- data.frame(y = rbeta(100, shape1 = 2, shape2 = 5), 
                   condition = rnorm(100), 
                   scale = rnorm(100))

dens(rbeta(0.4,0.5))

tmp %>% 
  rename("face_pair" = "face",
         "rating" = "categorization",
         "subj_age" = "Age.1",
         "subj_gender" = "Gender.1")%>% 
  mutate("face" = paste(face_pair, morph, sep = "_")) %>% 
  select(id, subj_age, subj_gender, face, masc, condition,scale, race, rating) %>% 
  write_csv("evb_data.csv")


