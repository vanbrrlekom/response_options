library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

#starting
read_csv("data/cat2stduy1_data.csv") %>% 
  mutate(across(!starts_with("Gender"), as.character)) %>%  
  #arrange(condition.1) %>% 
  pivot_longer(
    cols = !c( `Age:1`, starts_with("Gender"), starts_with("choose"), contains("assign"), starts_with("Prolific"), starts_with("TIME")),
    names_to = "face",
    values_to = "categorization") %>% 
  #separate(face, c("condition", "face", "morph", "scale")) %>% 
  select( face, categorization) %>% 
  filter(!categorization == "") %>%   
  filter(
    str_starts(face, "mc")) %>% 
  mutate(id = 1:nrow(.)) %>% 
  group_by(face,categorization) %>% 
  count(categorization) %>% 
  filter(!is.na(categorization))%>% 
  ggplot(aes(x=face, y=n, fill=categorization)) +
  geom_bar(stat="identity", position = "fill") +
  scale_fill_viridis_d(labels = c("1" = "Man", "2" = "Woman", "3" = "Non-binary", "4" = "I don't know"))+
  theme_classic() + 
  scale_x_discrete(label = abbreviate)


read.csv("data/cat2study1_pilot3_2.csv") %>% 
  mutate(across(!starts_with("Gender"), as.character)) %>%  
  mutate(id = nrow(.)+1:nrow(.)+1+nrow(.))%>% 
  select(!Age.1) %>% 
  pivot_longer(
    cols = starts_with("mc"),
    names_to = "face",
    values_to = "categorization") %>% 
  select(face, id,  categorization, Gender.1) %>% 
  group_by(face,categorization) %>% 
  count(categorization) %>% 
  filter(!is.na(categorization))%>% 
  ggplot(aes(x=face, y=n, fill=categorization)) +
  geom_bar(stat="identity", position ="fill") +
  #scale_fill_manual(labels = c("man", "woman", "non-binary", "don't know"))+
  scale_fill_viridis_d(labels = c("1" = "Man", "2" = "Woman", "3" = "Non-binary", "4" = "I don't know"))+
  theme_classic() +
  coord_flip()


  