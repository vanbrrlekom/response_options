read_and_clean <- function(data){
  read.csv(data) %>% 
    mutate_all(as.character) %>% 
    mutate(id = 1:nrow(.)) %>% 
    pivot_longer(
      cols = !c(id, Age.1, starts_with("Gender"), starts_with("choose"), contains("assign"), starts_with("Prolific"), starts_with("TIME")),
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
  
  }


read_and_clean <- function(data){
 d <-  read.csv("data/cat2stduy1_data.csv") %>% 
    mutate_all(as.character) %>% 
    mutate(id = 1:nrow(.)) %>% 
    #arrange(condition.1) %>% 
    pivot_longer(
      cols = !c(id, Age.1, starts_with("Gender"), starts_with("choose"), contains("assign"), starts_with("Prolific"), starts_with("TIME")),
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
  
}

