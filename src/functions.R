#Manipulation check is designed for categorizations1_data.csv. It makes sure that every participant
#used the correct pronoun three times. 


read_data <- function(file){ read.csv(file) %>% 
  mutate_all(as.character) %>%
  mutate(id = 1:nrow(.),
         pronoun = ifelse(they.1 == "", "he", "they")) %>% 
  pivot_longer(
    cols = starts_with("mc"),
    names_to = "face",
    values_to = "categorization") %>% 
  separate(face, c("condition", "trial"), sep = "_") %>% 
  select(pronoun, id, trial, categorization, Age.1, Gender.1) %>% 
  filter(!categorization == "") %>% 
  mutate(trial = recode(trial, "1.1" = "01.1", "2.1" = "02.1", "3.1" = "03.1", "4.1" = "04.1", 
                        "5.1" = "05.1", "6.1"="06.1", "7.1"="07.1", "8.1"="08.1", "9.1"="09.1"))
  }

  d <- d[-c(1:2),]
}

manipulation_check <- function(d){
  #manipulation check
  conditions <- c("they.1", "they.2", "they.3", "han.1", "han.2", "han.3")

  #First, create a column called condition in d with the first three letters in each of
  #the three sentences
  d_conditions <- apply( d[,conditions], 2 , substr, start = 1, stop = 3 ) 
  d_conditions <-  t(apply(d_conditions, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) %>%
    data.frame() %>% select(1:3)
  d$condition_tmp <- paste(d_conditions[,1], d_conditions[,2], d_conditions[,3], sep = "") %>%
    tolower()
  
  #then check if they used the same pronoun in the all three sentencs AND if they weren't
  #in the control condition. This involves creating a function within the function, doublecheck.
  d$completed <- d$condition_tmp == "henhenhen"|d$condition_tmp == "hanhanhan"| d$condition_tmp == "honhonhon"
  doublecheck <- function(x){
    ifelse(d$completed == FALSE, grepl(x, d$condition_tmp), F) 
  }
  d$doublechecks <- doublecheck("han") == T & doublecheck("hon") == T & doublecheck("hen") == T
  d$condition <- ifelse(d$completed == F, "control", d$condition_tmp) %>% 
    ifelse(d$doublechecks == T, NA, .) %>% 
    recode(., "henhenhen" = "hen", "honhonhon" = "hon", "hanhanhan" = "han")
  
  for (i in c("hen", "na", "han", "hon")){
    d$condition <- ifelse(d$completed == F & grepl(i, d$condition_tmp) == T, NA, d$condition ) 
  }
  return(d)
}

triple_check <- tibble(d$condition, d$conditions, d$hen1, d$han1, d$hon1, d$control1) %>% filter(!is.na(d$condition))

#standardize a variable
standardize <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}


post <- posterior_samples(cat_multilevel_m2)[,1:10] %>% 
  rename(p_hen = b_conditionhen, 
         p_hon = b_conditionhon,
         p_han = b_conditionhan,
         p_control = b_conditioncontrol) %>% 
  select(starts_with("p")) %>% 
  pivot_longer(cols= 1:4, names_to = "parameter") %>% 
  group_by(parameter) 

p <- post %>% 
  transmute(
    "control-hen" = p_hen - p_control, 
    "control-hon" =   p_hon-p_control, 
    "control-han" =  p_han - p_control, 
  )  %>% precis(p, prob = 0.95)

