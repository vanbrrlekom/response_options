library(brms)
library(tidyr)
library(dplyr)

# Psychometric response function. 
#conditions: 
#"xb" = "Woman/man", "mc" = "multiple categories", "ft" = "Free text", "Md" = separate man/woman dimensions, "SD" = single man-woman dimension.

d  <- read.csv("data/cat2study1_data.csv")

#Wrangle data
tmp <- d %>% 
  filter(condition == "xb" | condition == "mc") %>% 
  mutate(f_cat = ifelse(categorization == "1", 1, 0))

# f_cat is a binary variable coding whether or not a face was categorized as a woman
#Experimenting with fitting a bayesian model
fit_binary_stair <- brm(woman ~ masc:condition + (1 + masc|id) + (1|face), family = bernoulli(link = 'logit'), 
          prior = c(prior(normal(0,3), class = "b"),
                    prior(normal(0,3), class = "Intercept")),
          data = tmp,
          iter = 3000, warmup = 1000,
          cores = 4,
          file = "models/fit_binary_stair")


#adding a predictor for condition and allowing conditions to vary by id
fit_binary_stair2 <- brm(f_cat ~ masc:condition + (1 + masc|id) + (1|face), family = bernoulli(link = 'logit'), 
                        prior = c(prior(normal(0,3), class = "b"),
                                  prior(normal(0,3), class = "Intercept")),
                        data = tmp,
                        iter = 3000, warmup = 1000,
                        cores = 4,
                        sample_prior = TRUE,
                        file = "models/fit_binary_stair2.1")

#Trying for an ordered logit
tmp <- d %>% 
  filter(condition == "xb" | condition == "mc") %>% 
  mutate(f_cat = ifelse(categorization == "1", 1, 0),
        fem = recode(as.factor(masc))
        )
  

formula_face_ord <- bf(f_cat | thres(gr = masc) ~ 1 + condition + (1 | id))
prior_face_ord_thres_1pl <- 
  prior("normal(0, 3)", class = "Intercept") + 
  prior("cauchy(0, 3)", class = "sd") +
  prior("normal(0, 3", class = "b")

fit_ordinal <- brm(
  formula = formula_face_ord,
  data = d,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_face_ord_thres_1pl,
  #inits = 0, 
  chains = 4,
  seed = 1234,
  cores= 4,
  file = "models/fit_face_1pl"
)


## Copy paste from Stefan's code code. 
glm_stair = glm(tmp$f_cat ~ tmp$masc, family = binomial(link = 'probit'))

x = seq(min(tmp$masc), max(tmp$masc), length.out = 1000)
y = pnorm(glm_stair$coefficients[1]+glm_stair$coefficients[2]*x)
threshold_stair = (qnorm(0.5)-glm_stair$coefficients[1])/glm_stair$coefficients[2]
plot(jitter(tmp$masc),tmp$f_cat, main = threshold_stair, col =alpha("steelblue", 0.1))
lines(x, y)
abline(v = threshold_stair, lty = 1, col = 'red')
rm(x,y)

