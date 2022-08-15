
library(tidyverse)
library(cobalt)

set.seed(42)
n_pos_sample = 100
match_ratio = 10
n_total = n_pos_sample*(1 + 2*match_ratio)

df = tibble(
  person_time = rnorm(n_total, 12, 2),
  health_contact = rpois(n_total, 10),
  status = c(
    rep("Pos", n_pos_sample),
    rep("Neg", n_pos_sample*match_ratio),
    rep("Unt", n_pos_sample*match_ratio)) %>% 
    factor() %>% 
    fct_relevel("Unt")
) %>%
  rowwise() %>% 
  mutate(
    sex = case_when(
      status == "Pos" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.5, 0.5)),
      status == "Neg" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.5, 0.5)),
      status == "Unt" ~ sample(c("M", "F"), 1, replace = TRUE, prob = c(0.7, 0.3))
    ),
    asthma = case_when(
      status == "Pos" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.7, 0.3)),
      status == "Neg" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.5, 0.5)),
      status == "Unt" ~ sample(c("Yes", "No"), 1, replace = TRUE, prob = c(0.3, 0.7))
    ),
    age_group = case_when(
      status == "Pos" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.1, 0.15, 0.25, 0.5)),
      status == "Neg" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.1, 0.25, 0.25, 0.4)),
      status == "Unt" ~ sample(c("4-6", "7-10", "11-14", "15-18"), 1, prob = c(0.5, 0.15, 0.15, 0.1))
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    age_group = age_group %>% 
      factor() %>% 
      fct_relevel(c("4-6", "7-10", "11-14", "15-18")),
  )


#Using WeightIt to generate weights with multinomial
#logistic regression
W.out.mn <- WeightIt::weightit(status ~ sex + asthma + age_group, data = df,
                               method = "ps", use.mlogit = FALSE)

#Balance summary across treatment pairs
bal.tab(W.out.mn, un = TRUE)

#Assessing balance for each pair of treatments
bal.tab(W.out.mn, un = TRUE, disp.means = TRUE, which.treat = .all)

x = bal.tab(W.out.mn, un = TRUE, disp.means = TRUE, which.treat = .all)

df = df %>%
  ungroup() %>% 
  mutate(weights = W.out.mn$weights)

(1/df$weights) %>% sum()

#Assessing balance graphically
bal.plot(W.out.mn, "sex", which = "both")
bal.plot(W.out.mn, "asthma", which = "both")
bal.plot(W.out.mn, "age_group", which = "both")

#Summarizing balance in a Love plot
love.plot(W.out.mn, thresholds = c(m = .1), binary = "std",
          which.treat = .all, abs = TRUE, position = "bottom")

## Model healthcare contacts using poisson regression
library(MASS)
library(broom)

fit_pois = glm(health_contact ~ status + sex + asthma + offset(person_time),
               weights = W.out.mn$weights,
               data = df,
               family = poisson)
fit_pois %>% 
  tidy(conf.int = TRUE, exponentiate = TRUE)

## Model healthcare contacts using negative binomial regression
fit_nb = glm.nb(health_contact ~ status + sex + asthma + offset(person_time),
                weights = W.out.mn$weights,
                data = df,
                maxit = 100)
fit_nb %>% 
  tidy(conf.int = TRUE, exponentiate = TRUE)
