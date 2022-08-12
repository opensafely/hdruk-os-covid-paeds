

library(ipw)

set.seed(16)
n <- 1000
simdat <- data.frame(l = rnorm(n, 10, 5))
a.lin <- simdat$l - 10
pa <- exp(a.lin)/(1 + exp(a.lin))
simdat$a <- rbinom(n, 1, prob = pa)
simdat$y <- 10*simdat$a + 0.5*simdat$l + rnorm(n, -10, 5)
simdat[1:5,]

library("ipw")
temp <- ipwpoint(exposure = a, family = "binomial", link = "logit",
                 numerator = ~ 1, denominator = ~ l, data = simdat)
summary(temp$ipw.weights)

ipwplot(weights = temp$ipw.weights, logscale = FALSE,
        main = "Stabilized weights", xlim = c(0, 8))

glm(formula = y ~ a,  data = simdat)
glm(formula = y ~ a,  data = simdat, weights = temp$ipw.weights)

fit_num = glm(formula = a ~ 1, family = binomial(link = "logit"), data = simdat)
fit_den = glm(formula = a ~ l, family = binomial(link = "logit"), data = simdat)

p = predict(fit_num, type = "response")
pi = predict(fit_den, type = "response")

weights = ifelse(simdat$a == 1, p/pi, (1-p) / (1-pi) )


library("ipw")
data("haartdat")
haartdat[1:10,]