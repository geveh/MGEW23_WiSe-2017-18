## Bayes Theorem für eine diskrete Prior Wahrscheinlichkeitsverteilung

# Initialparameter
i <- 19 # Anzahl an Priors
x <- 155 # Ja Aussagen
y <- 45 # Nein Aussagen

# Prior
t <- seq(from = 0, to = 1, length.out = i+2) # ts zwischen 0 und 1
pt_norm <- dnorm(t, mean = 0.5, sd = 1) # normalverteilter Vektor der Länge t
pt <- pt_norm/sum(pt_norm) # Vektor auf 1 normiert

# Likelihood
like <- (pt^x)*((1-pt)^y) # Ist Bernoulli-verteilt

# Evidence
evi <- (like*pt)+((1-like)*(1-pt))
  
# Posterior
post <- (like * pt) / evi

# Plots
par(mfrow=c(3,1)) # Die nächsten drei Plots untereinander
plot(t, pt, main = "Prior", type = "h", lwd = 4) # Prior
abline(v = 0.5, col = "red")
plot(t,like, main = "Likelihood", type = "h", lwd = 4) # Likelihood
abline(v = 0.5, col = "red")
plot(t, post, main = "Posterior", type = "h", lwd = 4) # Posterior
abline(v = 0.5, col = "red")

