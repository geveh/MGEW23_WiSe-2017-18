## Bayes Theorem für eine diskrete Prior Wahrscheinlichkeitsverteilung

# Initialparameter
i <- 99 # Anzahl an Priors
x <- 1 # Ja Aussagen
y <- 2 # Nein Aussagen

# Prior
t <- seq(from = 0, to = 1, length.out = i) # ts zwischen 0 und 1
pt_norm <- dnorm(t, mean = 0.5, sd = 1) # normalverteilter Vektor der Länge t
pt <- pt_norm/sum(pt_norm) # Vektor auf 1 normiert

# Likelihood
like <- (t^x)*((1-t)^y) # Ist Bernoulli-verteilt

# Evidence
evi <- (like*pt)+((1-like)*(1-pt))
# evi <- sum(like*pt) # Diese Zeile geht auch
  
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

