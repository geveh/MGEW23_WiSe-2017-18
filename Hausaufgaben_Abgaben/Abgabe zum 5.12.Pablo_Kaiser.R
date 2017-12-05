# Prior plot

par(mfrow=c(3,1))#combining plots

theta = seq(0.1,0.9,0.1)
prior1 = dnorm(theta,0.1,0.1)
prior = prior1/sum(prior1)

plot(theta,prior,
     type = "h") 




#prior2 <- beta(1,30)


#Likelihood plot
Theta = seq(0.01,0.09,0.01)
#Data|Theta = seq(0.01,0.99,0.01)
likelihood = function(n,k,Theta){return(Theta^k*(1-Theta)^(n-k))} #Theta <- Probability; k <- observed value; <-n fixed value
like <- likelihood(12,7,Theta)
plot(Theta,like,type = "h")
box()

#Posterior plot
Posterior1=like*prior
Posterior=Posterior1/sum(Posterior1)
plot(theta,Posterior,type= "h")

