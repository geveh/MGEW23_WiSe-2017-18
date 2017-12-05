################

# Theta is the vector of candidate values for the parameter theta
# nThetaVals is the number of candidate theta values
# To produce the examples in the book (Kruschke, 2011), set nThetaVals to either 3 or 63
nThetaVals = 11
# Vector of theta values:
Theta <- seq(from = 1 / (nThetaVals + 1), to = nThetaVals / (nThetaVals + 1),
             by = 1 / (nThetaVals + 1))

# pTheta is the vector of prior probabilities on the theta values
pTheta <- pmin(Theta, 1 - Theta)    # Makes a triangular belief distribution
#pTheta <- rep(1/nThetaVals, nThetaVals)    # Makes a non-informative belief distribution
pTheta <- pTheta / sum(pTheta)      # Makes sure that beliefs sum to 1 (normalized)

# Specify the data. To produce examples in the book, use either
#Data <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#Data <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Data <- c(rep(1, 1), rep(0, 2))
#Data <- rep(round(runif(1000)))
nHeads <- sum(Data == 1)
nTails <- sum(Data == 0)

# Compute likelihood of data for each value of theta (Bernoulli)
pDataGivenTheta <- Theta ^ nHeads * (1 - Theta) ^ nTails
#pDataGivenTheta <- choose(sum(nHeads,nTails),nHeads) * Theta ^ nHeads * (1 - Theta) ^ nTails

# Compute the posterior
pData <- sum(pDataGivenTheta * pTheta)
pThetaGivenData <- pDataGivenTheta * pTheta / pData      # BAYES' RULE

# Plot
#quartz(7, 10)         # Create window of specified size
layout(matrix(c(1, 2, 3), nrow = 3, ncol = 1, byrow = FALSE))   # 3 x 1 panels
par(mar = c(3, 3, 1, 0))          # number of margin lines: bottom, left, top, right
par(mgp = c(2, 1, 0))             # which margin lines to use for labels
par(mai = c(0.5, 0.5, 0.3, 0.1))  # margin size in inches: bottom, left, top, right

# Plot the prior
plot(Theta, pTheta, type = "h", lwd = 3, main = "Prior",
     xlim = c(0, 1), xlab = bquote(theta),
     ylim = c(0, 1.1 * max(pThetaGivenData)), ylab = bquote(p(theta)),
     cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.5)

# Plot the likelihood
plot(Theta, pDataGivenTheta, type = "h", lwd = 3,
     main = "Likelihood",
     xlim = c(0, 1), xlab = bquote(theta),
     ylim = c(0, 1.1 * max(pDataGivenTheta)), ylab = bquote(paste("p(D|", theta, ")")),
     cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.5)
text(0.2, 0.85 * max(pDataGivenTheta), cex = 2.0,
     bquote("D: " * .(nHeads) * " correct and " * .(nTails) * " incorrect reports"), adj = c(0, 0.5))

# Plot the posterior
plot(Theta, pThetaGivenData, type = "h",lwd = 3, main = "Posterior",
     xlim = c(0, 1), xlab = bquote(theta),
     ylim = c(0, 1.1 * max(pThetaGivenData)), ylab = bquote(paste("p(", theta, "|D)")),
     cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.5)
text(0.55, 0.85 * max(pThetaGivenData), cex = 2.0,
     bquote("p(D) = " * .(signif(pData, 3))), adj = c(0, 0.5)) 

