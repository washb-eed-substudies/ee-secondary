
library(tidyverse)

#-------------------------------------
# Example immune marker composite score
#-------------------------------------

#3 log-normal markers in the numerator and 2 in the denominator with different ranges/concentrations

N1 <- exp(rnorm(1000, mean = .1, sd = .2))
N2 <- exp(rnorm(1000, mean = .5, sd = 1))
N3 <- exp(rnorm(1000, mean = 1, sd = 1))
D1 <- exp(rnorm(1000, mean = .1, sd = .2))
D2 <- exp(rnorm(1000, mean = 1, sd = 2))

#A raw composite index would be biased towards the higher concentration biomarkers
Y <- (N1 + N2 + N3)/(D1 + D2)
summary(Y)


#zscores would be useful in summing the biomarkers, but what about a ratio measure?
#Eith Z-scoring the raw markers or log transformations will lead to division be zero/near zero numbers and leads to extreme outliers

zN1 <-  as.vector(scale(log(N1)))
zN2 <-  as.vector(scale(log(N2)))
zN3 <-  as.vector(scale(log(N3)))
zD1 <-  as.vector(scale(log(D1)))
zD2 <-  as.vector(scale(log(D2)))

Y <- (zN1 + zN2 + zN3)/(zD1 + zD2)
summary(Y)
ggplot(data.frame(Y=Y)) + geom_density(aes(Y))


#Scaling without centering still causes issues when log-transforming as log-transformed variables have values near 0
cN1 <-  as.vector(scale(log(N1), center = FALSE, scale = apply(as.matrix(log(N1)), 2, sd, na.rm = TRUE)))
cN2 <-  as.vector(scale(log(N2), center = FALSE, scale = apply(as.matrix(log(N2)), 2, sd, na.rm = TRUE)))
cN3 <-  as.vector(scale(log(N3), center = FALSE, scale = apply(as.matrix(log(N3)), 2, sd, na.rm = TRUE)))
cD1 <-  as.vector(scale(log(D1), center = FALSE, scale = apply(as.matrix(log(D1)), 2, sd, na.rm = TRUE)))
cD2 <-  as.vector(scale(log(D2), center = FALSE, scale = apply(as.matrix(log(D2)), 2, sd, na.rm = TRUE)))

summary(cN1)
summary(cN2)
summary(cN3)


cY <- (cN1 + cN2 + cN3)/(cD1 + cD2)
summary(cY)
ggplot(data.frame(cY=cY)) + geom_density(aes(cY))







#Scaling without centering or log transformation and then log-transforming the composite score seems to be the approach 
cN1 <-  as.vector(scale((N1), center = FALSE, scale = apply(as.matrix(N1), 2, sd, na.rm = TRUE)))
cN2 <-  as.vector(scale((N2), center = FALSE, scale = apply(as.matrix(N2), 2, sd, na.rm = TRUE)))
cN3 <-  as.vector(scale((N3), center = FALSE, scale = apply(as.matrix(N3), 2, sd, na.rm = TRUE)))
cD1 <-  as.vector(scale((D1), center = FALSE, scale = apply(as.matrix(D1), 2, sd, na.rm = TRUE)))
cD2 <-  as.vector(scale((D2), center = FALSE, scale = apply(as.matrix(D2), 2, sd, na.rm = TRUE)))

summary(cN1)
summary(cN2)
summary(cN3)

summary(cD1)
summary(cD2)

sd(cN1)
sd(cN2)
sd(cN3)

sd(cD1)
sd(cD2)


cY <- (cN1 + cN2 + cN3)/(cD1 + cD2)
summary(cY)
ggplot(data.frame(cY=cY)) + geom_density(aes(cY))
#log transform outcome
ggplot(data.frame(cY=log(cY))) + geom_density(aes(cY))

