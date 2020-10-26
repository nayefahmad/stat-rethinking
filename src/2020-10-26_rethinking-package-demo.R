
#******************************************************************
#* Statistical Rethinking 
#* 2020-10-16
#* Nayef 
#*
#******************************************************************

library(tidyverse)
library(rethinking)


# set up the data ---------
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]


# define a model ----------
flist <- 
    alist(
        height ~ dnorm(mu, sig), 
        mu ~ dnorm(179, 20), 
        sig ~ dunif(0, 50)
    )


# use quadratic approx to get the posterior ------
m1 <- quap(flist, data=d2)


# summaries of the posterior ------ 
summary(m1)
precis(m1)


# Samples from the posterior ------
post_samples <- extract.samples(m1, 100) 


# data for prediction interval ----- 
rnorm(1000, mean = post_samples$mu, sd = post_samples$sig)

