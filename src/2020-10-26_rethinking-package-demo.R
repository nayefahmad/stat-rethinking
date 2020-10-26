
#******************************************************************
#* Statistical Rethinking 
#* 2020-10-16
#* Nayef 
#*
#******************************************************************

library(tidyverse)
library(rethinking)

# Example 1: height data --------
# > set up the data ---------
data("Howell1")
df1 <- Howell1
df2 <- df1 %>% filter(age >= 18)


# > define a model ----------
flist <- 
    alist(
        height ~ dnorm(mu, sig), 
        mu ~ dnorm(179, 20), 
        sig ~ dunif(0, 50)
    )


# > use quadratic approx to get the posterior ------
m1 <- quap(flist, data=df2)


# > summaries of the posterior ------ 
summary(m1)
precis(m1)


# > samples from the posterior ------
post_samples <- extract.samples(m1, 100) 


# > data for prediction interval ----- 
sim_heights <- rnorm(1000, mean = post_samples$mu, sd = post_samples$sig)
hist(sim_heights)

pred_interval <- quantile(sim_heights, c(.05, .95))

# This gives a prediction interval that should contain the stated proportion of 
# future height values. 
