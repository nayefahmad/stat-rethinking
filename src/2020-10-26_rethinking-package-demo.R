
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

hist(df2$height)

# > define a model ----------
flist <- 
    alist(
        height ~ dnorm(mu, sig),  # data model/likelihood 
        mu ~ dnorm(179, 20),  # prior for mean 
        sig ~ dunif(0, 50)    # prior for sig 
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





# Example 2: Height and weight: -------
# > define a model ----------
flist2 <- 
    alist(
        height ~ dnorm(mu, sig), 
        mu ~ a + b*(weight - mean(df2$weight)),   # weight values are centered (mean = 0) 
        a ~ dnorm(178, 20), 
        b ~ dlnorm(0,1), 
        sig ~ dunif(0, 50)
    )


# > use quadratic approx to get the posterior ------
m2 <- quap(flist2, data=df2)


# > summaries of the posterior ------ 
summary(m2)
precis(m2)


# > samples from the posterior ------
post_samples2 <- extract.samples(m2, 100) 


# > data for prediction interval ----- 
weight1 <- 2  # specific value of weight (centered)
sim_heights2 <- rnorm(1000, 
                     mean = post_samples2$a + post_samples2$b*weight1, 
                     sd = post_samples2$sig)
hist(sim_heights2)

pred_interval2 <- quantile(sim_heights2, c(.05, .95))
pred_interval2

# This gives a prediction interval that should contain the stated proportion of 
# future height values. 

