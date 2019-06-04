##Creating a Binomial Sampling Distribution for a coin toss with a prob of 0.5
hist(replicate(1000,sum(rbinom(10,1,.5)==1)))

