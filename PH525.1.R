# PH525.1  wk 2

getwd()
#install.packages("downloader")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

mean(x)

set.seed(1)
abs(mean(sample(x, 5)) - mean(x))

set.seed(5)
abs(mean(sample(x, 5)) - mean(x)) 

## NULL distribution
# 1 & 2 & 3
n <- 1000
avg <- mean(x)
set.seed(1)
s <- list()
for (i in 1:n) {
  s[i] <- mean(sample(x, 50))
}
sum(abs(unlist(s) - avg) > 1) / n
hist(unlist(s))

## Prob distribution
install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

x <- gapminder[gapminder$year == 1952,]$lifeExp

mean(x <= 40) 

mean(x <= 60) - mean(x <= 40)

prop = function(q) {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
plot(ecdf(x))

## normal distribution

normsample <- function(n,s,x) {
  set.seed(1)
  myList <- list()
  for (i in 1:n) {
    myList[i] <- mean(sample(x, s))
  }
  myList
}
x <- unlist( read.csv(filename) ) # from up eariler
avg <- mean(x)
ns_5 <- normsample(1000,5,x)
ns_50 <- normsample(1000,50,x)
par(mfrow = c(2,1))
hist(unlist(ns_5))
hist(unlist(ns_50))
par(mfrow = c(1,1))

pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43)

## population, samples and estimates
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )

install.packages("rafalib")
library(rafalib)
library(dplyr)
x <- dat %>% filter(Sex == "F" & Diet == "chow") 
# dim(x)
paste("variance F chow", popsd(x$Bodyweight))
# summarize(x, meanbw=mean(Bodyweight))
set.seed(1)
x_sam <- sample(x$Bodyweight,25)
# mean(x_sam)
y <- dat %>% filter(Sex == "F" & Diet == "hf") 
# dim(y)
paste("var F and HF:", popsd(y$Bodyweight))
set.seed(1)
y_sam <- sample(y$Bodyweight,25)
# mean(y_sam)

abs(abs(mean(y_sam) - mean(x_sam)) - abs(mean(x$Bodyweight) - mean(y$Bodyweight)))

#####  CLT
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)

y <- dat %>% filter(Sex == "M" & Diet == "chow") 
mc <- mean(y$Bodyweight)
popsd <- popsd(y$Bodyweight)
# What proportion of the mice are within one standard deviation away from the average weight (remember to use popsd for the population sd)
# so, 1 minus the porp of those under mc-popsd + those mice over mc+popsd.  This is the hard way...?  
# isn't the best way to do this becuse if I want 2 standard deviations, then you need to 2*popsd
1 - (mean(y$Bodyweight < mc-popsd) + mean(y$Bodyweight > mc+popsd))

# another way to do it is standardize the body weights, so...
z <- (y$Bodyweight - mean(y$Bodyweight)) / popsd(y$Bodyweight)
mean(abs(z) <= 1)  # all weights that are within 1 sd
mean(abs(z) <= 2)  # within 2 sd.
mean(abs(z) <= 3)  # within 3...

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z, main = "M / chow (ctrl)");abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z, main = "F / chow (ctrl)");abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z, main = "M and hf");abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z, main = "F and hf");abline(0,1)
mypar(1,1)

#9 
set.seed(1)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs); sd(avgs)
