# PH525.1  wk 2

getwd()
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