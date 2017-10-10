#Aggregates data ready for bar plotting via.
#This means that plotting code length depends
#on the number of levels, not on the length of v
aggregateForBarplot <- function(v) {
  outF <- data.frame(table(v))
  names(outF) <- c("x", "y")
  outF
}


#testing
#a <- sample(letters[1:4], 100, replace = TRUE, prob = c(0.1, 0.6, 0.2, 0.1))
#d <- data.frame(table(a))
#ggplot(data.frame(a = a), aes(x = a)) +
#  geom_bar()
#ggplot(aggregateForBarplot(a), aes(x = x, y = y)) +
#  geom_bar(stat = "identity")
