sample_data <- read.csv("data/sample.csv")
sample_data2 <- sample_n(sample_data, 10)

library(ggplot2)

ggplot(sample_data, aes(fill=Crime.Name1, y=Victims, x=Place)) + 
  geom_bar(position="fill", stat="identity")