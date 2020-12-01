library(ggplot2)
data(iris)
petal_length_mean <- mean(iris$Petal.Length)


plot <- ggplot(iris,aes(Petal.Length,color = 'green'))+
  geom_histogram(bins = 12, color = 'green')+
  labs(x="Petal Length",
       y="Frequency",
       title = "Histogram of Petal Length")

print(plot)

hist(iris$Petal.Length,
     xlab = "Petal Length",
     col = "red",
     main = "Histogram of Petal Length")
# test