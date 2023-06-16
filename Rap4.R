library(ggplot2)

c(runif(1,a,b), runif(1,c,d))

fun1 <- function(x){
  return(6 - (20 * x^2)/(9*pi^2))
}

fun2 <- function(x){
  return(cos(x) + cos(2 * x) + 2)
}

x1 <- seq(-5, 5, length.out=5000)
y1 <- fun1(x1)

y2 <- fun2(x1)

plot(x1, y1, type = "l")
lines(x1, y2)

a <- -5
b <- 5
c <- 0
d <- 6

points_x <- numeric(0)
points_y <- numeric(0)
in_or_out <- c()

for (i in 1:5000){
  point <- c(runif(1,a,b), runif(1,c,d))
  points_x[i] <- point[1]
  points_y[i] <- point[2]
  if (point[2] >= fun2(point[1]) & point[2] <= fun1(point[1])){
    in_or_out[i] <- 'blue'
  } else {
    in_or_out[i] <- 'grey'
  }
}

data_plot <- data.frame(x1, y1, y2, points_x, points_y, in_or_out)

ggplot(data = data_plot) +
  geom_point(aes(x = points_x, y = points_y, color=in_or_out)) +
  geom_line(aes(x = x1, y = y1), size = ) +
  geom_line(aes(x = x1, y = y2)) +
  labs(title = "Losowanie 5000 punktów",
       x = "",
       y = "")








