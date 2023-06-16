n <- 10
coupons <- 1:n
aquired_coupons <- numeric(n)
prize <- sum(1:n)
a <- 0

while (sum(aquired_coupons) != prize){
  coupon <- sample(1:n, 1)
  aquired_coupons[coupon] = coupon
  a <- a + 1
}
a  

1/(1:10)

n <- 109

n * sum(1/(1:n))

n_values <- 1:1000
experimental_boxes_number <- numeric(1000)
for (i in 1:1000) {
  n <- n_values[i]
  experimental_boxes_number[i] <- boxes(n)
}

n_values * log(n_values)

sum(dbinom(c(3, 4), 4, 0.5))
pbinom(2.5, 4, 0.5, lower.tail = FALSE)

pbinom(74, 100, 0.5, lower.tail = FALSE)
pbinom(0.75 * 50 - 1, 50, 0.5, lower.tail = FALSE)
pbinom(74, 100, 0.5, lower.tail = FALSE)

a <- seq(0, 5, 0.01)
pnorm(a, mean = 2, sd = 1, lower.tail = FALSE)  
2/a

n <- 2:1000
markow <- rep(0.5, 999)
czebyszew <- pi^2/(6*(log(n)))


plot(n, markow, ylim = c(0,1.1), type = "l", lwd = 2,
     xlab = " ",
     ylab = " ",
     main = "Porównanie oszacowania, z wartością dokładną") 
lines(n, czebyszew, type = "l", lwd = 2, col = "red")
legend(3, 1, legend = c("Wartość dokładna", "Czebyszew"), 
       col = c("black", "red"),
       lty = 1,
       cex = 1)

round(rnorm(200, mean = 178, sd = 5))

men <- round(rnorm(200, mean = 178, sd = 5))
mean_est <- mean(men)
median_est <- median(men)
sd_est <- sd(men) 

n <- 0
for (i in 465:900){
  n <- n + choose(900, i)
}

n/2^900

pnorm(2.1, lower.tail = FALSE)  
