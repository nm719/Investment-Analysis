
library(readr)
training <- read_csv("training.csv", show_col_types = FALSE)
test <- read_csv("test.csv", show_col_types = FALSE)

training_return <- (training[-1,3:ncol(training)] - training[-nrow(training),3:ncol(training)]) / training[-nrow(training),3:ncol(training)]

training_return_means <- colMeans(training_return[-ncol(training_return)])
training_return_covmat <- cov(training_return[-ncol(training_return)])
training_return_stdev <- diag(training_return_covmat)^0.5
training_return_cormat <- cor(training_return[-ncol(training_return)])

plot(training_return_stdev, training_return_means, title("Expected Return vs Standard Deviation of 30 Assets"), xlab = "Standard Deviation", ylab = "Expected Return")

training_return_ea_pf_mean <- mean(training_return_means)
training_return_ea_pf_stdev <- (sum(training_return_covmat)^0.5) / 30
plot(training_return_stdev, training_return_means, title("Expected Return vs Standard Deviation of 30 Assets and \n the Equal Allocation Portfolio of 30 Stocks"), xlab = "Standard Deviation", ylab = "Expected Return")
points(training_return_ea_pf_stdev, training_return_ea_pf_mean, col="red")

ones <- replicate(30,1)
training_return_mr_pf_x <- solve(training_return_covmat) %*% ones / as.numeric(t(ones) %*% solve(training_return_covmat) %*% ones)
training_return_mr_pf_mean <- t(training_return_mr_pf_x) %*% training_return_means
training_return_mr_pf_stdev <- (t(training_return_mr_pf_x) %*% training_return_covmat %*% training_return_mr_pf_x)^0.5
plot(training_return_stdev, training_return_means, title("Expected Return vs Standard Deviation of 30 Assets, the Equal \n Allocation Portfolio and the Minimum Risk Portfolio of 30 Stocks"), xlab = "Standard Deviation", ylab = "Expected Return", xlim=c(0.015, 0.14))
points(training_return_ea_pf_stdev, training_return_ea_pf_mean, col="red")
points(training_return_mr_pf_stdev, training_return_mr_pf_mean, col="blue")

A <- t(ones) %*% solve(training_return_covmat) %*% training_return_means
B <- t(training_return_means) %*% solve(training_return_covmat) %*% training_return_means
C <- t(ones) %*% solve(training_return_covmat) %*% ones
D <- B * C - A^2
E <- 0.08
lambda1 <- (C*E-A) / D
lambda2 <- (B-A*E) / D
training_return_ef_pf_comp <- as.numeric(lambda1) * solve(training_return_covmat) %*% training_return_means + as.numeric(lambda2) * solve(training_return_covmat) %*% ones

training_return_ef_pf_stdev <- seq((1/C)^0.5, 1, by = 0.0001)
training_return_ef_pf_minE <- A/C
options(warn=-1)
y1 <- (A+sqrt(D*(C*training_return_ef_pf_stdev^2-1)))*(1/C)
y2 <- (A-sqrt(D*(C*training_return_ef_pf_stdev^2-1)))*(1/C)
options(warn=0)
plot(training_return_ef_pf_stdev, y1, xlim=c(0,0.15), ylim=c(-0.01,0.1), type="l", xlab="Expected Risk", ylab="Expected Return", cex=1)
title("Expected Return vs Risk of 30 Assets, S&P 500 Index, \n Equal Allocation Portfolio, Minimum Risk Portfolio, \n Efficient Portfolio, and Efficient Frontier of 30 Stocks")
points(training_return_ef_pf_stdev, y2, type = "l")
points(training_return_stdev, training_return_means, col="black")
training_sp500_mean <- colMeans(training_return[ncol(training_return)])
training_sp500_stdev <- cov(training_return[ncol(training_return)])^0.5
points(training_sp500_stdev, training_sp500_mean, pch=19, col="darkgreen")
points(training_return_ea_pf_stdev, training_return_ea_pf_mean, pch=19, col="red")
points(training_return_mr_pf_stdev, training_return_mr_pf_mean, pch=19, col="blue")
training_return_ep_pf_stdev <- (t(training_return_ef_pf_comp) %*% training_return_covmat %*% training_return_ef_pf_comp)^0.5
training_return_ep_pf_mean <- t(training_return_ef_pf_comp) %*% training_return_means
points(training_return_ep_pf_stdev, training_return_ep_pf_mean, pch=19, col="orange")

test_return <- (test[-1,3:ncol(test)] - test[-nrow(test),3:ncol(test)]) / test[-nrow(test),3:ncol(test)]
test_return_means <- colMeans(test_return[-ncol(test_return)])
test_return_covmat <- cov(test_return[-ncol(test_return)])
test_return_stdev <- diag(test_return_covmat)^0.5
test_return_cormat <- cor(test_return[-ncol(test_return)])
test_return_ea_pf_mean <- mean(test_return_means)
test_return_ea_pf_stdev <- (sum(test_return_covmat)^0.5) / 30
ones <- replicate(30,1)
test_return_mr_pf_x <- solve(test_return_covmat) %*% ones / as.numeric(t(ones) %*% solve(test_return_covmat) %*% ones)
test_return_mr_pf_mean <- t(test_return_mr_pf_x) %*% test_return_means
test_return_mr_pf_stdev <- (t(test_return_mr_pf_x) %*% test_return_covmat %*% test_return_mr_pf_x)^0.5
A <- t(ones) %*% solve(test_return_covmat) %*% test_return_means
B <- t(test_return_means) %*% solve(test_return_covmat) %*% test_return_means
C <- t(ones) %*% solve(test_return_covmat) %*% ones
D <- B * C - A^2
E <- 0.08
lambda1 <- (C*E-A) / D
lambda2 <- (B-A*E) / D
test_return_ef_pf_comp <- as.numeric(lambda1) * solve(test_return_covmat) %*% test_return_means + as.numeric(lambda2) * solve(test_return_covmat) %*% ones
test_return_ef_pf_stdev <- seq((1/C)^0.5, 1, by = 0.0001)
test_return_ef_pf_minE <- A/C
options(warn=-1)
y3 <- (A+sqrt(D*(C*test_return_ef_pf_stdev^2-1)))*(1/C)
y4 <- (A-sqrt(D*(C*test_return_ef_pf_stdev^2-1)))*(1/C)
options(warn=0)
plot(training_return_ef_pf_stdev, y1, xlim=c(0,0.15), ylim=c(-0.01,0.1), type="l", xlab="Expected Risk", ylab="Expected Return", cex=1)
title("Expected Return vs Risk of 30 Assets, S&P 500 Index, \n Equal Allocation Portfolio, Minimum Risk Portfolio, \n Efficient Portfolio, and Efficient Frontier of 30 Stocks")
points(training_return_ef_pf_stdev, y2, type = "l")
points(training_return_stdev, training_return_means, col="black")
training_sp500_mean <- colMeans(training_return[ncol(training_return)])
training_sp500_stdev <- cov(training_return[ncol(training_return)])^0.5
points(training_sp500_stdev, training_sp500_mean, pch=19, col="darkgreen")
points(training_return_ea_pf_stdev, training_return_ea_pf_mean, pch=19, col="brown4")
points(training_return_mr_pf_stdev, training_return_mr_pf_mean, pch=19, col="blue4")
training_return_ep_pf_stdev <- (t(training_return_ef_pf_comp) %*% training_return_covmat %*% training_return_ef_pf_comp)^0.5
training_return_ep_pf_mean <- t(training_return_ef_pf_comp) %*% training_return_means
points(test_return_ef_pf_stdev, y3, type = "l", col="darkgray")
points(test_return_ef_pf_stdev, y4, type = "l", col="darkgray")
points(training_return_ep_pf_stdev, training_return_ep_pf_mean, pch=19, col="darkorange2")
points(test_return_stdev, test_return_means, col="darkgray")
test_sp500_mean <- colMeans(test_return[ncol(test_return)])
test_sp500_stdev <- cov(test_return[ncol(test_return)])^0.5
points(test_sp500_stdev, test_sp500_mean, pch=19, col="darkolivegreen3")
points(test_return_ea_pf_stdev, test_return_ea_pf_mean, pch=19, col="brown3")
points(test_return_mr_pf_stdev, test_return_mr_pf_mean, pch=19, col="cornflowerblue")
test_return_ep_pf_stdev <- (t(test_return_ef_pf_comp) %*% test_return_covmat %*% test_return_ef_pf_comp)^0.5
test_return_ep_pf_mean <- t(test_return_ef_pf_comp) %*% test_return_means
points(test_return_ep_pf_stdev, test_return_ep_pf_mean, pch=19, col="orange")