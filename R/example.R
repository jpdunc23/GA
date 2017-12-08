#### example
set.seed(1)
n <- 500
C <- 40
X <- matrix(rnorm(n * C), nrow = n)
beta <- c(88, 0.1, 123, 4563, 1.23, 20)
y <- X[ ,1:6] %*% beta
colnames(X) <- c(paste("real", 1:6, sep = ""),
                 paste("noi", 1:34, sep = ""))
system.time(
     o1 <- select(X, y, nsplits = 3, max_iter = 200)
)
# o1
# system.time(
#     o2 <- select(X, y, selection = "proportional", n_splits = 3)
# )
# o2
