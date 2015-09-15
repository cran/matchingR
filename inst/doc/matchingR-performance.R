## ---- results = "hide", echo=FALSE, message = FALSE----------------------
library(matchingR)

## ------------------------------------------------------------------------
# number of men and women, respectively
n = 100
# level of commonality
lambda = 0.5
# men's preferences
uM = lambda * matrix(runif(n), nrow = n, ncol = n) +
    (1 - lambda) * runif(n ^ 2)

## ------------------------------------------------------------------------
# womens's preferences
uW = lambda * matrix(runif(n), nrow = n, ncol = n) +
    (1 - lambda) * runif(n ^ 2)

## ------------------------------------------------------------------------
# market sizes
N = c(100, 500, 1000, 2500, 5000)

## ------------------------------------------------------------------------
# levels of commonality
Commonality = c(0.0, 0.25, 0.5, 0.75, 0.99)

## ------------------------------------------------------------------------
set.seed(1)

test_one2one = function(n, lambda) {
    uM = lambda * matrix(runif(n), nrow = n, ncol = n) +
        (1 - lambda) * runif(n ^ 2)
    uW = lambda * matrix(runif(n), nrow = n, ncol = n) +
        (1 - lambda) * runif(n ^ 2)
    one2one(uM, uW)
}

## ---- echo=FALSE---------------------------------------------------------
res1 = microbenchmark::microbenchmark(
    test_one2one(N[1], Commonality[1]),
    test_one2one(N[2], Commonality[1]),
    test_one2one(N[3], Commonality[1]),
    test_one2one(N[4], Commonality[1]),
    test_one2one(N[5], Commonality[1]), times = 1, unit = "s")

res2 = microbenchmark::microbenchmark(
    test_one2one(N[1], Commonality[2]),
    test_one2one(N[2], Commonality[2]),
    test_one2one(N[3], Commonality[2]),
    test_one2one(N[4], Commonality[2]),
    test_one2one(N[5], Commonality[2]), times = 1, unit = "s")

res3 = microbenchmark::microbenchmark(
    test_one2one(N[1], Commonality[3]),
    test_one2one(N[2], Commonality[3]),
    test_one2one(N[3], Commonality[3]),
    test_one2one(N[4], Commonality[3]),
    test_one2one(N[5], Commonality[3]), times = 1, unit = "s")

res4 = microbenchmark::microbenchmark(
    test_one2one(N[1], Commonality[4]),
    test_one2one(N[2], Commonality[4]),
    test_one2one(N[3], Commonality[4]),
    test_one2one(N[4], Commonality[4]),
    test_one2one(N[5], Commonality[4]), times = 1, unit = "s")

res5 = microbenchmark::microbenchmark(
    test_one2one(N[1], Commonality[5]),
    test_one2one(N[2], Commonality[5]),
    test_one2one(N[3], Commonality[5]),
    test_one2one(N[4], Commonality[5]),
    test_one2one(N[5], Commonality[5]), times = 1, unit = "s")

## ---- echo=FALSE---------------------------------------------------------
table = matrix(c(summary(res1, unit = 's')$mean,
                 summary(res2, unit = 's')$mean,
                 summary(res3, unit = 's')$mean,
                 summary(res4, unit = 's')$mean,
                 summary(res5, unit = 's')$mean), ncol = length(Commonality), 
               dimnames = list("N" = gsub(" ", "", sprintf("N=%s", format(N, big.mark = ","))), 
                               "Commonality" = sprintf("lambda=%.2f" , Commonality)))
knitr::kable(table, digits = 6, caption = "Run time (in seconds) for the matching of men to women")

## ------------------------------------------------------------------------
u = matrix(rnorm(16), nrow = 4)
results = onesided(utils = u)

## ------------------------------------------------------------------------
test_roommate = function(n) {
    onesided(utils = matrix(rnorm( 4 * n ^ 2 ), nrow = 2 * n))
}

## ---- echo=FALSE---------------------------------------------------------
N = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 
      11, 12, 13, 14, 15, 16, 17, 
      18, 19, 20, 21, 22, 23, 24, 
      25, 30, 35, 40, 45)

res_irving = microbenchmark::microbenchmark(
    test_roommate(N[1]),
    test_roommate(N[2]),
    test_roommate(N[3]),
    test_roommate(N[4]),
    test_roommate(N[5]),
    test_roommate(N[6]),
    test_roommate(N[7]),
    test_roommate(N[8]),
    test_roommate(N[9]),
    test_roommate(N[10]),
    test_roommate(N[11]),
    test_roommate(N[12]),
    test_roommate(N[13]),
    test_roommate(N[14]),
    test_roommate(N[15]),
    test_roommate(N[16]),
    test_roommate(N[17]),
    test_roommate(N[18]),
    test_roommate(N[19]),
    test_roommate(N[20]),
    test_roommate(N[21]),
    test_roommate(N[22]),
    test_roommate(N[23]),
    test_roommate(N[24]),
    test_roommate(N[25]),
    test_roommate(N[26]),
    test_roommate(N[27]),
    test_roommate(N[28]), times = 1, unit = "s")

solution_exists = rep(0, 28);
for (n in 1:28) {
    for (i in 1:100) {
        results = onesided(utils = matrix(rnorm( 4 * N[n] ^ 2 ), nrow = 2 * N[n]))
        if (!is.null(results)) {
            solution_exists[n] = solution_exists[n] + 1;
        }
    }
}

irving_original_prop = c(0.961, 0.931, 0.909, 0.868, 
                         0.871, 0.867, 0.857, 0.830, 
                         0.815, 0.808, 0.816, 0.788, 
                         0.750, 0.766, 0.755, 0.770, 
                         0.740, 0.725, 0.745, 0.775, 
                         0.740, 0.740, 0.730, 0.710, 
                         0.725, 0.670, 0.675, 0.690)

## ---- echo=FALSE---------------------------------------------------------
table = matrix(c(rep(100, 28), solution_exists, solution_exists/100, irving_original_prop, summary(res_irving)$mean),  
               ncol = 5, nrow = 28, 
               dimnames = list("N" = gsub(" ", "", sprintf("N = %s", format(N, big.mark = ","))), 
                               "t" = c("No. of instances", 
                                       "No. with solution", 
                                       "Proportion with solution", 
                                       "Proportion with solution (Irving 1985)",
                                       "Average cpu time")))
knitr::kable(table, digits = 6, caption = "Proportion of matchings which exist and run time (in seconds) for the stable roommate algorithm.")

## ------------------------------------------------------------------------
u = matrix(rnorm(16), nrow = 4)
results = toptrading(utils = u)

## ------------------------------------------------------------------------
test_toptrading = function(n) {
    toptrading(utils = matrix(rnorm( n ^ 2 ), nrow = n))
}

## ---- echo=FALSE---------------------------------------------------------
N = c(2, 4, 8, 16, 32, 64, 128, 256, 512, 
      1024, 2048, 4096, 4096*2, 4096*3)

res_irving = microbenchmark::microbenchmark(
    test_toptrading(N[1]),
    test_toptrading(N[2]),
    test_toptrading(N[3]),
    test_toptrading(N[4]),
    test_toptrading(N[5]),
    test_toptrading(N[6]),
    test_toptrading(N[7]),
    test_toptrading(N[8]),
    test_toptrading(N[9]),
    test_toptrading(N[10]),
    test_toptrading(N[11]),
    test_toptrading(N[12]),
    test_toptrading(N[13]),
    test_toptrading(N[14]), times = 1, unit = "s")

## ---- echo=FALSE---------------------------------------------------------
table = matrix(c(rep(1, 14), summary(res_irving)$mean),  
               ncol = 2, nrow = 14, 
               dimnames = list("N" = gsub(" ", "", sprintf("N = %s", format(N, big.mark = ","))), 
                               "t" = c("No. of instances", 
                                       "Average cpu time")))
knitr::kable(table, digits = 6, caption = "Run time (in s) for top trading cycle algorithm")

