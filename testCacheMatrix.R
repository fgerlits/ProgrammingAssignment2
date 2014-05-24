library(testthat)

setwd(dirname(sys.frame(1)$ofile))
source("cachematrix.R")

test_that("the inverse of an identity matrix is itself", {
        identity.matrix <- function(size) { diag(size) }
        
        test.sizes = sample(1:100, 10)
        for (size in test.sizes) {
                i = identity.matrix(size)
                m <- makeCacheMatrix(i)
                s <- cacheSolve(m)
                expect_that(s, equals(i))
        }
})

random.matrix <- function(size) {
        repeat {
                m <- matrix(rnorm(size * size), size, size)
                det <- det(m)
                if (! isTRUE(all.equal(0, det)) && ! is.infinite(det)) {
                        return(m)
                }
        }
}

test_that("cacheSolve does the same as solve, for some random matrices", {
        replicate(100, {
                size <- sample(1:100, 1)
                m <- random.matrix(size)
                s <- solve(m)
                mc <- makeCacheMatrix(m)
                sc <- cacheSolve(mc)
                expect_that(sc, equals(s))
        })
})

test_that("cacheSolve returns the same value when called multiple times", {
        replicate(10, {
                size <- sample(1:100, 1)
                m <- random.matrix(size)
                mc <- makeCacheMatrix(m)
                sc <- cacheSolve(mc)
                num.reps <- sample(1:10, 1)
                replicate(num.reps, {
                        other.sc <- cacheSolve(mc)
                        expect_that(other.sc, equals(sc))
                })
        })
})

test_that("subsequent calls to cacheSolve are faster than the first one", {
        size <- 300     # you may have to adjust this if your computer
                        # is faster or slower than mine
        m <- random.matrix(size)
        mc <- makeCacheMatrix(m)
        first.time.taken <- system.time(cacheSolve(mc))["elapsed"]
        #print(first.time.taken)
        num.reps <- sample(1:3, 1)
        replicate(num.reps, {
                later.time.taken <- system.time(cacheSolve(mc))["elapsed"]
                #print(later.time.taken)
                expect_true(later.time.taken < first.time.taken)
        })
})