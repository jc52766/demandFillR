demandFillR
================
Jason Corderoy
2024-11-13

## Fill that demand

A simple minimalistic library for demand filling.

Of course there are fancy solves like OR tools but most the time you
just need something simple and that can be quickly/easily tinkered to
solve for what your personal needs are.

And it’s fast!

Below is a simple example of it solving a simple demand profile. And
kinda like a maze it goes too far one way before adjusting its path the
next time around to a much better solution, less greedy, solution.

``` r
# Check 1st solve is non as good as later solves in this specific case
solved <- matrix(rep(0, 3*4), ncol = 3, byrow = T)
options <- matrix(c(2, 0, 0, 1, 1, 0, 1, 0, 1), ncol = 3, byrow = T)
demand <- c(4, 2, 2)
dfw <- c(10, 1, 1) # loves the 1st one
dew <- c(-10, -0.5, -0.5) # once 1st options demand exceeded it is heavily penalized
expect1 <- matrix(c(2, 0, 0, 2, 0, 0, 1, 1, 0, 1, 1, 0),
                 ncol = 3,
                 byrow = T)
expect2 <- matrix(c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0),
                  ncol = 3,
                  byrow = T)
for (i in 1:4) {
  solved <- xsolve(
    solved = solved,
    options = options,
    demand = demand,
    dfw = dfw,
    dew = dew
  )
}
solved1 <- solved
expect_identical(solved1, expect1)

print(solved1)
```

    ##      [,1] [,2] [,3]
    ## [1,]    2    0    0
    ## [2,]    2    0    0
    ## [3,]    1    1    0
    ## [4,]    1    1    0

``` r
print(expect1)
```

    ##      [,1] [,2] [,3]
    ## [1,]    2    0    0
    ## [2,]    2    0    0
    ## [3,]    1    1    0
    ## [4,]    1    1    0

``` r
print(colSums(expect1))
```

    ## [1] 6 2 0

``` r
print(demand)
```

    ## [1] 4 2 2

``` r
for (i in 1:4) {
  solved <- xsolve(
    solved = solved,
    options = options,
    demand = demand,
    dfw = dfw,
    dew = dew
  )
}
solved2 <- solved
expect_identical(solved2, expect2)

print(solved2)
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    0    1
    ## [2,]    1    0    1
    ## [3,]    1    1    0
    ## [4,]    1    1    0

``` r
print(expect2)
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    0    1
    ## [2,]    1    0    1
    ## [3,]    1    1    0
    ## [4,]    1    1    0

``` r
print(colSums(expect2))
```

    ## [1] 4 2 2

``` r
print(demand)
```

    ## [1] 4 2 2
