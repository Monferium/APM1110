Formative Assessment 4
================

### A geospatial analysis systems has four sensors supplying images. The percentage of images supplied by each sensor and the percentage of images relevant to a query are shown in the following table:

``` r
sensor <- c(1, 2, 3, 4)
supplied_images <- c(15, 20, 25, 40)
relevant_images<- c(50, 60, 80, 85)
```

``` r
data <- data.frame(sensor, supplied_images, relevant_images)
print(data)
```

    ##   sensor supplied_images relevant_images
    ## 1      1              15              50
    ## 2      2              20              60
    ## 3      3              25              80
    ## 4      4              40              85

### What is the overall percentage of relevant images?

``` r
# Solution 1

weighted_contribution <- data$supplied_images * data$relevant_images / 100

op_relevant <- sum(weighted_contribution) / sum(data$supplied_images)

cat("The overall percentage of relevant images:", op_relevant, "or", round(op_relevant* 100, 2), "%\n")
```

    ## The overall percentage of relevant images: 0.735 or 73.5 %

``` r
## The overall percentage of relevant images: 0.735 or 73.5 %
```

``` r
# Solution 2

# using the conditional probability and independent probability:
# let s = supplied_images and r = relevant_images

prob_s <- c(0.15, 0.20, 0.25, 0.40)
prob_r_given_s <- c(0.50, 0.60, 0.80, 0.85)

prob_s_and_r <- sum(prob_r_given_s * prob_s)

cat("The overall percentage of relevant images:",prob_s_and_r, "or", round(prob_s_and_r* 100, 2),"%\n")
```

    ## The overall percentage of relevant images: 0.735 or 73.5 %

``` r
## The overall percentage of relevant images: 0.735 or 73.5 %
```

## Explanation:

$$
P(S_1) = 15\% = 0.15
$$

$$
P(S_2) = 20\% = 0.20
$$

$$
P(S_3) = 
$$
