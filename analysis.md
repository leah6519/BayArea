Data Project: Birds, birds, birds
================
Leah Hong

## Part 1: Critical Thinking

1.  A likely source of error/noise/uncertainty in this data set is the
    uncertainty in the max_individuals measurement. A participant may
    have a difficult time distinguishing between bird species if they
    are visually similar to one another and misidentify a bird as
    another species. Another likely source of error in this data set is
    if people are observing and recording information about birds at
    different times of the day. For instance, certain species may be
    more prone to coming out early in the day as opposed to later in the
    afternoon.

2.  I think FeederWatch asks for the maximum number of individuals seen
    at one time (flock size) instead of individual bird reports because
    it is too difficult to keep track of birds and distinguish them from
    one another. The FeederWatch would also not ask for the total number
    of birds of each species that visits the feeder throughout the day
    because the same bird could come back and be over-counted.

3.  With a larger number of citizen scientists in the Bay area
    contributing to the FeederWatch project, the estimates should be
    less biased and more accurate.

## Part 2: Working with the data

1.  In 2011, there are 73 unique feeder locations provided. In 2021,
    there are 119 unique feeder locations provided. In both 2011 and
    2021, there are 18 unique feeder locations provided.

2.  The five species that visited the feeders in the largest flocks in
    2011 are Cedar Waxwing, Wild Turkey, Lawrence’s Goldfinch, Spinus
    sp. (goldfinch sp.), and Red-winged Blackbird. The five species that
    visited the feeders in the largest flocks in 2011 are Lesser
    Goldfinch, Pine Siskin, Wild Turkey, Rock Pigeon (Feral Pigeon), and
    Cedar Waxwing. All of the species have changed except Cedar Waxwing
    and Wild Turkey. For the species that remained in the list in 2021,
    the sizes have changed. For the Cedar Waxwing, the average flock
    size has gone from 13.78 to 14.65. For the Wild Turkey, the average
    flock size has gone from 13.96 to 7.32.

![](analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

3.  Looking at the graph above, we can see that over time, the
    proportions tend to decrease over time except 2016 to 2017 and 2020
    to 2021 where there are slight increases. Overall though, the
    proportions do tend to decrease over time.

## Part 3: EDA

<div class="figure" style="text-align: center">

<img src="analysis_files/figure-gfm/example-plot-1.png" alt="A scatterplot showing the areas where the flock Dark eyed Juncoes (Juncos?) were spotted in 2010 vs 2011" width="70%" />
<p class="caption">
A scatterplot showing the areas where the flock Dark eyed Juncoes
(Juncos?) were spotted in 2010 vs 2011
</p>

</div>

The graph I chose to portray is the latitude vs longitude of the bird
species Dark-eyed Juncos in the years 2010 and 2021. Since 2010 is the
earliest year in the data set and 2021 is the latest year in the data
set, I wanted to see if there was a large change in where the Dark-eyed
Juncos were spotted. For instance, I wanted to see if there were new
locations where Juncos were spotted or areas that do not have spotted
Juncos anymore.

Looking at the graph, I see that there seems to be a couple of new spots
where Juncos are located. One particular area that stood out to me was
at (-122.45, 37.75). In the year 2010, there were so many more spotted
observations of Juncos compared to 2021. This could be due to the fact
that Juncos did not like the area they were living in and decided to
populate elsewhere. This could also be due to the fact that there are
less Junco birds now than eleven years ago.

Another point is that the locations of volunteers could be different in
2010 vs 2021. There could be new people who are contributing to the
dataset, which could account for the new sightings of Junco in a certain
area.

Nonetheless, looking at the graph, it seems as though the Dark-eyed
Juncos relatively stayed in the same area over the years.

## Part 4: Parameter estimation

1.  The population that this data is designed to capture is bay area
    birds between November and April.

2.  I think the data is a biased representation of the population I
    identified in the previous question because the data does not
    involve data for the months May to September. Since the data is not
    representative of all birds in the Bay Area since some species may
    travel to the Bay Area during the months not included, the data is
    biased. In addition, the volunteers are observing and counting birds
    in flocks when they prefer to, which does not allow for all birds to
    have an equal probability of being observed.

3.  An estimate of the average flock size of Dark-eyed Juncos feeding at
    bird feeders in 2021 in the Bay Area based on the sample mean is
    about 3.12.

4.  Plot a histogram of the distribution of Dark-eyed Junco flock size
    in 2021.

<!-- -->

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

5.  The derivation of the MLE is written separately and is the same as
    Lab 2. I found that the MLE is equivalent to the sample mean of the
    n observations in the sample. In other words, the MLE estimate to
    estimate the parameter lambda is equivalent to the sample mean of
    3.116519 computed in a previous question.

<!-- -->

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Based on my plot, I think the Poisson assumption is reasonable since it
is equal to the sample mean.

6.  

``` r
# CODE FOR PART 4.6

lambda <- 3.116519 
dark_eyed_juncos_2021 <- birds %>% filter(species_name == "Dark-eyed Junco", year == 2021) 
num_samples <- nrow(dark_eyed_juncos_2021) 

p_boot_mean_df <- map_df(1:num_samples, function(i) {
  # sample from the approximated distribution
  bootstrap_data <- rpois(num_samples, lambda)
  # compute the sample mean of the parametric bootstrap sample
  data.frame(boot_mean = mean(bootstrap_data))
})
```

``` r
# CODE FOR PART 4.6

# an estimate of the bias of the average flock size sample mean estimate
p_bias <- mean(p_boot_mean_df$boot_mean) - mean(dark_eyed_juncos_2021$max_individuals)
p_bias
```

    ## [1] 0.001155141

``` r
# CODE FOR PART 4.6

# an estimate of the variance of the average flock size sample mean estimate
p_var <- var(p_boot_mean_df$boot_mean)
p_var
```

    ## [1] 0.004301186

``` r
# CODE FOR PART 4.6

# a histogram of the parametric bootstrapped sample means 
p_boot_mean_df %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean), color = "white", fill = "darkgray",
                 binwidth = 0.05) +
  # line for the sample estimate of the mean
  geom_vline(xintercept = mean(dark_eyed_juncos_2021$max_individuals), 
             color = "orange", size = 2) +
  # line for the bootstrapped estimate of the mean
  geom_vline(xintercept = mean(p_boot_mean_df$boot_mean), 
             color = "cornflowerblue", size = 2) +
  labs(x = "bootstrapped sample mean",
       title = "Histogram of Parametric Bootstrap Estimates of Dark-eyed Junco flock size in 2021") +
  theme_classic()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

7.  

``` r
# CODE FOR PART 4.7

N <- 1000
dark_eyed_juncos_2021 <- birds %>% filter(species_name == "Dark-eyed Junco", year == 2021) 

# draw some non-parametric bootstrap samples
np_boot_mean_df <- map_df(1:N, function(i) {
  # sample from the data with replacement
  bootstrap_data <- sample(dark_eyed_juncos_2021$max_individuals, length(dark_eyed_juncos_2021$max_individuals), replace = TRUE)
  # compute the sample mean of the bootstrap sample
  data.frame(boot_mean = mean(bootstrap_data))
})
```

``` r
# CODE FOR PART 4.7

# non-parametric bootstrap estimate of the sample mean bias 
np_bias <- mean(np_boot_mean_df$boot_mean) - mean(dark_eyed_juncos_2021$max_individuals)
np_bias
```

    ## [1] 0.005004425

``` r
# CODE FOR PART 4.7

# non-parametric bootstrap estimate of the sample mean SD
np_var <- var(np_boot_mean_df$boot_mean)
np_var
```

    ## [1] 0.01626811

``` r
# CODE FOR PART 4.7

# a histogram of the bootstrapped sample means
np_boot_mean_df %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean), color = "white", fill = "darkgray",
                 binwidth = 0.05) +
  # line for the sample estimate of the mean
  geom_vline(xintercept = mean(dark_eyed_juncos_2021$max_individuals), 
             color = "orange", size = 2) +
  # line for the bootstrapped estimate of the mean
  geom_vline(xintercept = mean(np_boot_mean_df$boot_mean), 
             color = "cornflowerblue", size = 2) +
  labs(x = "bootstrapped sample mean",
       title = "Histogram of Non-Parametric Bootstrap Estimates of Dark-eyed Junco flock size in 2021") +
  theme_classic()
```

![](analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Part 5: Become a citizen scientist!

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["type"],"name":[1],"type":["chr"],"align":["left"]},{"label":["number_of_birds"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["time_of_day"],"name":[3],"type":["chr"],"align":["left"]},{"label":["description"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"(Great Horned?) Owl","2":"1","3":"8:00pm","4":"Resting on a tree branch"},{"1":"American Crow","2":"5","3":"12:00pm","4":"Sitting on top of a house"},{"1":"Rock Pigeon","2":"2","3":"3:00pm","4":"Strolling around near Unit 3"},{"1":"Anna's Hummingbird","2":"1","3":"10:00am","4":"Flying around the lawn area near my apartment"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>
