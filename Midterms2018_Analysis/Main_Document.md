2018 Midterms Analysis
================

# Data and Library Management

## Libraries and Parameters

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.1       ✔ purrr   0.3.2  
    ## ✔ tibble  2.1.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
library(maps)
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(rnaturalearth)
library(USAboundaries)

context_file <- "~/CS109/election-context-2018.csv"

georgia_file <- "~/CS109/Georgia.xlsx"

florida_file <- "~/CS109/Florida.xlsx"

ohio_file <- "~/CS109/Ohio.xlsx"

georgia_population <- 10099320L

num_counties <- 159L
```

## Loading files

``` r
context <- read_csv(context_file)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   state = col_character(),
    ##   county = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
georgia <- read_xlsx(georgia_file, sheet = 3, skip = 2)
florida <- read_xlsx(florida_file)
ohio <- read_excel(ohio_file, sheet = 4, skip = 4, col_names = FALSE)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * … and 48 more problems

# Wrangling Data

## Modified Data Set: Georgia-specific details

``` r
georgia_fixed <-
  georgia %>% 
  rename(county = County, total_2018 = Total) %>% 
  left_join(context, by = "county") %>%
  filter(state == "Georgia") %>% 
  mutate(
    county = str_to_lower(county),
    vote_pct_2018 = total_2018 / total_population,
    abrams_pct = Abrams_Votes / total_2018,
    kemp_pct = Kemp_Votes / total_2018,
    total_2016 = trump16 + clinton16 + otherpres16,
    total_2012 = obama12 + romney12 + otherpres12,
    clinton_pct = clinton16 / total_2016,
    trump_pct = trump16 / total_2016,
    obama_pct = obama12 / total_2012, 
    romney_pct = romney12 / total_2012,
    abrams_gain = abrams_pct - clinton_pct,
    vote_pct_2016 = total_2016 / total_population, 
    vote_pct_2012 = total_2012 / total_population, 
    avg_dem_pct = (clinton_pct + obama_pct + abrams_pct) / 3,
    sd_dem_pct = sqrt(((clinton_pct - avg_dem_pct)^2 + (obama_pct - avg_dem_pct)^2 + (abrams_pct - avg_dem_pct)^2) / 3),
    avg_rep_pct = (kemp_pct + trump_pct + romney_pct) / 3,
    sd_rep_pct = sqrt(((kemp_pct - avg_rep_pct)^2 + (trump_pct - avg_rep_pct)^2 + (romney_pct - avg_rep_pct)^2) / 3)
  ) 
```

# Central Model:

## Create a normal model for actual percentage of democratic voters and republican voters

``` r
## Here, I use a normal distribution to model the uncertainty about the actual percentage of voters that are democratic in each county. The democratic percentage itself becomes the random variable, distributed, with dem_pct ~ Norm(abrams_pct, sd(the democratic percentages of previous 3 races)). I choose to use the abrams as the mean (rather than the mean of the previous 3 races) because her work in increasing voter turnout revealed that there were many democratic voters staying home - meaning that the true mean is likely closer to her percentage than that of obama or clinton. I decided to use the standard deviation of the 3 previous races because I thought it was a good metric of the actual county variance. If a county shifted to be a LOT more democratic in the past few years, it suggests new voter registration efforts or demographic changes that are likely to be consistent moving forward. 
binomial_pcts <-
  georgia_fixed %>%
  dplyr::select(county, starts_with("vote_pct"), ends_with("pct"), total_population) %>% 
  mutate(
    estimated_dem_pct = rnorm(n = num_counties, mean = abrams_pct, sd = sd_dem_pct),
    estimated_rep_pct = rnorm(n = num_counties, mean = kemp_pct, sd = sd_rep_pct)
    # equal_props_2016_dem = clinton_pct * total_population, 
    # equal_props_2016_rep = total_population - equal_props_2016_dem,
    # equal_props_2012_dem = obama_pct * total_population, 
    # equal_props_2012_rep = total_population - equal_props_2012_dem,
  )
```

## Apply a binomial to each row to estimate the actual number of republican and democrats that will vote in the next election.

``` r
#This siumulates making an estimate of the actual numbers of democrats and republicans that will actually vote in an election.

binomial_voting <-
  binomial_pcts %>% 
  mutate(
    estimated_dem_votes = rbinom(num_counties, total_population, prob = estimated_dem_pct),
    estimated_rep_votes = rbinom(num_counties, total_population, prob = estimated_rep_pct)
    # equal_props_2016_dem = clinton_pct * total_population, 
    # equal_props_2016_rep = total_population - equal_props_2016_dem,
    # equal_props_2012_dem = obama_pct * total_population, 
    # equal_props_2012_rep = total_population - equal_props_2012_dem,
  )


## this sums the estimated number of dem and rep votes for each county to make an estimatte of the total dem and rep votes in georgia.

binom_count <-
  binomial_voting %>% 
  summarize(
    estimated_dem_votes_total = sum(estimated_dem_votes), 
    estimated_rep_votes_total = sum(estimated_rep_votes),
    # equal_props_2016_dem_total = sum(equal_props_2016_dem), 
    # equal_props_2016_rep_total = sum(equal_props_2016_rep),
    # equal_props_2012_dem_total = sum(equal_props_2012_dem), 
    # equal_props_2012_rep_total = sum(equal_props_2012_rep)
  )

binom_count
```

    ## # A tibble: 1 x 2
    ##   estimated_dem_votes_total estimated_rep_votes_total
    ##                       <int>                     <int>
    ## 1                   4983163                   5139954

## Apply a function to estimate the average estimated total\_dem and total\_rep votes.

## Alter the initial dem/rep percentages according to different voter interventions designed in different ways (eg generic “get out the vote”, targeting increase in specific counties/communities)

``` r
## for this example, let's assume that we specifically target the five highest prop-black counties in the state, conduct extensive  

high_blk_cutoff <- .6 # around the top 7 countiees with highest black population
est_dem_increase <- .01

altered_county_pcts <-
  georgia_fixed %>% 
  mutate(high_blk_pct = if_else(black_pct > high_blk_cutoff, TRUE, FALSE)) %>% 
  mutate(
    adjusted_dem_pct = if_else(high_blk_pct, avg_dem_pct + .01, avg_dem_pct),
    adjusted_rep_pct = if_else(high_blk_pct, avg_rep_pct - .01, avg_rep_pct)
  )
```

# Other General Data Analysis

## County Population Proportion

``` r
georgia_key_props <-
  georgia_fixed %>% 
  select(county, starts_with("vote_pct"), ends_with("pct"), total_population) %>% 
  mutate(georgia_pop = sum(total_population)) %>% 
  group_by(county) %>% 
  summarise(
    georgia_pop_pct = total_population / georgia_pop,
    abrams_pct, 
    clinton_pct, 
    obama_pct
  )
```

## Summary Details of Data

``` r
georgia_summary <-
  georgia_fixed %>% 
  summarise(
    total_2018 = sum(total_2018),
    total_2016 = sum(total_2016),
    total_2012 = sum(total_2012),
    total_population = sum(total_population)
  )

georgia_summary
```

    ## # A tibble: 1 x 4
    ##   total_2018 total_2016 total_2012 total_population
    ##        <dbl>      <dbl>      <dbl>            <dbl>
    ## 1    3903968    4114711    3900050         10099320

``` r
georgia_fixed %>% 
  select(county, starts_with("vote_pct"), ends_with("pct"), total_population) %>% 
  mutate(
    equal_props_2018_dem = abrams_pct * total_population,
    equal_props_2018_rep = total_population - equal_props_2018_dem,
    equal_props_2016_dem = clinton_pct * total_population, 
    equal_props_2016_rep = total_population - equal_props_2016_dem,
    equal_props_2012_dem = obama_pct * total_population, 
    equal_props_2012_rep = total_population - equal_props_2012_dem,
  ) %>% 
  summarize(
    equal_props_2018_dem_total = sum(equal_props_2018_dem), 
    equal_props_2018_rep_total = sum(equal_props_2018_rep),
    equal_props_2016_dem_total = sum(equal_props_2016_dem), 
    equal_props_2016_rep_total = sum(equal_props_2016_rep),
    equal_props_2012_dem_total = sum(equal_props_2012_dem), 
    equal_props_2012_rep_total = sum(equal_props_2012_rep)
  )
```

    ## # A tibble: 1 x 6
    ##   equal_props_201… equal_props_201… equal_props_201… equal_props_201…
    ##              <dbl>            <dbl>            <dbl>            <dbl>
    ## 1         4889704.         5209616.         4610526.         5488794.
    ## # … with 2 more variables: equal_props_2012_dem_total <dbl>,
    ## #   equal_props_2012_rep_total <dbl>

## County Data Management

``` r
#world <- ne_countries(scale = "medium", returnclass = "sf")

states <- us_states(resolution = "high", states = "georgia") %>%
  st_transform(crs = 4326)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- 
  subset(counties, grepl("georgia", counties$ID)) %>% 
  separate(ID, into = c("state_name", "county"), sep = ",")
counties$area <- as.numeric(st_area(counties))
```

## Plotting Datasets

``` r
georgia_plots <-
  georgia_fixed %>% 
  left_join(counties, by = "county")
```

# Visualizations

## Constants for Visualizations

``` r
NYT_DEM <- "#1A80C4"
NYT_REP <- "#CC3D41" 
```

## Abrams Relative Increase in Percentage of Vote by County

``` r
ggplot(data = states) +
  geom_sf() +
  geom_sf(data = georgia_plots, color = gray(.75), aes(fill = abrams_gain)) +
  coord_sf(xlim = c(-88, -78), ylim = c(30, 36), expand = FALSE) + 
  theme_light() + 
  scale_fill_gradient2(low = "orange", high = "blue", mid = "white", midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  labs(title = "Abrams relative gain over Clinton in proportion of votes won")
```

![](Main_Document_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Clinton Percentage of Vote by County

``` r
ggplot(data = states) +
  geom_sf() +
  geom_sf(data = georgia_plots, color = gray(.75), aes(fill = clinton_pct)) +
  coord_sf(xlim = c(-88, -78), ylim = c(30, 36), expand = FALSE) + 
  theme_light() + 
  scale_fill_gradient2(low = NYT_REP, high = NYT_DEM, mid = "white", midpoint = .5, space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  labs(title = "Clinton Percentage")
```

![](Main_Document_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## 

``` r
ggplot(data = states) +
  geom_sf() +
  geom_sf(data = georgia_plots, color = gray(.75), aes(fill = vote_pct_2018 - vote_pct_2016)) +
  coord_sf(xlim = c(-88, -78), ylim = c(30, 36), expand = FALSE) + 
  theme_light() + 
  scale_fill_gradient2(low = "orange", high = "blue", mid = "white", midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  labs(title = "Voter Turnout 2018")
```

![](Main_Document_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
