Assignment 3
================

``` r
library(gapminder)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.0.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.6
    ## √ tidyr   0.8.1     √ stringr 1.3.1
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Tasks
=====

1. Get the maximum and minimum of GDP per capita for all continents.
--------------------------------------------------------------------

``` r
minGDP <-gapminder %>% 
  group_by(continent,country) %>% 
  summarize(min=min(gdpPercap)) %>% 
  summarize(min=min(min)) 
knitr::kable(minGDP)
```

| continent |         min|
|:----------|-----------:|
| Africa    |    241.1659|
| Americas  |   1201.6372|
| Asia      |    331.0000|
| Europe    |    973.5332|
| Oceania   |  10039.5956|

``` r
maxGDP <-gapminder %>% 
  group_by(continent,country) %>% 
  summarize(max=max(gdpPercap)) %>% 
  summarize(max=max(max)) 
knitr::kable(maxGDP)
```

| continent |        max|
|:----------|----------:|
| Africa    |   21951.21|
| Americas  |   42951.65|
| Asia      |  113523.13|
| Europe    |   49357.19|
| Oceania   |   34435.37|

``` r
  ## plot the minimum of GDP per capital for all continents
  ggplot(minGDP,aes(continent,min))+
  geom_point(aes(size=min,color=continent))
```

![](assignment3_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
  ## plot the maximum of GDP per capital for all continents
    ggplot(maxGDP,aes(continent,max))+
  geom_point(aes(size=max,color=continent))
```

![](assignment3_files/figure-markdown_github/unnamed-chunk-2-2.png)

2. Look at the spread of GDP per capita within the continents
-------------------------------------------------------------

``` r
ggplot(gapminder, aes(gdpPercap))+
  facet_wrap(~continent, scales="free_x","free_y")+
  geom_histogram(aes(fill=continent))
```

    ## Warning: Coercing `nrow` to be an integer.

    ## Warning in sanitise_dim(nrow): Ç¿ÖÆ¸Ä±ä¹ý³ÌÖÐ²úÉúÁËNA

    ## Warning: `nrow` is missing or less than 1 and will be treated as NULL.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](assignment3_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
## we can also use combination of scatter plot and box plot to show the spread of the GDP per capita
ggplot(gapminder,aes(gdpPercap,continent))+
  geom_point(aes(colour=continent,size=gdpPercap),alpha=0.4)
```

![](assignment3_files/figure-markdown_github/unnamed-chunk-3-2.png)

3. Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population.
--------------------------------------------------------------------------------------------------------------

the following is computed to find the 25% trimmed mean which is known as interquartile mean. The interquartile mean as well as the normal mean of lifeExp for each country are found:

``` r
temp <-gapminder %>% 
  group_by(year) %>% 
  summarize(trimmedMean=mean(lifeExp, trim=0.25),Mean=mean(lifeExp))
knitr::kable(temp)
```

|      year|    trimmedMean|                              Mean|
|---------:|--------------:|---------------------------------:|
|      1952|       47.33607|                          49.05762|
|      1957|       50.27669|                          51.50740|
|      1962|       52.79151|                          53.60925|
|      1967|       55.43138|                          55.67829|
|      1972|       58.07853|                          57.64739|
|      1977|       60.46878|                          59.57016|
|      1982|       62.70044|                          61.53320|
|      1987|       64.76540|                          63.21261|
|      1992|       66.19274|                          64.16034|
|      1997|       67.25389|                          65.01468|
|      2002|       68.30965|                          65.69492|
|      2007|       69.68886|                          67.00742|
|  \# these|  results can b|  e compared using the below plot:|

``` r
ggplot(temp)+
  geom_line(aes(year,y=trimmedMean),color="red")+
  geom_line(aes(year,y=Mean),color="blue")+
  xlab("year")+
  ylab("mean")
```

![](assignment3_files/figure-markdown_github/unnamed-chunk-5-1.png)

Compute a weighted mean of lifeExp for different years, weighting by population
===============================================================================

``` r
 gapminder%>% 
  group_by(year) %>% 
 summarize(weighted.mean(lifeExp,pop)) 
```

    ## # A tibble: 12 x 2
    ##     year `weighted.mean(lifeExp, pop)`
    ##    <int>                         <dbl>
    ##  1  1952                          48.9
    ##  2  1957                          52.1
    ##  3  1962                          52.3
    ##  4  1967                          57.0
    ##  5  1972                          59.5
    ##  6  1977                          61.2
    ##  7  1982                          62.9
    ##  8  1987                          64.4
    ##  9  1992                          65.6
    ## 10  1997                          66.8
    ## 11  2002                          67.8
    ## 12  2007                          68.9
