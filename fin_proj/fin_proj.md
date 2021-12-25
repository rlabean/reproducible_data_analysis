Geoscience Salary Survey
================
Robert LaBean

``` r
start_time <- Sys.time()

library(tidyverse)
library(readxl)
library(rvest)
```

Accessing the Bureau of Labor Statistics most current state salary data.

``` r
url <- "https://www.bls.gov/oes/special.requests/oesm19st.zip"

temp <- tempfile()
temp2 <- tempfile()

dstart_time <- Sys.time()
download.file(url, temp)
den_time <- Sys.time()
unzip(zipfile = temp, exdir = temp2)
raw_salary_data <- read_excel(file.path(temp2, "oesm19st/state_M2019_dl.xlsx"))

unlink(c(temp, temp2))
```

Next, we’re going to scrape the Missouri Economic Research and
Information Center (MERIC) web page an use their calculation for a cost
of living index. I like this calculation because they take the cost of
living index for each city in a state and average it to make a state
cost of living index.

``` r
scrape_url <- "https://meric.mo.gov/data/cost-living-data-series"
webpage <- read_html(scrape_url)
web_tables <- html_nodes(webpage, "table")
state_list <- html_table(web_tables)[[1]]

glimpse(state_list)
```

    ## Rows: 52
    ## Columns: 9
    ## $ Rank           <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"…
    ## $ State          <chr> "Mississippi", "Kansas", "Oklahoma", "Alabama", "Georgi…
    ## $ Index          <dbl> 85.1, 86.9, 88.2, 88.6, 89.8, 90.0, 90.1, 90.3, 90.5, 9…
    ## $ Grocery        <dbl> 95.3, 90.9, 92.7, 98.0, 95.5, 95.7, 96.4, 99.0, 97.0, 9…
    ## $ Housing        <dbl> 66.4, 72.8, 75.2, 69.5, 75.1, 79.4, 78.4, 74.7, 75.3, 8…
    ## $ Utilities      <dbl> 90.4, 101.4, 97.4, 101.8, 91.0, 93.4, 90.3, 97.0, 97.0,…
    ## $ Transportation <dbl> 89.7, 97.2, 93.6, 92.3, 92.9, 89.3, 86.3, 97.1, 106.3, …
    ## $ Health         <dbl> 96.0, 100.3, 95.4, 90.9, 96.5, 91.6, 87.0, 99.3, 95.5, …
    ## $ Misc.          <dbl> 91.6, 88.3, 92.0, 95.3, 97.3, 95.3, 98.3, 94.5, 93.4, 9…

Now, I’m combining both data-sets into one. I’m also cleaning the data a
bit as we aren’t going to be needing a lot of the information from the
MERIC data-set

``` r
col_index <- filter(state_list, !grepl("District of Columbia|Puerto Rico|US", State)) %>%
  arrange(State)

salary_data <- inner_join(raw_salary_data, col_index, by = c("area_title" = "State")) %>%
  filter(occ_code == "19-2042") # This is the occupation code the BLS uses for Geoscientists

salary_data$normalized = as.numeric(salary_data$a_mean) / (salary_data$Index/100)
```

    ## Warning: NAs introduced by coercion

``` r
# Calculating the normalized 75th percentile salaries
salary_data$pct75_norm = as.numeric(salary_data$a_pct75) / (salary_data$Index/100)
```

    ## Warning: NAs introduced by coercion

``` r
# Here I'm creating a sort of 'ranking' system by 
# multiplying the normalized salary by the location
# quotient
salary_data$ranking = ((salary_data$normalized * as.numeric(salary_data$loc_quotient))) / as.numeric(max(salary_data$loc_quotient)) 
```

    ## Warning: NAs introduced by coercion

``` r
salary_data$rank2 <- salary_data$ranking / max(salary_data$ranking, na.rm = TRUE)

glimpse(col_index)
```

    ## Rows: 51
    ## Columns: 9
    ## $ Rank           <chr> "US", "4", "45", "36", "14", "49", "34", "43", "35", "3…
    ## $ State          <chr> "***", "Alabama", "Alaska", "Arizona", "Arkansas", "Cal…
    ## $ Index          <dbl> 100.0, 88.6, 125.7, 107.0, 92.1, 146.9, 105.8, 119.5, 1…
    ## $ Grocery        <dbl> 100.0, 98.0, 131.9, 104.5, 93.5, 119.2, 97.1, 109.3, 11…
    ## $ Housing        <dbl> 100.0, 69.5, 126.0, 117.8, 79.8, 221.0, 117.3, 131.2, 1…
    ## $ Utilities      <dbl> 100.0, 101.8, 156.5, 103.7, 94.2, 117.5, 89.7, 132.0, 9…
    ## $ Transportation <dbl> 100.0, 92.3, 115.1, 97.8, 93.3, 133.3, 108.3, 107.5, 10…
    ## $ Health         <dbl> 100.0, 90.9, 151.5, 98.1, 83.0, 112.4, 97.8, 109.8, 105…
    ## $ Misc.          <dbl> 100.0, 95.3, 113.8, 103.9, 101.8, 115.2, 104.9, 115.4, …

Finally we’re going to normalize all the salaries with our cost of
living index and plot it on a chart to see which states typically have
the highest salaries compared to their respective cost of living.

``` r
salary_plot2 <- ggplot(salary_data) +
  theme_light(
  )+
  theme(
    axis.text.x = element_text(angle = 90,
                                   vjust = 0,
                                   hjust = 1),
        legend.position = "right"
    ) +
  labs(
    title = "Average Geoscientist Salary",
       y ="Annual Salary (USD)",
       x = "Mean Annual Salary in Red, Normalized by Cost of
       Living Index in Black"
    ) +
  geom_col(
    mapping = aes(area_title, normalized), 
           width = 0.8,
           fill = "black", 
           color = "black"
           ) +
  geom_col(
    mapping = aes(area_title, as.numeric(a_mean)),
           width = 0.3, 
           fill = "red"
           ) +
  scale_y_continuous(
    limits = c(0, 175000), 
                     breaks = seq(0, 175000, 25000), 
                     labels = c("0" = "$0", "25000" = "$25k", "50000" = "$50k", 
                                "75000" = "$75k", "100000" = "$100k", 
                                "125000" = "$125k", "150000" = "$150k", 
                                "175000" = "$175k")
    )
```

75th Percentile Plots - I added these plots because I was curious
whether California and New York might have much higher salaries than the
mean shows, but it didn’t change much.

``` r
salary_plot3 <- ggplot(salary_data) +
  theme_light(
  )+
  theme(
    axis.text.x = element_text(angle = 90,
                                   vjust = 0,
                                   hjust = 1, 
                               size = 10
                               ),
        legend.position = "right"
    ) +
  labs(
    title = "75th Percentile Geoscientist Salary",
       y ="75th Percentile Salary (USD)",
       x = "75th Percentile Annual Salary in Red, Normalized by Cost of
       Living Index in Black"
    ) +
  geom_col(
    mapping = aes(area_title, pct75_norm), 
           width = 0.8,
           fill = "black", 
           color = "black"
           ) +
  geom_col(
    mapping = aes(area_title, as.numeric(a_pct75)),
           width = 0.3, 
           fill = "red"
           ) +
  scale_y_continuous(
    limits = c(0, 225000), 
                     breaks = seq(0, 225000, 25000), 
                     labels = c("0" = "$0", "25000" = "$25k", "50000" = "$50k", 
                                "75000" = "$75k", "100000" = "$100k", 
                                "125000" = "$125k", "150000" = "$150k", 
                                "175000" = "$175k", "200000" = "$200k", "225000" = "$225k")
    )
```

Print plots

``` r
ggsave("salary_plot2.png", width = 20, height = 10, limitsize = FALSE)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning: Removed 2 rows containing missing values (position_stack).

    ## Warning: Removed 2 rows containing missing values (position_stack).

``` r
print(salary_plot2)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

![](fin_proj_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("salary_plot3.png", width = 20, height = 10, limitsize = FALSE)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

``` r
print(salary_plot3)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

    ## Warning in FUN(X[[i]], ...): Removed 2 rows containing missing values
    ## (position_stack).

![](fin_proj_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

Here I attempted to make a sort of ‘ranking’ system, but the math
methodology could be better.

``` r
ranking_index = arrange(salary_data, rank2)

rank_plot1 <- ggplot(salary_data) +
  theme_light(
  )+
  theme(
    axis.text.x = element_text(angle = 90,
                                   vjust = 0,
                                   hjust = 1, 
                               size = 10
                               ),
        legend.position = "right"
    ) +
  labs(
    title = "Best Career Locations for Geoscientists 2019",
       y ="Rank",
       x = ""
    ) +
  geom_col(
    mapping = aes(x = reorder(area_title, -rank2), rank2),
           fill = "black", 
           color = "black",
    width = 0.8
           )
ggsave("rank_plot1.png", width = 20, height = 10, limitsize = FALSE)
```

    ## Warning: Removed 3 rows containing missing values (position_stack).

``` r
print(rank_plot1)
```

    ## Warning: Removed 3 rows containing missing values (position_stack).

![](fin_proj_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
end_time <- Sys.time()

run_time <- end_time - start_time - (den_time - dstart_time)

print(run_time)
```

    ## Time difference of 6.72233 secs
