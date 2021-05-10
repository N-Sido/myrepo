Analysis of Homelessness in Austin, Texas by Demographics and Housing
Circumstances
================
SDS348 Spring 2021

## Name: Nikita Sidorchuk

### UT EID: nas3428

------------------------------------------------------------------------

### Datasets:

#### Datasets were found on Data.gov, and investigate homelessness in Austin, Texas. The dataset ‘homeless\_count’ looks at the number of individuals experiencing sheltered homelessness in Austin/Travis County by fiscal year; the data looks at the variables race, gender, ethnicity, age category, veteran status, and disability status per person. The sheltered homelessness is divided into three categories: emergency shelter, safe haven, and transitional housing. The dataset ‘homeless\_return’ was created by the same data platform, the Homeless Management Information Group. It investigates the amount of people that exit homelessness to a permanent housing destination and return to homelessness within two years. This data is also broken up by the type of housing people exit from (emergency shelter, safe haven, etc.). Both of these datasets look at the demographics that individuals experiencing homelessness belong to. Analysis of these datasets can help to better understand which populations are most susceptible to homelessness and the disparities, whether racial, gender-related, or otherwise, that lead to people having difficulty in recovering from homelessness.

``` r
homeless_count <- read.csv("Homelessness_Count.csv")
homeless_return <- read.csv("Homelessness_Return.csv")
```

### Tidy Data:

``` r
head(homeless_count)
```

    ##   Demographic.Category                      Specific.Demographic
    ## 1                 Race         American Indian or Alaskan Native
    ## 2                 Race                                     Asian
    ## 3                 Race                 Black or African American
    ## 4                 Race Native Hawaiian or Other Pacific Islander
    ## 5                 Race                                     White
    ## 6                 Race                                   Unknown
    ##   Emergency.Shelter.Count Safe.Haven.Count Transitional.Housing.Count
    ## 1                      73                0                          1
    ## 2                      40                1                          3
    ## 3                    1830               19                        170
    ## 4                      15                1                          1
    ## 5                    2278               44                        212
    ## 6                      40                0                          0
    ##   Total.Unduplicated.Clients Fiscal.Year
    ## 1                         73        2016
    ## 2                         42        2016
    ## 3                       1981        2016
    ## 4                         16        2016
    ## 5                       2475        2016
    ## 6                         40        2016

``` r
head(homeless_return)
```

    ##   Demographic.Category                      Specific.Demographic Fiscal.Year
    ## 1                 Race         American Indian or Alaskan Native        2017
    ## 2                 Race                                     Asian        2017
    ## 3                 Race                 Black or African American        2017
    ## 4                 Race Native Hawaiian or Other Pacific Islander        2017
    ## 5                 Race                                     White        2017
    ## 6                 Race     Client Doesn't Know or Client Refused        2017
    ##   SO.ES.TH.SH.PH Numerator Denominator Rate
    ## 1             SO         0           0    0
    ## 2             SO         0           0    0
    ## 3             SO         0           0    0
    ## 4             SO         0           0    0
    ## 5             SO         0           0    0
    ## 6             SO         0           0    0

#### As can be seen, the data for ‘homeless\_count’ is already in a tidy form: each variable has its own column, every observation has its own row, and each cell has only one value. However, ‘homeless\_return’ is not yet tidy since the column ‘SO.ES.TH.SH.PH’ be five separate variables to represent five different housing situations in the client universe.

``` r
library(tidyverse)
library(dplyr)
homeless_return_tidy <- homeless_return %>%
  pivot_wider(names_from = SO.ES.TH.SH.PH, # Separate this column into independent variables
              names_sep = '.',             # Separate by period
              values_from = c(Numerator, Denominator, Rate)) #Take values from these columns
```

### Join Datasets:

``` r
homeless <- homeless_count %>%
  left_join(homeless_return_tidy, by = c("Demographic.Category", "Specific.Demographic", "Fiscal.Year"))
```

#### Left join was used to add the tidyed ‘return’ dataset onto the ‘count’ dataset, matching with the demographic category, specific demographic, and fiscal year, as both datasets share those variables in common.

### Summary Statistics:

#### Using the mutate dplyr function, new variables was created that measure the total amount of people that returned to homelessness, combining the information from the SO (Street Outreach), ES (Emergency Shelter), TH (Temporary Housing), SH (Safe Haven), and PH (Permanent Housing). Total rate, in particular, measures the rate at which people from this entire client universe of SO, ES, TH, SH, and PH return to homelessness.

``` r
homeless_summarized <- homeless %>%
  mutate(Total_Returned = Numerator.ES + Numerator.SO + 
           Numerator.TH + Numerator.SH + Numerator.PH) %>%
  mutate(Total_Exited = Denominator.SO + Denominator.ES + 
           Denominator.TH + Denominator.SH + Denominator.PH) %>%
  mutate(Total_Rate = Total_Returned/Total_Exited)
```

#### The dataset now contains now does not discriminate based on housing situation. One is able to compare between the total amount of people that are in homelessness from the ‘Total.Unduplicated.Clients’ variable from the ‘counts’ dataset, and the amount of people that exit homelessness, and return to it. Furthermore, if information regarding specifc housing situation is favored, it can be found within the dataset.

``` r
if (!require(qwraps2)) install.packages("qwraps2", repos = "http://cran.us.r-project.org")
library(qwraps2)
options(qwraps2_markup = "markdown")
demo_left = homeless_summarized %>% filter(Total_Returned == 0 & Total_Exited > 0, na.rm = TRUE) %>% count() # Count number of demographics where no one returned to homelessness and at least 1 exited
stats_summary <-
  list("Emergency Shelter" =
       list("min"       = ~ min(Emergency.Shelter.Count),
            "max"       = ~ max(Emergency.Shelter.Count),
            "mean (sd)" = ~ qwraps2::mean_sd(Emergency.Shelter.Count),
            "variance"  = ~ round(var(Emergency.Shelter.Count), 2)),  # Looks at min, max, mean +- sd, var for ES
       "Safe Haven" =
       list("min"       = ~ min(Safe.Haven.Count),
            "max"       = ~ max(Safe.Haven.Count),
            "mean (sd)" = ~ qwraps2::mean_sd(Safe.Haven.Count),
            "variance"  = ~ round(var(Safe.Haven.Count), 2)),
       "Transitional Housing" =
       list("min"       = ~ min(Transitional.Housing.Count),
            "max"       = ~ max(Transitional.Housing.Count),
            "mean (sd)" = ~ qwraps2::mean_sd(Transitional.Housing.Count),
            "variance"  = ~ round(var(Transitional.Housing.Count), 2)),
       "Total Returned" =
       list("min"       = ~ min(Total_Returned, na.rm = TRUE),
            "max"       = ~ max(Total_Returned, na.rm = TRUE),
            "mean"      = ~ round(mean(Total_Returned, na.rm = TRUE), 2),
            "count of no returns" = ~ demo_left$n,
            "variance"  = ~ round(var(Total_Returned, na.rm = TRUE), 2)),
       "Total Exited" =
       list("min"       = ~ min(Total_Exited, na.rm = TRUE),
            "max"       = ~ max(Total_Exited, na.rm = TRUE),
            "mean"      = ~ round(mean(Total_Exited, na.rm = TRUE), 2),
            "variance"  = ~ round(var(Total_Exited, na.rm = TRUE), 2)),
       "Rate Returned (Total)" =
       list("min"       = ~ min(Total_Rate, na.rm = TRUE),
            "max"       = ~ round(max(Total_Rate, na.rm = TRUE), 3),
            "mean"      = ~ round(mean(Total_Rate, na.rm = TRUE), 3),
            "standard deviation" = ~ round(sd(Total_Rate, na.rm = TRUE), 3),
            "variance"  = ~ round(var(Total_Rate, na.rm = TRUE), 3))
       )

table_1 <- summary_table(homeless_summarized, stats_summary)
table_1
```

|                           | homeless\_summarized (N = 104) |
|:--------------------------|:-------------------------------|
| **Emergency Shelter**     |                                |
|    min                    | 0                              |
|    max                    | 4011                           |
|    mean (sd)              | 965.10 ± 1,216.81              |
|    variance               | 1480633.74                     |
| **Safe Haven**            |                                |
|    min                    | 0                              |
|    max                    | 69                             |
|    mean (sd)              | 14.70 ± 23.29                  |
|    variance               | 542.21                         |
| **Transitional Housing**  |                                |
|    min                    | 0                              |
|    max                    | 297                            |
|    mean (sd)              | 77.08 ± 91.47                  |
|    variance               | 8367.53                        |
| **Total Returned**        |                                |
|    min                    | 0                              |
|    max                    | 243                            |
|    mean                   | 79.89                          |
|    count of no returns    | 5                              |
|    variance               | 5503.38                        |
| **Total Exited**          |                                |
|    min                    | 0                              |
|    max                    | 1192                           |
|    mean                   | 438.25                         |
|    variance               | 155571.19                      |
| **Rate Returned (Total)** |                                |
|    min                    | 0                              |
|    max                    | 0.356                          |
|    mean                   | 0.175                          |
|    standard deviation     | 0.082                          |
|    variance               | 0.007                          |

#### The package qwraps2 was used to create a data table that summarized the statistics of 6 of the variables in the dataset. Some notable results are that among all of the living conditions, Emergency Shelters saw the highest amount of people, with a mean of 965 people, compared to 14.70 and 77.08 in Safe Havens and Transitional Housing situations, respectively. The variation of demographic living within Emergency Shelters was also the highest, with a variance of 1480633.74, this shows there is a lot of difference between the demographics and that they are generally far from the mean of 965.

#### There are also notable statistics about the data regarding people that had returned to homelessness. For one, out of the 26 demographics in the dataset, 5 had at least one person leave homelessness and have no one return. It was also shown that, with a very small variance of 0.007, the average rate of any given demographic returning to homelessness is 0.175. The maximum for any demographic to return to homelessness was 0.356, a rather right rate. These results show that, in general, once people exit homelessness, they tend to not return to it, and the trend is consistent among most demographics.

``` r
homeless_race <- homeless_summarized %>%
  filter(Demographic.Category == "Race") %>%  #To group by the categorical variable Race, filter to only leave Race
  filter(!Specific.Demographic == "Unknown") #Get rid of Unknown Race rows since all NA values in the homeless_return dataset

homeless_race$Demographic.Category <- NULL #This column is now unnecessary

stats_summary_race <-
  list("Total Returned" =
       list("min"       = ~ min(Total_Returned, na.rm = TRUE),
            "max"       = ~ max(Total_Returned, na.rm = TRUE),
            "mean"      = ~ round(mean(Total_Returned, na.rm = TRUE), 2),
            "variance"  = ~ round(var(Total_Returned, na.rm = TRUE), 2)),
       "Total Exited" =
       list("min"       = ~ min(Total_Exited, na.rm = TRUE),
            "max"       = ~ max(Total_Exited, na.rm = TRUE),
            "mean"      = ~ round(mean(Total_Exited, na.rm = TRUE), 2),
            "variance"  = ~ round(var(Total_Exited, na.rm = TRUE), 2)),
       "Rate Returned (Total)" =
       list("min"       = ~ round(min(Total_Rate, na.rm = TRUE),3),
            "max"       = ~ round(max(Total_Rate, na.rm = TRUE), 3),
            "mean"      = ~ round(mean(Total_Rate, na.rm = TRUE), 3),
            "standard deviation" = ~ round(sd(Total_Rate, na.rm = TRUE), 3),
            "variance"  = ~ round(var(Total_Rate, na.rm = TRUE), 3))
       )

table_2 <- summary_table(dplyr::group_by(homeless_race, Specific.Demographic), stats_summary_race)
table_2
```

|                           | American Indian or Alaskan Native (N = 4) | Asian (N = 4) | Black or African American (N = 4) | Native Hawaiian or Other Pacific Islander (N = 4) | White (N = 4) |
|:--------------------------|:------------------------------------------|:--------------|:----------------------------------|:--------------------------------------------------|:--------------|
| **Total Returned**        |                                           |               |                                   |                                                   |               |
|    min                    | 1                                         | 0             | 121                               | 0                                                 | 127           |
|    max                    | 3                                         | 1             | 149                               | 1                                                 | 145           |
|    mean                   | 2                                         | 0.33          | 131                               | 0.33                                              | 133.67        |
|    variance               | 1                                         | 0.33          | 244                               | 0.33                                              | 97.33         |
| **Total Exited**          |                                           |               |                                   |                                                   |               |
|    min                    | 8                                         | 7             | 668                               | 3                                                 | 648           |
|    max                    | 13                                        | 15            | 746                               | 11                                                | 766           |
|    mean                   | 9.67                                      | 11            | 718                               | 7.33                                              | 716.33        |
|    variance               | 8.33                                      | 16            | 1884                              | 16.33                                             | 3742.33       |
| **Rate Returned (Total)** |                                           |               |                                   |                                                   |               |
|    min                    | 0.125                                     | 0             | 0.162                             | 0                                                 | 0.173         |
|    max                    | 0.25                                      | 0.091         | 0.201                             | 0.125                                             | 0.199         |
|    mean                   | 0.202                                     | 0.03          | 0.183                             | 0.042                                             | 0.187         |
|    standard deviation     | 0.067                                     | 0.052         | 0.02                              | 0.072                                             | 0.013         |
|    variance               | 0.005                                     | 0.003         | 0                                 | 0.005                                             | 0             |

#### After grouping the dataset by a specific demographic, it is possible to see the data broken up almost by individuals, since no one belongs to two specific demographics. In this case, the data was grouped by Race. From the table, it can be seen that people who identify as Asian, or Pacific Islander have significantly lower rates of return to homelessness than Black/African American, White, or American Native peoples. Another statistic of note is that while Black people represent roughly 7% of Austin, their numbers of exiting/returning to homelessness are near identical to White people, who represent roughly 70% of Austin. With both races having an average of 717 people exit homelessness per year from 2017-2019, and 132 people return to homelessness, their rates of return are near identical at 18.3% for Black people and 18.7% for White people. Since the numbers are so similar, yet the proportion representing the city so different, it shows that Black people are affected by homelessness significantly more than any other race looked at by the study in Austin, Texas.

### Visualizations:

``` r
homeless_clean <- homeless_summarized %>%
  drop_na(Total_Rate) #Get rid of all rows with a NA value in Total_Rate, coincidentally, also all the NAs in data set

#Filter by race and age to make a ggplot
homeless_plot1 <- homeless_clean %>%
  filter(Demographic.Category == "Race" | Demographic.Category == "Age Category")
ggplot(data = homeless_plot1, aes(x = Fiscal.Year, y = Total_Rate, color = Specific.Demographic)) +
  geom_line(size = 1.2) +
  labs(title = "Total Rate Returned To Homelessness by Age and Race, 2017-2019",
       x = "Fiscal Year",
       y = "Total Rate",
       color = "Specific Demographic") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(size=12)) + 
  scale_color_brewer(palette = "RdYlBu") +
  theme_bw()
```

![image](https://user-images.githubusercontent.com/83439507/117615050-cfed9d00-b12e-11eb-82f5-9504d45f23d1.png)

#### This plot shows the total rate at which specific demographics return to homelessness from 2017 to 2019. The particular metrics tracked are Total Rate and Fiscal Year, with a break down across race and age category. Looking at the peaks within the graph, it can be seen that 2018 shows a peak for 7 of the 9 demographics in terms of returning to homelessness, with Asian and Native Islander people actually seeing the only time across the three years when they had anyone return to homelessness. Besides the peak, however, 7 of the 9 demographics also had a lower rate of return in 2019 than in 2017, which may be indicative of a long term decline in return to homelessness.

``` r
#Create Correlation matrix then heat map
homeless_numeric <- homeless_summarized %>%
  select_if(is.numeric) %>%
  #To reduce redundancy and clutter, remove Numerator and Denominator values
  select(!starts_with("Numerator.")) %>%
  select(!starts_with("Denominator."))
cor_matrix <- cor(homeless_numeric, use = "pairwise.complete.obs") 
cor(homeless_numeric, use = "pairwise.complete.obs") %>%
  #Save as data frame
  as.data.frame %>% 
  #Convert row names to variable
  rownames_to_column() %>%
  #Pivot
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  #Create ggplot
  ggplot(aes(rowname, other_var, fill = correlation)) +
  #Heat map
  geom_tile() +
  scale_fill_gradient2(limit = c(-1, 1), low = "dark blue", mid = "white", high = "dark red") +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 3) +
  labs(title = "Correlation Matrix Heat Map of Variables in Homelessness Data",
       x = "Variable 1",
       y = "Variable 2") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))
```

![image](https://user-images.githubusercontent.com/83439507/117615099-e0057c80-b12e-11eb-928c-702b7cf7babe.png)

#### A heat map was created based on the correlation matrix derived from the numeric variables of the dataset. An interesting feature that is immediately noticable is that very few of the variables have a negative correlation with each other, as almost the entire heat map is between the red to white hues. The only instances where a negative correlation does occur are regarding the Fiscal Year variable, though a lot of these are near-zero correlation values. However, this does imply that either many of the variables are not well correlated with the year, or that there is indeed a very slight negative correlation between homelessness in Austin and the year. Variables that tended to be highly correlated with others are ones such as Total Rate, Total Exited, Total Returned, or Total Undupliated Clients. The high correlation is probably due to a ‘Total’ variable being immediately linked to all other variables that comprise the ‘Total’ that is measured.

``` r
library(viridis)
homeless_plot2 <- homeless_clean %>%
  filter(Demographic.Category == "Age Category") %>%
  #Find mean Total Rate for each Age Category over the years
  group_by(Specific.Demographic) %>%
  mutate(avg_TR = mean(Total_Rate))

ggplot(data = homeless_plot2, 
       aes(x = Specific.Demographic, y = Total.Unduplicated.Clients)) +
  geom_col(aes(fill = avg_TR)) +
  labs(title = "Total Amount of Homeless Clients Per Age Category",
       subtitle = "Colored by the Average Return Rate from 2017-2019",
       x = "Age Category",
       y = "Total Unduplicated Clients",
       fill = "Average Total Rate") +
  scale_fill_viridis(limits = c(0, .24), option = "C") +
  theme_bw()
```

![image](https://user-images.githubusercontent.com/83439507/117615110-e693f400-b12e-11eb-8dbe-d36e49bea4bc.png)

#### This plot shows the amount of homeless clients tallied by the city, per age category, colored by the groups average rate of return to homelessness from 2017-2019. A key feature of the graph is that Adults aged 25-61 show a massive peak in the graph, at almost 10000 clients. This however, is slightly misleading, since that group covers 36 years of age which is far greater than any other age category, and is thus expected to have a greater amount of people belonging to it. An unfortunate feature regarding the age categories is that Children are the second largest group in the graph, outnumbering the elderly and matching Adults aged 18-24. Children, however, have the lowest rate of return to homelessness, by a substantial margin, compared to the other age categories. Meanwhile, the elderly have the highest rate of return to homelessness, followed by Adults aged 25-61. This graph shows that more can be done to prevent children from entering homelessness, and more assistance should be given to the elderly to help keep them out of homelessness once they exit.

### Principal Component Analysis:

``` r
#Create a dataset with no NA values and only numeric variables
homeless_pca_prep <- homeless_clean %>%
  select_if(is.numeric)

#Conduct PCA
homeless_pca <- homeless_pca_prep %>%
  scale() %>%
  prcomp()

#Find percent of variance explained
percent <- 100 * (homeless_pca$sdev^2 / sum(homeless_pca$sdev^2))
percent_data <- as.data.frame(percent)
#percent[1] + percent[2]  = 66.09256% of the variance in the model is explained by PC1 and PC2
#percent[1]               = 51.83423% of the variance in the model is explained by PC1
#percent[1] + percent[2] + percent[3] + percent[4] = 83.15311% in first 4 PCs

library(factoextra)
homeless_pca_prep %>%
  cor() %>%
  eigen() #11.92187 3.27941 2.60088 1.32304 1.05057 are eigenvalues for the first 5 PCs
```

    ## eigen() decomposition
    ## $values
    ##  [1] 1.192187e+01 3.279416e+00 2.600880e+00 1.323045e+00 1.050578e+00
    ##  [6] 7.519212e-01 5.156881e-01 4.650107e-01 3.719617e-01 2.035963e-01
    ## [11] 1.413644e-01 1.050808e-01 8.039459e-02 5.304440e-02 4.112742e-02
    ## [16] 3.793525e-02 2.338901e-02 1.736243e-02 8.361916e-03 7.954033e-03
    ## [21] 1.491887e-05 2.069572e-16 1.391201e-16
    ## 
    ## $vectors
    ##              [,1]        [,2]        [,3]        [,4]        [,5]         [,6]
    ##  [1,] -0.27223498 -0.02405838  0.10311538  0.02651565 -0.23558404  0.002380432
    ##  [2,] -0.17199321  0.35351724 -0.15238729 -0.25967881  0.01183177  0.207323256
    ##  [3,] -0.24964408 -0.10584972  0.16083581  0.10717751 -0.07303382 -0.151931002
    ##  [4,] -0.27385395 -0.02147388  0.10007646  0.02543449 -0.22159137  0.001584717
    ##               [,7]        [,8]        [,9]        [,10]       [,11]
    ##  [1,]  0.063105957 -0.03201862  0.03323012  0.046778185 -0.21411302
    ##  [2,]  0.068466822  0.23108579 -0.13720603  0.191055847  0.15737320
    ##  [3,]  0.074639329 -0.02999770 -0.04210710  0.621886994  0.28359882
    ##  [4,]  0.061998158 -0.02324246  0.02858067  0.066735505 -0.19056455
    ##              [,12]        [,13]         [,14]       [,15]        [,16]
    ##  [1,] -0.139303203  0.368633193 -0.0019741255  0.32162091  0.019011519
    ##  [2,]  0.055406920  0.144699770 -0.0715208811  0.28227733  0.168875050
    ##  [3,] -0.227757753 -0.110061614  0.4397724512 -0.27726655 -0.065391599
    ##  [4,] -0.148983166  0.362199336  0.0144160494  0.30493031  0.019377982
    ##              [,17]        [,18]        [,19]        [,20]         [,21]
    ##  [1,] -0.003014506 -0.085364181  0.193665421 -0.115602356  6.978940e-01
    ##  [2,] -0.030797916 -0.072569299 -0.361475164  0.546995389  8.829757e-03
    ##  [3,]  0.005183936 -0.202673684 -0.039226043  0.058756168  2.552795e-02
    ##  [4,]  0.002655982 -0.100323589  0.200614187 -0.098557305 -7.154191e-01
    ##               [,22]         [,23]
    ##  [1,]  0.000000e+00  2.282605e-12
    ##  [2,] -2.202027e-14  2.985939e-14
    ##  [3,] -4.074990e-15  8.394462e-14
    ##  [4,]  1.402872e-14 -2.346518e-12
    ##  [ reached getOption("max.print") -- omitted 19 rows ]

``` r
fviz_screeplot(homeless_pca) + 
  geom_text(aes(label = round(percent_data, 2)), size = 0, vjust = -0.5)
```

![image](https://user-images.githubusercontent.com/83439507/117615132-ebf13e80-b12e-11eb-8248-bb549f3114a9.png)

#### In determining how many PCs to keep for analysis, it was decided that 5 PCs should be kept. Eigenvalues are greater than 1 for the first 5 PCs (Kaiser’s Rule), and it takes at least 4 PCs for the cumulative variance explained by the model to be above 80%. Furthermore, analyzing the scree plot, variance starts to level off after about 3 PCs, and even more at about 5 PCs. However, a 5 dimensional PC plot is not possible to make, so only a plot of PC1 against PC2 will be shown below. These two PCs still explain 66.09% of the variance in the model.

``` r
#Get the PC1 and PC2 values for the observations
pca_data <- as.data.frame(homeless_pca[["x"]])
#Bind the columns Demographic Category and Specific Demographic to find a way of clustering/identifying information in the plot
DC = homeless_clean$Demographic.Category
pca_data <- cbind(pca_data, DC)
ggplot(pca_data, aes(x = PC1, y = PC2, color = DC)) + #DC showed better potential for clustering
  geom_point(size = 2) +
  labs(title = "Principal Components 1 and 2, colored by Demographic Category",
       color = "Demographic Category") +
  theme_bw()
```

![image](https://user-images.githubusercontent.com/83439507/117615142-f01d5c00-b12e-11eb-97fa-43f34f012389.png)

#### The PCA plot compares PC1 and PC2 and is colored by demographic category. The percent of variance explained by PC1 is 51.834%, so it has the greatest predictive value compared the the other principal components. Looking at the plot, it can be seen that there is some clustering consistent with the demographic categories. Race, marked in blue, has two clusters on PC1 one at around -4.25, and another from 0.25-3.75. Age Category also clusters together around -2.5 on PC1. In general, however, the scattering is fairly homogenous without much distinct clustering, implying that either PC1 and PC2 are not enough to show classifications of people, or that the variables involved in the dataset do not predict a specific “type” of person may experience homelessness.
