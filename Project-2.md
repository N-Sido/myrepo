Statistical Modeling and Classification of Star Types based on Physical
Properties
================
SDS348 Spring 2021

## Name: Nikita Sidorchuk

### UT EID: nas3428

------------------------------------------------------------------------

### Introduction:

#### The featured dataset of this project is a dataset containing information about various features of stars. The variables included in the dataset are: Temperature, which is a measure of the surface temperature of the star; Luminosity, the amount of light the star emits; Radius, the size of the star; Absolute Magnitude, the brightness of the star accounting for distance (closely related to Luminosity); Star Type, whether it is a dwarf, main sequence, or giant star; Star Color, and Spectral Class, which both categorize the star based on the color of it. The dataset was found on Kaggle.com, created by a user who got much of the information from Wikipedia and derived certain missing values by using specific astrophysics equations. In total, the dataset has 240 observations, and does not need to be made tidy since each row is already an individual observation.

#### I am interested in exploring this dataset because I have an interest in astrophysics and plan to take a couple classes next semester on the subject. The dataset is made to help create a Hertzsprung-Russel diagram, which is a diagram plotting each of the variables against each other to help categorize the stars by their physical properties. From this project, I am interested in seeing which variables have the best potential to predict a stars classification, and which of the variables impact classification most significantly.

##### *Note: Star Type is a categorical variable measured 0-5. There are 6 difference categories of star in this dataset but they cannot be merged together as their features are far too distnict and would make the data skewed.*

``` r
stars <- read.csv('stars.csv')
```

### EDA:

``` r
library(tidyverse)
library(psych)
library(knitr)
if (!require(kableExtra)) install.packages("kableExtra")

#Create Correlation matrix with bivariate/univariate graphs
stars_numeric <- stars %>%
  select_if(is.numeric)

stars_categorical <- stars %>%
  select(starts_with("Star.type") | starts_with("Spectral")) %>%
  as.data.frame %>%
  mutate(Spectral.Class = factor(Spectral.Class, levels = c("O", "B", "A", "F", "G", "K", "M"))) %>%
  table()

pairs.panels(stars_numeric,
             method = "pearson",
             hist.col = "blue",
             smooth = FALSE, density = FALSE, ellipses = FALSE)
```

![image](https://user-images.githubusercontent.com/83439507/117617604-653e6080-b132-11eb-8c46-8efd6577dbd4.png)

``` r
stars_summary <- stars %>%
  group_by(Star.type) %>%
  summarise(mean(Temperature), mean(Luminosity), mean(Radius), mean(Absolute.magnitude))

kable(stars_summary, col.names = c("Star Type", 'Mean Temperature', 'Mean Luminosity', 'Mean Radius', 'Mean Brightness'), digits = 5, caption = "Table 1: Summary of Features of Star Types", align = "c") %>%
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Table 1: Summary of Features of Star Types
</caption>
<thead>
<tr>
<th style="text-align:center;">
Star Type
</th>
<th style="text-align:center;">
Mean Temperature
</th>
<th style="text-align:center;">
Mean Luminosity
</th>
<th style="text-align:center;">
Mean Radius
</th>
<th style="text-align:center;">
Mean Brightness
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
2997.950
</td>
<td style="text-align:center;">
0.00069
</td>
<td style="text-align:center;">
0.11002
</td>
<td style="text-align:center;">
17.56350
</td>
</tr>
<tr>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
3283.825
</td>
<td style="text-align:center;">
0.00541
</td>
<td style="text-align:center;">
0.34814
</td>
<td style="text-align:center;">
12.53998
</td>
</tr>
<tr>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
13931.450
</td>
<td style="text-align:center;">
0.00243
</td>
<td style="text-align:center;">
0.01073
</td>
<td style="text-align:center;">
12.58250
</td>
</tr>
<tr>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
16018.000
</td>
<td style="text-align:center;">
32067.38628
</td>
<td style="text-align:center;">
4.43030
</td>
<td style="text-align:center;">
-0.36742
</td>
</tr>
<tr>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
15347.850
</td>
<td style="text-align:center;">
301816.25000
</td>
<td style="text-align:center;">
51.15000
</td>
<td style="text-align:center;">
-6.36993
</td>
</tr>
<tr>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
11405.700
</td>
<td style="text-align:center;">
309246.52500
</td>
<td style="text-align:center;">
1366.89750
</td>
<td style="text-align:center;">
-9.65425
</td>
</tr>
</tbody>
</table>

``` r
kable(stars_categorical, caption = "Table 2: Spectral Class Count by Star Type") %>%
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Table 2: Spectral Class Count by Star Type
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
O
</th>
<th style="text-align:right;">
B
</th>
<th style="text-align:right;">
A
</th>
<th style="text-align:right;">
F
</th>
<th style="text-align:right;">
G
</th>
<th style="text-align:right;">
K
</th>
<th style="text-align:right;">
M
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
22
</td>
</tr>
</tbody>
</table>

#### Basic exploratory data analysis shows a few important features in the data. Firstly, looking at the main diagonal of the correlation/univariate/bivariate matrix, it can be seen that Temperature, Luminosity, and Radius are highly right skewed in their distribution, while Absolute Magnitude is bimodally distributed. Attempts were made to normalize the data using inverse, ln, log 10, sqrt, exp, and squared transformations but the data remained non-normal after each transform according to the Shaprio-Wilk test. The correlation coefficients reveal that the only highly correlated variables are Absolute Magnitude and Star Type, while the other variables have almost no correlation or a moderate one. Looking at the scatterplots of each variable compared to the other, it can be seen that some of the variables have non-linear relationships, often taking the shape of a logarithmic or an exponential decay distribution.

#### Looking at the tables, several trends can be spotted easily. Firstly, from Table 1, it can be seen that as Star Type changes, the means of the associated physical features also change quite directly. This makes sense, as the Star Type variables represents 0: Brown Dwarf, 1: Red Dwarf, 2: White Dwarf, 3: Main Sequence Star, 4: Supergiant, 5: Hypergiant. As the Star Type value increases, so should the size of the star (which can be seen by the increase in mean radius). Furthermore, brightness also increases with star type, however, the scale for brightness is such that more negative numbers are brighter than more positive ones. Finally, temperature also is consistent with the trend as giant and dwarf stars tend to burn cooler than main sequence ones.

### MANOVA:

### Assumptions:

``` r
library(car)
library(ggpubr)
if (!require(mvnormtest)) install.packages("mvnormtest")
library(rstatix)

#Covariance Assumption
box_m(stars[, c("Temperature", "Luminosity", "Absolute.magnitude", "Radius")], stars$Star.type)
```

    ## # A tibble: 1 x 4
    ##   statistic p.value parameter method                                            
    ##       <dbl>   <dbl>     <dbl> <chr>                                             
    ## 1     6819.       0        50 Box's M-test for Homogeneity of Covariance Matric~

``` r
#p-value = so low it's literally 0 -> assumption not met

#Multivariate Normality
stars %>%
  select(Temperature, Luminosity, Absolute.magnitude, Radius) %>%
  mshapiro_test()
```

    ## # A tibble: 1 x 2
    ##   statistic  p.value
    ##       <dbl>    <dbl>
    ## 1     0.802 9.27e-17

``` r
#p-value = 9.27e-17 -> assumption not met

#Multicollinearity
stars %>%
  cor_test(Temperature, Luminosity, Absolute.magnitude, Radius)
```

    ## # A tibble: 16 x 8
    ##    var1         var2            cor statistic        p conf.low conf.high method
    ##    <chr>        <chr>         <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <chr> 
    ##  1 Temperature  Temperature   1       1.04e+9 0.         1.00       1.00  Pears~
    ##  2 Temperature  Luminosity    0.39    6.60e+0 2.63e-10   0.281      0.495 Pears~
    ##  3 Temperature  Absolute.ma~ -0.42   -7.15e+0 1.09e-11  -0.519     -0.310 Pears~
    ##  4 Temperature  Radius        0.064   9.93e-1 3.22e- 1  -0.0629     0.189 Pears~
    ##  5 Luminosity   Temperature   0.39    6.60e+0 2.63e-10   0.281      0.495 Pears~
    ##  6 Luminosity   Luminosity    1       1.04e+9 0.         1.00       1.00  Pears~
    ##  7 Luminosity   Absolute.ma~ -0.69   -1.48e+1 1.27e-35  -0.753     -0.620 Pears~
    ##  8 Luminosity   Radius        0.53    9.55e+0 1.62e-18   0.428      0.612 Pears~
    ##  9 Absolute.ma~ Temperature  -0.42   -7.15e+0 1.09e-11  -0.519     -0.310 Pears~
    ## 10 Absolute.ma~ Luminosity   -0.69   -1.48e+1 1.27e-35  -0.753     -0.620 Pears~
    ## 11 Absolute.ma~ Absolute.ma~  1     Inf       0.         1          1     Pears~
    ## 12 Absolute.ma~ Radius       -0.61   -1.18e+1 1.00e-25  -0.683     -0.522 Pears~
    ## 13 Radius       Temperature   0.064   9.93e-1 3.22e- 1  -0.0629     0.189 Pears~
    ## 14 Radius       Luminosity    0.53    9.55e+0 1.62e-18   0.428      0.612 Pears~
    ## 15 Radius       Absolute.ma~ -0.61   -1.18e+1 1.00e-25  -0.683     -0.522 Pears~
    ## 16 Radius       Radius        1     Inf       0.         1          1     Pears~

``` r
#p-values = all very low, assumption met
```

#### For the MANOVA, only the assumptions of multicollinearity, and having more samples than variables were met. Other assumptions such as multivariate normal distribution and groups having the same covariance were not met (p-values for mshapiro and Box’s M test are 0 and 9.27e-17, respectively).

#### 

### Test:

``` r
manova_stars <- manova(cbind(Temperature, Luminosity, Radius, Absolute.magnitude) ~ Star.type, data = stars)
summary(manova_stars)
```

    ##            Df  Pillai approx F num Df den Df    Pr(>F)    
    ## Star.type   1 0.92412   715.46      4    235 < 2.2e-16 ***
    ## Residuals 238                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary.aov(manova_stars)
```

    ##  Response Temperature :
    ##              Df     Sum Sq    Mean Sq F value    Pr(>F)    
    ## Star.type     1 3.6862e+09 3686217558  48.411 3.323e-11 ***
    ## Residuals   238 1.8122e+10   76143913                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response Luminosity :
    ##              Df     Sum Sq    Mean Sq F value    Pr(>F)    
    ## Star.type     1 3.5251e+12 3.5251e+12  201.21 < 2.2e-16 ***
    ## Residuals   238 4.1697e+12 1.7520e+10                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response Radius :
    ##              Df   Sum Sq  Mean Sq F value    Pr(>F)    
    ## Star.type     1 27926149 27926149  184.65 < 2.2e-16 ***
    ## Residuals   238 35994421   151237                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response Absolute.magnitude :
    ##              Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## Star.type     1 24194.6 24194.6  2483.6 < 2.2e-16 ***
    ## Residuals   238  2318.5     9.7                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# All significant, concur with PERMANOVA

library(vegan)
dists <- stars %>%
  select(Temperature, Luminosity, Radius, Absolute.magnitude) %>%
  dist

adonis(dists ~ Star.type, data = stars)
```

    ## 
    ## Call:
    ## adonis(formula = dists ~ Star.type, data = stars) 
    ## 
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##            Df  SumsOfSqs    MeanSqs F.Model     R2 Pr(>F)    
    ## Star.type   1 3.5289e+12 3.5289e+12  200.55 0.4573  0.001 ***
    ## Residuals 238 4.1878e+12 1.7596e+10         0.5427           
    ## Total     239 7.7167e+12                    1.0000           
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Results agree, proceed to t tests

pairwise.t.test(stars$Temperature, stars$Star.type, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  stars$Temperature and stars$Star.type 
    ## 
    ##   0       1       2     3     4    
    ## 1 0.872   -       -     -     -    
    ## 2 3.4e-09 8.1e-09 -     -     -    
    ## 3 4.0e-12 1.1e-11 0.242 -     -    
    ## 4 3.8e-11 9.6e-11 0.427 0.707 -    
    ## 5 4.0e-06 8.1e-06 0.157 0.010 0.028
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(stars$Luminosity, stars$Star.type, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  stars$Luminosity and stars$Star.type 
    ## 
    ##   0      1      2      3      4   
    ## 1 1.00   -      -      -      -   
    ## 2 1.00   1.00   -      -      -   
    ## 3 0.20   0.20   0.20   -      -   
    ## 4 <2e-16 <2e-16 <2e-16 <2e-16 -   
    ## 5 <2e-16 <2e-16 <2e-16 <2e-16 0.77
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(stars$Radius, stars$Star.type, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  stars$Radius and stars$Star.type 
    ## 
    ##   0      1      2      3      4     
    ## 1 0.992  -      -      -      -     
    ## 2 0.997  0.989  -      -      -     
    ## 3 0.854  0.862  0.851  -      -     
    ## 4 0.031  0.031  0.030  0.048  -     
    ## 5 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(stars$Absolute.magnitude, stars$Star.type, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  stars$Absolute.magnitude and stars$Star.type 
    ## 
    ##   0       1       2       3       4      
    ## 1 < 2e-16 -       -       -       -      
    ## 2 < 2e-16 0.92    -       -       -      
    ## 3 < 2e-16 < 2e-16 < 2e-16 -       -      
    ## 4 < 2e-16 < 2e-16 < 2e-16 < 2e-16 -      
    ## 5 < 2e-16 < 2e-16 < 2e-16 < 2e-16 9.3e-14
    ## 
    ## P value adjustment method: none

#### 65 tests were conducted: 1 MANOVA, 4 ANOVA, and 60 t-tests. Thus, using the Bonferroni correction, the new p-value for signficance is 0.000769. This also means the probability of at least 1 Type-I error is 0.96435. According to the ANOVA tests, all of the variables have signficantly different means across at least one of the groups. Analyzing the t-tests, most of the variables have several significant differences between one star type and another. For example, across every variable, except Temperature, a star of type 5 (Hypergiant) has a signficant difference from at least 4 other star types. Absolute Magnitude shows a difference across every single star type except 1 and 2 (Red Dwarf and White Dwarf). Luminosity and Temperature seem to be the two variables where there are the least amount of significant differences, with 7 pairs of star types having insignficant p-values.

### Randomization Test:

``` r
obs_F <- 48.411

Fs <- replicate(5000,{
  # Randomly permute the Temperature across Star Type
  new <- stars %>%
    mutate(Temperature = sample(Temperature))
  # Compute variation within groups
  SSW <- new %>%
    group_by(Star.type) %>%
    summarize(SSW = sum((Temperature - mean(Temperature))^2)) %>%
    summarize(sum(SSW)) %>% 
    pull
  # Compute variation between groups
  SSB <- new %>% 
    mutate(mean = mean(Temperature)) %>%
    group_by(Star.type) %>% 
    mutate(groupmean = mean(Temperature)) %>%
    summarize(SSB = sum((mean - groupmean)^2)) %>%
    summarize(sum(SSB)) %>%
    pull
  # Compute the F-statistic (ratio of MSB and MSW)
  # df for SSB is 6 groups - 1 = 5
  # df for SSW is 240 observations - 6 groups = 234
  (SSB/5)/(SSW/234)
})

hist(Fs, prob = T, main = "Distribution of Sampled F values")
abline(v = obs_F, col="red",add=T)
```

![image](https://user-images.githubusercontent.com/83439507/117617649-77b89a00-b132-11eb-88e1-a7e57eb904e3.png)

``` r
mean(Fs > obs_F)
```

    ## [1] 0

#### Since many assumptions were not met for the ANOVA tests, a randomization test was conducted to test whether the mean temperature differs between star types. The null hypothesis was that the mean temperature was the same across star types, while the alternative is that it is different between at least 1 star type and the others. The results of the test show that the proportion of randomized F-statistics that are greater than the observed F-statistic is 0, meaning we can reject the null hypothesis and conclude that at least 1 star type’s mean temperature is different from the rest.

### Linear Regression Model:

``` r
# Mean Center Variables
stars <- stars %>%
  mutate(Temperature_C = Temperature - mean(Temperature)) %>%
  mutate(Luminosity_C = Luminosity - mean(Luminosity)) %>%
  mutate(Radius_C = Radius - mean(Radius)) %>%
  mutate(Absolute.magnitude_C = Absolute.magnitude - mean(Absolute.magnitude))

# Change Star.type to categorical variable
stars$Star.type <- as.character(stars$Star.type)

fit_1 <- lm(Temperature ~ Luminosity_C * Absolute.magnitude_C * Star.type, data = stars)
summary(fit_1)
```

    ## 
    ## Call:
    ## lm(formula = Temperature ~ Luminosity_C * Absolute.magnitude_C * 
    ##     Star.type, data = stars)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16668.0  -1400.7    -29.6    738.7  26885.5 
    ## 
    ## Coefficients: (6 not defined because of singularities)
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                  -3.894e+03  1.157e+04  -0.337
    ## Luminosity_C                                 -7.097e-02  4.615e-02  -1.538
    ## Absolute.magnitude_C                         -8.140e+02  8.673e+02  -0.939
    ## Star.type1                                   -1.330e+02  1.185e+04  -0.011
    ## Star.type2                                    3.630e+04  1.216e+04   2.984
    ## Star.type3                                   -9.956e+03  5.518e+04  -0.180
    ## Star.type4                                    8.157e+04  3.000e+04   2.718
    ## Star.type5                                    4.986e+04  2.052e+04   2.430
    ## Luminosity_C:Absolute.magnitude_C            -7.088e-03  3.342e-03  -2.121
    ## Luminosity_C:Star.type1                              NA         NA      NA
    ## Luminosity_C:Star.type2                              NA         NA      NA
    ## Luminosity_C:Star.type3                      -1.055e-01  5.052e-01  -0.209
    ## Luminosity_C:Star.type4                       4.639e-02  1.152e-01   0.403
    ## Luminosity_C:Star.type5                              NA         NA      NA
    ## Absolute.magnitude_C:Star.type1               1.793e+01  1.038e+03   0.017
    ## Absolute.magnitude_C:Star.type2              -3.126e+03  1.089e+03  -2.871
    ## Absolute.magnitude_C:Star.type3              -4.043e+03  6.193e+03  -0.653
    ## Absolute.magnitude_C:Star.type4               6.889e+03  2.709e+03   2.544
    ## Absolute.magnitude_C:Star.type5               3.660e+03  1.478e+03   2.476
    ## Luminosity_C:Absolute.magnitude_C:Star.type1         NA         NA      NA
    ##                                              Pr(>|t|)   
    ## (Intercept)                                   0.73666   
    ## Luminosity_C                                  0.12551   
    ## Absolute.magnitude_C                          0.34900   
    ## Star.type1                                    0.99105   
    ## Star.type2                                    0.00316 **
    ## Star.type3                                    0.85699   
    ## Star.type4                                    0.00708 **
    ## Star.type5                                    0.01591 * 
    ## Luminosity_C:Absolute.magnitude_C             0.03506 * 
    ## Luminosity_C:Star.type1                            NA   
    ## Luminosity_C:Star.type2                            NA   
    ## Luminosity_C:Star.type3                       0.83481   
    ## Luminosity_C:Star.type4                       0.68752   
    ## Luminosity_C:Star.type5                            NA   
    ## Absolute.magnitude_C:Star.type1               0.98623   
    ## Absolute.magnitude_C:Star.type2               0.00448 **
    ## Absolute.magnitude_C:Star.type3               0.51456   
    ## Absolute.magnitude_C:Star.type4               0.01165 * 
    ## Absolute.magnitude_C:Star.type5               0.01403 * 
    ## Luminosity_C:Absolute.magnitude_C:Star.type1       NA   
    ##  [ reached getOption("max.print") -- omitted 4 rows ]
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5984 on 222 degrees of freedom
    ## Multiple R-squared:  0.6355, Adjusted R-squared:  0.6076 
    ## F-statistic: 22.77 on 17 and 222 DF,  p-value: < 2.2e-16

``` r
ggplot(stars, aes(x = Absolute.magnitude, y = Temperature)) +
  geom_smooth(aes(col = Star.type), method = 'lm') +
  labs(title = "Star Temperature vs Absolute Magnitude, colored by Star Type",
       x = "Absolute Magnitude",
       y = "Temperature (K)",
       col = "Star Type")
```

![image](https://user-images.githubusercontent.com/83439507/117617685-82732f00-b132-11eb-9aa3-ee6068738a9a.png)

``` r
plot(fit_1, which = 1) # Residuals
```

![image](https://user-images.githubusercontent.com/83439507/117617693-869f4c80-b132-11eb-9bae-641da78a6b51.png)

``` r
plot(fit_1, which = 2) # QQ Plot
```

![image](https://user-images.githubusercontent.com/83439507/117617706-8a32d380-b132-11eb-82fe-619e2b84324c.png)
``` r
# Robust Standard Errors
if (!require(sandwich)) install.packages("sandwich")
library(sandwich)
library(lmtest)
coeftest(fit_1, vcov = vcovHC(fit_1))[,1:4]
```

    ##                                                   Estimate   Std. Error
    ## (Intercept)                                  -3.894248e+03 7.024162e+03
    ## Luminosity_C                                 -7.097238e-02 6.507172e-02
    ## Absolute.magnitude_C                         -8.139971e+02 4.984044e+02
    ## Star.type1                                   -1.329921e+02 8.698278e+02
    ## Star.type2                                    3.630083e+04 3.442246e+03
    ## Star.type3                                   -9.956161e+03 6.843298e+04
    ## Star.type4                                    8.156753e+04 5.051755e+04
    ## Star.type5                                    4.985622e+04 2.607701e+04
    ## Luminosity_C:Absolute.magnitude_C            -7.087863e-03 4.610551e-03
    ## Luminosity_C:Star.type3                      -1.054815e-01 6.380055e-01
    ## Luminosity_C:Star.type4                       4.638671e-02 2.034250e-01
    ## Absolute.magnitude_C:Star.type1               1.793273e+01 7.207679e+01
    ## Absolute.magnitude_C:Star.type2              -3.126497e+03 3.766656e+02
    ## Absolute.magnitude_C:Star.type3              -4.043052e+03 7.668307e+03
    ## Absolute.magnitude_C:Star.type4               6.889486e+03 4.563903e+03
    ## Absolute.magnitude_C:Star.type5               3.659844e+03 1.739030e+03
    ## Luminosity_C:Absolute.magnitude_C:Star.type3 -2.116462e-02 7.088052e-02
    ## Luminosity_C:Absolute.magnitude_C:Star.type4  3.380063e-03 1.771696e-02
    ##                                                 t value     Pr(>|t|)
    ## (Intercept)                                  -0.5544074 5.798581e-01
    ## Luminosity_C                                 -1.0906793 2.765964e-01
    ## Absolute.magnitude_C                         -1.6332063 1.038431e-01
    ## Star.type1                                   -0.1528948 8.786202e-01
    ## Star.type2                                   10.5456827 2.432453e-21
    ## Star.type3                                   -0.1454878 8.844578e-01
    ## Star.type4                                    1.6146376 1.078096e-01
    ## Star.type5                                    1.9118841 5.717854e-02
    ## Luminosity_C:Absolute.magnitude_C            -1.5373136 1.256406e-01
    ## Luminosity_C:Star.type3                      -0.1653300 8.688347e-01
    ## Luminosity_C:Star.type4                       0.2280286 8.198339e-01
    ## Absolute.magnitude_C:Star.type1               0.2488004 8.037452e-01
    ## Absolute.magnitude_C:Star.type2              -8.3004575 1.011647e-14
    ## Absolute.magnitude_C:Star.type3              -0.5272418 5.985522e-01
    ## Absolute.magnitude_C:Star.type4               1.5095604 1.325776e-01
    ## Absolute.magnitude_C:Star.type5               2.1045313 3.645699e-02
    ## Luminosity_C:Absolute.magnitude_C:Star.type3 -0.2985958 7.655278e-01
    ## Luminosity_C:Absolute.magnitude_C:Star.type4  0.1907813 8.488713e-01

``` r
# Bootstrapping Standard Errors from Observations
# Repeat bootstrapping 5000 times, saving the coefficients each time
samp_SEs <- replicate(5000, {
  # Bootstrap your data (resample observations)
  boot_data <- sample_frac(stars, replace = TRUE)
  # Fit regression model
  fitboot <- lm(Temperature ~ Luminosity_C * Absolute.magnitude_C * Star.type, data = boot_data)
  # Save the coefficients
  coef(fitboot)
})

# Estimated SEs
samp_SEs %>%
  # Transpose the obtained matrices
  t %>%
  # Consider the matrix as a data frame
  as.data.frame %>%
  # Compute the standard error (standard deviation of the sampling distribution)
  summarize_all(sd)
```

    ##   (Intercept) Luminosity_C Absolute.magnitude_C Star.type1 Star.type2
    ## 1    9789.091   0.09108001              683.745   79523411 4783846636
    ##   Star.type3 Star.type4 Star.type5 Luminosity_C:Absolute.magnitude_C
    ## 1   147398.7    48052.1   29900.02                       0.006355292
    ##   Luminosity_C:Star.type1 Luminosity_C:Star.type2 Luminosity_C:Star.type3
    ## 1                      NA                      NA                1.374541
    ##   Luminosity_C:Star.type4 Luminosity_C:Star.type5
    ## 1               0.2113825                      NA
    ##   Absolute.magnitude_C:Star.type1 Absolute.magnitude_C:Star.type2
    ## 1                        65.51631                         374.077
    ##   Absolute.magnitude_C:Star.type3 Absolute.magnitude_C:Star.type4
    ## 1                        16618.44                         4355.22
    ##   Absolute.magnitude_C:Star.type5 Luminosity_C:Absolute.magnitude_C:Star.type1
    ## 1                        2003.887                                           NA
    ##   Luminosity_C:Absolute.magnitude_C:Star.type2
    ## 1                                           NA
    ##   Luminosity_C:Absolute.magnitude_C:Star.type3
    ## 1                                     0.153865
    ##   Luminosity_C:Absolute.magnitude_C:Star.type4
    ## 1                                   0.01861463
    ##   Luminosity_C:Absolute.magnitude_C:Star.type5
    ## 1                                           NA

#### A linear regression model was constructed to predict star temperature based on luminosity, absolute magnitude, and star type. The model created has an R-squared value of 0.6076, meaning 60.76% of the variance in the model is explained by luminosity, absolute magnitude, and star type, making the model a decent fit and predictor of the variable. The coefficient of mean centered Luminosity is -7.097e-02, with a p-value of 0.1255, making it not a significant predictor, but for every increase in 1 Lo (unit for Luminosity), there is .07097 K decrease in the stars temperature. The coefficient of mean centered Absolute Magnitude says that for every 1 Mv increase in absolute magnitude there is general decrease of 814.0 K decrease in temperature. The Star Type coefficients are the most significant predictors and imply that if a star is classified as a star besides type 0 (Brown Dwarf), the slope of the line increases/decreases by the coefficient amount. The interaction visualization compares a stars type to its absolute magnitude. The coefficients of the interactions between these two variables state that the star classification changes the slope of the absolute magnitude coefficient by the coefficient amount. The graph itself shows a significant difference between star type 0 and star types 2, 3, 4, and 5, which is to be expected as most of the star types have different temepratures and brightnesses, while Brown and Red dwarfs are fairly similar.

#### None of the assumptions for the linear regression were not met in conducting the model. From the correlation matrix it was seen that few of the variables had a linear correlation, while the QQ plot shows an obvious lack of normality, and the residual plot shows heteroskedacsiticity, due to the cone shape of the residual spread. Thus, robust standard errors and bootstrapping methods were used to recompute similar parameters. Robust SEs estimated coefficients to have p-values where only 4 coefficients were significant (Star types 2 and 5, Absolute.magnitude:Star.type2/5), compared to 7 from the original regression model. The coefficients more or less remained fairly similar, though the Robust SEs tend to be slightly more negative. Bootstrapped SEs that were produced differ quite drastically from the original regression model, as most of the coefficients are 1 to 3 orders of magnitude greater than the original regression model; the non-interaction coefficients tend to be much larger, while the interaction coefficients are much closer to 0.

### Logistic Regression Model:

``` r
# Create binary categorical variable based on whether the star is a main sequence or not
stars <- stars %>%
  mutate(hypergiant = ifelse(Star.type == 5, 1, 0))

# Create model
model_a <- glm(hypergiant ~ Temperature + Luminosity, data = stars, family = binomial(link="logit"))
summary(model_a)
```

    ## 
    ## Call:
    ## glm(formula = hypergiant ~ Temperature + Luminosity, family = binomial(link = "logit"), 
    ##     data = stars)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5656  -0.4200  -0.4007  -0.2825   2.2022  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.212e+00  3.169e-01  -6.979 2.98e-12 ***
    ## Temperature -5.874e-05  2.494e-05  -2.355   0.0185 *  
    ## Luminosity   7.790e-06  1.264e-06   6.162 7.18e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 216.27  on 239  degrees of freedom
    ## Residual deviance: 159.76  on 237  degrees of freedom
    ## AIC: 165.76
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
stars$prob <- predict(model_a, type = "response")
stars$predicted <- ifelse(stars$prob > .5, "Hypergiant", "Other") 

# Confusion Matrix
table(truth = stars$hypergiant, prediction = stars$predicted) %>% 
  addmargins
```

    ##      prediction
    ## truth Hypergiant Other Sum
    ##   0            9   191 200
    ##   1           10    30  40
    ##   Sum         19   221 240

``` r
# Plot
ggplot(stars, aes(Luminosity, hypergiant)) +
  geom_jitter(aes(color = predicted), width = .3, height = 0) +
  stat_smooth(method="glm", method.args = list(family="binomial"), se = FALSE) +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab("Pr(Hypergiant)")
```

![image](https://user-images.githubusercontent.com/83439507/117617726-91f27800-b132-11eb-837f-e7202c75b8f3.png)

``` r
# Save the predicted log-odds in the dataset
stars$logit <- predict(model_a)

# Compare to the outcome in the dataset with a density plot
ggplot(stars, aes(logit, fill = as.factor(hypergiant))) +
  geom_density(alpha = .3) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(fill = "Hypergiant")
```

![image](https://user-images.githubusercontent.com/83439507/117617739-97e85900-b132-11eb-8a46-459eac4fe439.png)

``` r
#Accuracy = 83.75%
(10 + 191) / 240
```

    ## [1] 0.8375

``` r
#Sensitivity (TPR) = 25%
10 / 40
```

    ## [1] 0.25

``` r
#Specificity (TNR) = 95.5%
191 / 200
```

    ## [1] 0.955

``` r
# Recall/Precision (PPV) = 52.63%
10 / 19
```

    ## [1] 0.5263158

``` r
# ROC Plot
library(plotROC)
ROCplot1 <- ggplot(stars) + 
  geom_roc(aes(d = hypergiant, m = prob), cutoffs.at = list(0.1, 0.5, 0.9))
ROCplot1
```

![image](https://user-images.githubusercontent.com/83439507/117617750-9cad0d00-b132-11eb-8730-44551b4c9acf.png)

``` r
# AUC
calc_auc(ROCplot1)
```

    ##   PANEL group      AUC
    ## 1     1    -1 0.912125

#### A logistic regression model was created to predict whether a star was a Hypergiant from its temperature and luminosity. The coeffecients of Temperature and Luminosity were -5.874e-05 and 7.790e-06, respectively. These mean that, while holding other coefficients constant, for every 1 K increase in temperature, there is a -5.874e-05 decrease in the log odds of the star being a Hypergiant, similarly, for every 1 Lo increase in luminosity there is a 7.790e-06 increase in the star being a Hypergiant. While these may seem like very small increases, it makes sense due to the massive scales that temperature and luminosity are measured on. Looking at the ROC plot, it can be seen that a 0.5 cut off for the probability is not ideal since it only gives an approximalte 0.25 true positive rate and a 0.05 false positive rate. A better cutoff rate would be 0.1 since its true positve rate is nearly 1, and the false positive rate is only 0.13. The overall AUC for the ROC is 0.9121, meaning the model is a fairly good predictor.
