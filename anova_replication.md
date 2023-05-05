Replication of Mixed Design ANOVA on Web-Based Positive Psychology
Interventions
================
2023-05-03

Goals for this analysis replication:

1.  (TODO) Means and standard deviations of happiness (AHI) and
    depression (CES- D) measures for each intervention group on each of
    the six measurement occasions (pretest, posttest, 1-week follow-up,
    1-month follow-up, 3-month follow-up, 6-month follow-up).
    1.  visual table to summarize the data
2.  (Done) ANOVA (mixed design analyses of variance)
    1.  (provide anova table and visuals of testing for assumptions)
3.  (Done) Visual inspection of the 95% confidence intervals lends
    support to the conclusions from the mixed-design analyses of
    variance:
    1.  visual over continuous spaced time
4.  (TODO) Multilevel linear mixed-effects models, with
    maximum-likelihood estimation, were used to relate longitudinal
    changes in AHI and CES-D scores to intervention group.

For the purpose of direct comparison and replication, we conducted
similar analyses, including in the ANOVAs only those participants who
completed all the study requirements (N = 72). In the next section, we
report additional analyses using all enrolled participants.

## 4x6 Mixed Design ANOVA

### Load Packages

``` r
# Load the packages:

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.1     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.1.8
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(ggplot2)
library(gplots)
```

    ## 
    ## Attaching package: 'gplots'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(emmeans)
library(nortest)
library(afex)
```

    ## Loading required package: lme4
    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## ************
    ## Welcome to afex. For support visit: http://afex.singmann.science/
    ## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ## - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    ## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    ## - NEWS: emmeans() for ANOVA models now uses model = 'multivariate' as default.
    ## - Get and set global package options with: afex_options()
    ## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
    ## - For example analyses see: browseVignettes("afex")
    ## ************
    ## 
    ## Attaching package: 'afex'
    ## 
    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

``` r
library(ggfortify)
library(ggResidpanel)

# Disable scientific notation:
options(scipen=999, digits=4)
```

### Load CSV

``` r
#Load Files
ahi_cesd <- read_csv("data/ahi-cesd.csv")
```

    ## Rows: 992 Columns: 50
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (50): id, occasion, elapsed.days, intervention, ahi01, ahi02, ahi03, ahi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#View Files
ahi_cesd <- as.data.frame(ahi_cesd)
head(ahi_cesd)
```

    ##   id occasion elapsed.days intervention ahi01 ahi02 ahi03 ahi04 ahi05 ahi06
    ## 1  1        0        0.000            4     2     3     2     3     3     2
    ## 2  1        1       11.773            4     3     3     4     3     3     4
    ## 3  2        0        0.000            1     3     4     3     4     2     3
    ## 4  2        1        8.018            1     3     4     4     4     3     3
    ## 5  2        2       14.304            1     3     4     4     4     3     3
    ## 6  2        3       31.984            1     3     4     4     4     4     4
    ##   ahi07 ahi08 ahi09 ahi10 ahi11 ahi12 ahi13 ahi14 ahi15 ahi16 ahi17 ahi18 ahi19
    ## 1     3     3     3     2     3     3     4     2     3     3     2     2     3
    ## 2     4     3     3     2     2     3     4     3     3     3     2     3     3
    ## 3     4     3     3     3     2     3     4     3     3     3     3     3     3
    ## 4     4     4     4     3     3     4     4     4     3     4     4     4     4
    ## 5     4     3     4     4     3     4     4     4     3     4     4     4     4
    ## 6     4     3     4     4     4     4     4     4     3     4     4     4     4
    ##   ahi20 ahi21 ahi22 ahi23 ahi24 cesd01 cesd02 cesd03 cesd04 cesd05 cesd06
    ## 1     3     2     2     3     2      2      1      1      4      1      2
    ## 2     3     3     3     4     2      2      1      1      4      1      1
    ## 3     3     2     2     4     3      1      1      1      1      1      1
    ## 4     4     4     3     4     4      3      2      1      3      1      1
    ## 5     3     4     4     4     4      1      1      1      1      1      1
    ## 6     4     4     4     4     4      1      1      1      1      1      1
    ##   cesd07 cesd08 cesd09 cesd10 cesd11 cesd12 cesd13 cesd14 cesd15 cesd16 cesd17
    ## 1      1      3      1      1      3      2      2      3      1      2      1
    ## 2      2      4      1      2      2      4      1      2      1      3      1
    ## 3      1      1      1      1      2      4      1      1      1      4      1
    ## 4      2      1      1      1      1      3      1      1      1      3      1
    ## 5      1      1      1      1      3      4      3      1      1      1      1
    ## 6      1      4      1      1      2      1      1      1      1      3      1
    ##   cesd18 cesd19 cesd20 ahiTotal cesdTotal
    ## 1      1      2      2       63        14
    ## 2      1      1      1       73         6
    ## 3      1      1      1       73         7
    ## 4      1      1      1       89        10
    ## 5      1      1      1       89        13
    ## 6      1      1      1       93         8

### Filter for only Non.Dropout Participants

``` r
## count unique id with all recorded ocassion c(1,2,3,4,5)

library(hash)
```

    ## hash-2.2.6.2 provided by Decision Patterns

    ## 
    ## Attaching package: 'hash'

    ## The following object is masked from 'package:psych':
    ## 
    ##     make.keys

``` r
#create new dataframe of ahi_cesd.csv
dat <- ahi_cesd

#create a new dictionary {id:occasion}
h <- hash()

# loop through each id and add the occasion's recorded
for (i in 1:nrow(dat)) {
  key <- as.character(dat[i,'id'])
  if (!has.key(key,h)) {
    h[key] <- deframe(dat[i,'occasion'])
  } else {
    h[key] <- append(h[[key]], deframe(dat[i,'occasion']) )
  }
}

#return the count of id that completed all c(1,2,3,4,5) occasions
completed <- c()

for (i in 1:length(h)) {
  #access key
  k <- keys( h[as.character(i)] )
  
  #access values
  v <- values(h[k],USE.NAMES=FALSE, simplify=FALSE)
  v <- v[[1]]
  
  #add key to completed vector
  if (all(v == c(0,1,2,3,4,5))) {
    completed = append(completed,i)
  }
}

completed
```

    ##  [1]   2  11  12  14  24  27  28  37  38  40  41  44  48  50  65  66  74  79  83
    ## [20]  92  95  98 111 114 118 124 127 128 130 131 132 134 136 137 141 143 153 164
    ## [39] 167 169 177 178 182 184 186 187 190 191 194 199 205 207 211 222 237 238 239
    ## [58] 241 243 245 249 251 254 257 259 265 273 276 278 281 282 283

### Create new df of only Non.Dropout Participants for ANOVA

``` r
#Create new df to work with
ahi.df <- ahi_cesd %>%
  select(id, intervention, occasion, ahiTotal) %>%
  mutate(status = ifelse(id %in% completed, '1', '0'))

head(ahi.df)
```

    ##   id intervention occasion ahiTotal status
    ## 1  1            4        0       63      0
    ## 2  1            4        1       73      0
    ## 3  2            1        0       73      1
    ## 4  2            1        1       89      1
    ## 5  2            1        2       89      1
    ## 6  2            1        3       93      1

``` r
# Factor the predictors
#Using Signiture Strengths', "Three Good Things", "Gratitude Visit", "Recording early memories"

ahi.df$intervention <- factor(ahi.df$intervention, 
                              labels = c('Using Signiture Strengths', 
                                         "Three Good Things", "Gratitude Visit", 
                                         "Recording early memories"),
                              levels = c(1,2,3,4), 
                              ordered = FALSE)

ahi.df$occasion <- factor(ahi.df$occasion, 
                          labels = c("t0","t7","t14","t38","t98","t189"),
                          levels = c(0,1,2,3,4,5), 
                          ordered = TRUE)

ahi.df$intervention <- relevel(ahi.df$intervention, ref=4)
```

Part 2

``` r
# Is the data balanced or unbalanced?
table(ahi.df$intervention, ahi.df$occasion)
```

    ##                            
    ##                             t0 t7 t14 t38 t98 t189
    ##   Recording early memories  73 45  39  37  34   33
    ##   Using Signiture Strengths 72 30  38  29  36   27
    ##   Three Good Things         76 48  48  43  37   37
    ##   Gratitude Visit           74 24  32  30  27   23

Part 3

``` r
m <- aov_4(ahiTotal ~ intervention*occasion + (occasion|id), type=2, data=ahi.df)
```

    ## Contrasts set to contr.sum for the following variables: intervention

``` r
m
```

    ## Anova Table (Type 2 tests)
    ## 
    ## Response: ahiTotal
    ##                  Effect            df    MSE        F  ges p.value
    ## 1          intervention         3, 68 873.47     0.12 .004    .951
    ## 2              occasion  3.73, 253.74  50.38 5.74 *** .015   <.001
    ## 3 intervention:occasion 11.19, 253.74  50.38     0.50 .004    .906
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

``` r
summary(m)
```

    ## 
    ## Univariate Type II Repeated-Measures ANOVA Assuming Sphericity
    ## 
    ##                        Sum Sq num Df Error SS den Df F value
    ## (Intercept)           2339656      1    59396     68 2678.59
    ## intervention              302      3    59396     68    0.12
    ## occasion                 1079      5    12783    340    5.74
    ## intervention:occasion     281     15    12783    340    0.50
    ##                                     Pr(>F)    
    ## (Intercept)           < 0.0000000000000002 ***
    ## intervention                          0.95    
    ## occasion                          0.000042 ***
    ## intervention:occasion                 0.94    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Mauchly Tests for Sphericity
    ## 
    ##                       Test statistic       p-value
    ## occasion                       0.317 0.00000000016
    ## intervention:occasion          0.317 0.00000000016
    ## 
    ## 
    ## Greenhouse-Geisser and Huynh-Feldt Corrections
    ##  for Departure from Sphericity
    ## 
    ##                       GG eps Pr(>F[GG])    
    ## occasion               0.746    0.00029 ***
    ## intervention:occasion  0.746    0.90555    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                       HF eps Pr(>F[HF])
    ## occasion               0.795  0.0001977
    ## intervention:occasion  0.795  0.9138725

``` r
nice(m,
     intercept=TRUE,
     correction = "none")
```

    ## Anova Table (Type 2 tests)
    ## 
    ## Response: ahiTotal
    ##                  Effect      df    MSE           F  ges p.value
    ## 1           (Intercept)   1, 68 873.47 2678.59 *** .970   <.001
    ## 2          intervention   3, 68 873.47        0.12 .004    .951
    ## 3              occasion  5, 340  37.60    5.74 *** .015   <.001
    ## 4 intervention:occasion 15, 340  37.60        0.50 .004    .941
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

Replication of the paper’s 4x6 mixed-design ANOVA. Intervention ( 4
levels) x Occasion (6 levels) on AHI scores.

Viewing the ANOVA table, intervention and the interaction involving
intervention is not statistically significant. Occasion had a
statistically significant effect (p = \<0.001). Potentially showing
participant scores increasing over time.

``` r
#Plot the data
emmip(m, intervention ~ occasion, CIs=TRUE,
       xlab="Time",
      ylab="ahiTotal")
```

![](anova_replication_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ occasion*intervention,  model="multivariate",adjust="tukey")

posthocs$emmeans # Estimated marginal means (EMM)
```

    ##  occasion intervention              emmean   SE df lower.CL upper.CL
    ##  t0       Recording early memories    71.6 2.78 68     66.1     77.1
    ##  t7       Recording early memories    74.2 2.85 68     68.5     79.9
    ##  t14      Recording early memories    74.3 2.83 68     68.7     80.0
    ##  t38      Recording early memories    75.4 3.00 68     69.4     81.4
    ##  t98      Recording early memories    75.7 3.20 68     69.3     82.0
    ##  t189     Recording early memories    74.5 3.15 68     68.3     80.8
    ##  t0       Using Signiture Strengths   67.9 3.01 68     61.9     74.0
    ##  t7       Using Signiture Strengths   72.7 3.10 68     66.5     78.8
    ##  t14      Using Signiture Strengths   71.8 3.07 68     65.7     78.0
    ##  t38      Using Signiture Strengths   74.6 3.25 68     68.1     81.1
    ##  t98      Using Signiture Strengths   75.9 3.47 68     69.0     82.9
    ##  t189     Using Signiture Strengths   75.4 3.42 68     68.6     82.2
    ##  t0       Three Good Things           70.0 2.54 68     64.9     75.0
    ##  t7       Three Good Things           72.6 2.60 68     67.4     77.8
    ##  t14      Three Good Things           73.7 2.59 68     68.5     78.9
    ##  t38      Three Good Things           72.1 2.74 68     66.6     77.5
    ##  t98      Three Good Things           74.2 2.92 68     68.4     80.1
    ##  t189     Three Good Things           74.1 2.88 68     68.3     79.8
    ##  t0       Gratitude Visit             72.5 3.75 68     65.1     80.0
    ##  t7       Gratitude Visit             75.3 3.85 68     67.6     83.0
    ##  t14      Gratitude Visit             74.8 3.82 68     67.2     82.4
    ##  t38      Gratitude Visit             75.9 4.04 68     67.8     84.0
    ##  t98      Gratitude Visit             75.2 4.32 68     66.6     83.8
    ##  t189     Gratitude Visit             75.9 4.25 68     67.4     84.4
    ## 
    ## Confidence level used: 0.95

``` r
posthocs$contrasts %>% 
  summary(infer = TRUE) # Post-hoc comparisons
```

    ##  contrast                                                       estimate   SE
    ##  t0 Recording early memories - t7 Recording early memories        -2.600 1.61
    ##  t0 Recording early memories - t14 Recording early memories       -2.700 1.91
    ##  t0 Recording early memories - t38 Recording early memories       -3.800 2.47
    ##  t0 Recording early memories - t98 Recording early memories       -4.050 2.36
    ##  t0 Recording early memories - t189 Recording early memories      -2.950 2.32
    ##  t0 Recording early memories - t0 Using Signiture Strengths        3.659 4.10
    ##  t0 Recording early memories - t7 Using Signiture Strengths       -1.047 4.16
    ##  t0 Recording early memories - t14 Using Signiture Strengths      -0.224 4.14
    ##  t0 Recording early memories - t38 Using Signiture Strengths      -2.988 4.28
    ##  t0 Recording early memories - t98 Using Signiture Strengths      -4.341 4.45
    ##  t0 Recording early memories - t189 Using Signiture Strengths     -3.812 4.41
    ##  t0 Recording early memories - t0 Three Good Things                1.642 3.76
    ##  t0 Recording early memories - t7 Three Good Things               -0.983 3.81
    ##  t0 Recording early memories - t14 Three Good Things              -2.108 3.80
    ##  t0 Recording early memories - t38 Three Good Things              -0.483 3.90
    ##  t0 Recording early memories - t98 Three Good Things              -2.650 4.03
    ##  t0 Recording early memories - t189 Three Good Things             -2.483 4.00
    ##  t0 Recording early memories - t0 Gratitude Visit                 -0.945 4.67
    ##  t0 Recording early memories - t7 Gratitude Visit                 -3.673 4.75
    ##  t0 Recording early memories - t14 Gratitude Visit                -3.218 4.72
    ##  t0 Recording early memories - t38 Gratitude Visit                -4.309 4.91
    ##  t0 Recording early memories - t98 Gratitude Visit                -3.582 5.13
    ##  t0 Recording early memories - t189 Gratitude Visit               -4.309 5.08
    ##  t7 Recording early memories - t14 Recording early memories       -0.100 1.11
    ##  t7 Recording early memories - t38 Recording early memories       -1.200 1.55
    ##  t7 Recording early memories - t98 Recording early memories       -1.450 1.86
    ##  t7 Recording early memories - t189 Recording early memories      -0.350 1.98
    ##  t7 Recording early memories - t0 Using Signiture Strengths        6.259 4.15
    ##  t7 Recording early memories - t7 Using Signiture Strengths        1.553 4.21
    ##  t7 Recording early memories - t14 Using Signiture Strengths       2.376 4.19
    ##  t7 Recording early memories - t38 Using Signiture Strengths      -0.388 4.33
    ##  t7 Recording early memories - t98 Using Signiture Strengths      -1.741 4.49
    ##  t7 Recording early memories - t189 Using Signiture Strengths     -1.212 4.45
    ##  t7 Recording early memories - t0 Three Good Things                4.242 3.82
    ##  t7 Recording early memories - t7 Three Good Things                1.617 3.86
    ##  t7 Recording early memories - t14 Three Good Things               0.492 3.85
    ##  t7 Recording early memories - t38 Three Good Things               2.117 3.95
    ##  t7 Recording early memories - t98 Three Good Things              -0.050 4.08
    ##  t7 Recording early memories - t189 Three Good Things              0.117 4.05
    ##  t7 Recording early memories - t0 Gratitude Visit                  1.655 4.71
    ##  t7 Recording early memories - t7 Gratitude Visit                 -1.073 4.79
    ##  t7 Recording early memories - t14 Gratitude Visit                -0.618 4.77
    ##  t7 Recording early memories - t38 Gratitude Visit                -1.709 4.95
    ##  t7 Recording early memories - t98 Gratitude Visit                -0.982 5.17
    ##  t7 Recording early memories - t189 Gratitude Visit               -1.709 5.12
    ##  t14 Recording early memories - t38 Recording early memories      -1.100 1.51
    ##  t14 Recording early memories - t98 Recording early memories      -1.350 1.62
    ##  t14 Recording early memories - t189 Recording early memories     -0.250 2.00
    ##  t14 Recording early memories - t0 Using Signiture Strengths       6.359 4.14
    ##  t14 Recording early memories - t7 Using Signiture Strengths       1.653 4.20
    ##  t14 Recording early memories - t14 Using Signiture Strengths      2.476 4.18
    ##  t14 Recording early memories - t38 Using Signiture Strengths     -0.288 4.31
    ##  t14 Recording early memories - t98 Using Signiture Strengths     -1.641 4.48
    ##  t14 Recording early memories - t189 Using Signiture Strengths    -1.112 4.44
    ##  t14 Recording early memories - t0 Three Good Things               4.342 3.80
    ##  t14 Recording early memories - t7 Three Good Things               1.717 3.85
    ##  t14 Recording early memories - t14 Three Good Things              0.592 3.84
    ##  t14 Recording early memories - t38 Three Good Things              2.217 3.94
    ##  t14 Recording early memories - t98 Three Good Things              0.050 4.07
    ##  t14 Recording early memories - t189 Three Good Things             0.217 4.04
    ##  t14 Recording early memories - t0 Gratitude Visit                 1.755 4.70
    ##  t14 Recording early memories - t7 Gratitude Visit                -0.973 4.78
    ##  t14 Recording early memories - t14 Gratitude Visit               -0.518 4.76
    ##  t14 Recording early memories - t38 Gratitude Visit               -1.609 4.94
    ##  t14 Recording early memories - t98 Gratitude Visit               -0.882 5.16
    ##  t14 Recording early memories - t189 Gratitude Visit              -1.609 5.11
    ##  t38 Recording early memories - t98 Recording early memories      -0.250 2.04
    ##  t38 Recording early memories - t189 Recording early memories      0.850 2.26
    ##  t38 Recording early memories - t0 Using Signiture Strengths       7.459 4.25
    ##  t38 Recording early memories - t7 Using Signiture Strengths       2.753 4.31
    ##  t38 Recording early memories - t14 Using Signiture Strengths      3.576 4.29
    ##  t38 Recording early memories - t38 Using Signiture Strengths      0.812 4.42
    ##  t38 Recording early memories - t98 Using Signiture Strengths     -0.541 4.59
    ##  t38 Recording early memories - t189 Using Signiture Strengths    -0.012 4.55
    ##  t38 Recording early memories - t0 Three Good Things               5.442 3.93
    ##  t38 Recording early memories - t7 Three Good Things               2.817 3.97
    ##  t38 Recording early memories - t14 Three Good Things              1.692 3.96
    ##  t38 Recording early memories - t38 Three Good Things              3.317 4.06
    ##  t38 Recording early memories - t98 Three Good Things              1.150 4.19
    ##  t38 Recording early memories - t189 Three Good Things             1.317 4.16
    ##  t38 Recording early memories - t0 Gratitude Visit                 2.855 4.80
    ##  t38 Recording early memories - t7 Gratitude Visit                 0.127 4.88
    ##  t38 Recording early memories - t14 Gratitude Visit                0.582 4.86
    ##  t38 Recording early memories - t38 Gratitude Visit               -0.509 5.03
    ##  t38 Recording early memories - t98 Gratitude Visit                0.218 5.26
    ##  t38 Recording early memories - t189 Gratitude Visit              -0.509 5.20
    ##  t98 Recording early memories - t189 Recording early memories      1.100 2.00
    ##  t98 Recording early memories - t0 Using Signiture Strengths       7.709 4.40
    ##  t98 Recording early memories - t7 Using Signiture Strengths       3.003 4.45
    ##  t98 Recording early memories - t14 Using Signiture Strengths      3.826 4.44
    ##  t98 Recording early memories - t38 Using Signiture Strengths      1.062 4.56
    ##  t98 Recording early memories - t98 Using Signiture Strengths     -0.291 4.72
    ##  t98 Recording early memories - t189 Using Signiture Strengths     0.238 4.68
    ##  t98 Recording early memories - t0 Three Good Things               5.692 4.08
    ##  t98 Recording early memories - t7 Three Good Things               3.067 4.13
    ##  t98 Recording early memories - t14 Three Good Things              1.942 4.12
    ##  t98 Recording early memories - t38 Three Good Things              3.567 4.21
    ##  t98 Recording early memories - t98 Three Good Things              1.400 4.33
    ##  t98 Recording early memories - t189 Three Good Things             1.567 4.30
    ##  t98 Recording early memories - t0 Gratitude Visit                 3.105 4.93
    ##  t98 Recording early memories - t7 Gratitude Visit                 0.377 5.00
    ##  t98 Recording early memories - t14 Gratitude Visit                0.832 4.99
    ##  t98 Recording early memories - t38 Gratitude Visit               -0.259 5.16
    ##  t98 Recording early memories - t98 Gratitude Visit                0.468 5.37
    ##  t98 Recording early memories - t189 Gratitude Visit              -0.259 5.32
    ##  t189 Recording early memories - t0 Using Signiture Strengths      6.609 4.36
    ##  t189 Recording early memories - t7 Using Signiture Strengths      1.903 4.42
    ##  t189 Recording early memories - t14 Using Signiture Strengths     2.726 4.40
    ##  t189 Recording early memories - t38 Using Signiture Strengths    -0.038 4.53
    ##  t189 Recording early memories - t98 Using Signiture Strengths    -1.391 4.69
    ##  t189 Recording early memories - t189 Using Signiture Strengths   -0.862 4.65
    ##  t189 Recording early memories - t0 Three Good Things              4.592 4.05
    ##  t189 Recording early memories - t7 Three Good Things              1.967 4.09
    ##  t189 Recording early memories - t14 Three Good Things             0.842 4.08
    ##  t189 Recording early memories - t38 Three Good Things             2.467 4.17
    ##  t189 Recording early memories - t98 Three Good Things             0.300 4.30
    ##  t189 Recording early memories - t189 Three Good Things            0.467 4.27
    ##  t189 Recording early memories - t0 Gratitude Visit                2.005 4.90
    ##  t189 Recording early memories - t7 Gratitude Visit               -0.723 4.97
    ##  t189 Recording early memories - t14 Gratitude Visit              -0.268 4.95
    ##  t189 Recording early memories - t38 Gratitude Visit              -1.359 5.13
    ##  t189 Recording early memories - t98 Gratitude Visit              -0.632 5.34
    ##  t189 Recording early memories - t189 Gratitude Visit             -1.359 5.29
    ##  t0 Using Signiture Strengths - t7 Using Signiture Strengths      -4.706 1.74
    ##  t0 Using Signiture Strengths - t14 Using Signiture Strengths     -3.882 2.07
    ##  t0 Using Signiture Strengths - t38 Using Signiture Strengths     -6.647 2.67
    ##  t0 Using Signiture Strengths - t98 Using Signiture Strengths     -8.000 2.56
    ##  t0 Using Signiture Strengths - t189 Using Signiture Strengths    -7.471 2.52
    ##  t0 Using Signiture Strengths - t0 Three Good Things              -2.017 3.94
    ##  t0 Using Signiture Strengths - t7 Three Good Things              -4.642 3.98
    ##  t0 Using Signiture Strengths - t14 Three Good Things             -5.767 3.97
    ##  t0 Using Signiture Strengths - t38 Three Good Things             -4.142 4.07
    ##  t0 Using Signiture Strengths - t98 Three Good Things             -6.309 4.20
    ##  t0 Using Signiture Strengths - t189 Three Good Things            -6.142 4.17
    ##  t0 Using Signiture Strengths - t0 Gratitude Visit                -4.604 4.81
    ##  t0 Using Signiture Strengths - t7 Gratitude Visit                -7.332 4.89
    ##  t0 Using Signiture Strengths - t14 Gratitude Visit               -6.877 4.87
    ##  t0 Using Signiture Strengths - t38 Gratitude Visit               -7.968 5.04
    ##  t0 Using Signiture Strengths - t98 Gratitude Visit               -7.241 5.26
    ##  t0 Using Signiture Strengths - t189 Gratitude Visit              -7.968 5.21
    ##  t7 Using Signiture Strengths - t14 Using Signiture Strengths      0.824 1.20
    ##  t7 Using Signiture Strengths - t38 Using Signiture Strengths     -1.941 1.68
    ##  t7 Using Signiture Strengths - t98 Using Signiture Strengths     -3.294 2.02
    ##  t7 Using Signiture Strengths - t189 Using Signiture Strengths    -2.765 2.15
    ##  t7 Using Signiture Strengths - t0 Three Good Things               2.689 4.00
    ##  t7 Using Signiture Strengths - t7 Three Good Things               0.064 4.05
    ##  t7 Using Signiture Strengths - t14 Three Good Things             -1.061 4.03
    ##  t7 Using Signiture Strengths - t38 Three Good Things              0.564 4.13
    ##  t7 Using Signiture Strengths - t98 Three Good Things             -1.603 4.26
    ##  t7 Using Signiture Strengths - t189 Three Good Things            -1.436 4.23
    ##  t7 Using Signiture Strengths - t0 Gratitude Visit                 0.102 4.86
    ##  t7 Using Signiture Strengths - t7 Gratitude Visit                -2.626 4.94
    ##  t7 Using Signiture Strengths - t14 Gratitude Visit               -2.171 4.92
    ##  t7 Using Signiture Strengths - t38 Gratitude Visit               -3.262 5.09
    ##  t7 Using Signiture Strengths - t98 Gratitude Visit               -2.535 5.31
    ##  t7 Using Signiture Strengths - t189 Gratitude Visit              -3.262 5.26
    ##  t14 Using Signiture Strengths - t38 Using Signiture Strengths    -2.765 1.64
    ##  t14 Using Signiture Strengths - t98 Using Signiture Strengths    -4.118 1.75
    ##  t14 Using Signiture Strengths - t189 Using Signiture Strengths   -3.588 2.17
    ##  t14 Using Signiture Strengths - t0 Three Good Things              1.865 3.98
    ##  t14 Using Signiture Strengths - t7 Three Good Things             -0.760 4.03
    ##  t14 Using Signiture Strengths - t14 Three Good Things            -1.885 4.02
    ##  t14 Using Signiture Strengths - t38 Three Good Things            -0.260 4.12
    ##  t14 Using Signiture Strengths - t98 Three Good Things            -2.426 4.24
    ##  t14 Using Signiture Strengths - t189 Three Good Things           -2.260 4.21
    ##  t14 Using Signiture Strengths - t0 Gratitude Visit               -0.722 4.85
    ##  t14 Using Signiture Strengths - t7 Gratitude Visit               -3.449 4.92
    ##  t14 Using Signiture Strengths - t14 Gratitude Visit              -2.995 4.90
    ##  t14 Using Signiture Strengths - t38 Gratitude Visit              -4.086 5.08
    ##  t14 Using Signiture Strengths - t98 Gratitude Visit              -3.358 5.30
    ##  t14 Using Signiture Strengths - t189 Gratitude Visit             -4.086 5.25
    ##  t38 Using Signiture Strengths - t98 Using Signiture Strengths    -1.353 2.21
    ##  t38 Using Signiture Strengths - t189 Using Signiture Strengths   -0.824 2.45
    ##  t38 Using Signiture Strengths - t0 Three Good Things              4.630 4.12
    ##  t38 Using Signiture Strengths - t7 Three Good Things              2.005 4.17
    ##  t38 Using Signiture Strengths - t14 Three Good Things             0.880 4.16
    ##  t38 Using Signiture Strengths - t38 Three Good Things             2.505 4.25
    ##  t38 Using Signiture Strengths - t98 Three Good Things             0.338 4.37
    ##  t38 Using Signiture Strengths - t189 Three Good Things            0.505 4.34
    ##  t38 Using Signiture Strengths - t0 Gratitude Visit                2.043 4.96
    ##  t38 Using Signiture Strengths - t7 Gratitude Visit               -0.684 5.04
    ##  t38 Using Signiture Strengths - t14 Gratitude Visit              -0.230 5.02
    ##  t38 Using Signiture Strengths - t38 Gratitude Visit              -1.321 5.19
    ##  t38 Using Signiture Strengths - t98 Gratitude Visit              -0.594 5.40
    ##  t38 Using Signiture Strengths - t189 Gratitude Visit             -1.321 5.35
    ##  t98 Using Signiture Strengths - t189 Using Signiture Strengths    0.529 2.17
    ##  t98 Using Signiture Strengths - t0 Three Good Things              5.983 4.30
    ##  t98 Using Signiture Strengths - t7 Three Good Things              3.358 4.34
    ##  t98 Using Signiture Strengths - t14 Three Good Things             2.233 4.33
    ##  t98 Using Signiture Strengths - t38 Three Good Things             3.858 4.42
    ##  t98 Using Signiture Strengths - t98 Three Good Things             1.691 4.54
    ##  t98 Using Signiture Strengths - t189 Three Good Things            1.858 4.51
    ##  t98 Using Signiture Strengths - t0 Gratitude Visit                3.396 5.11
    ##  t98 Using Signiture Strengths - t7 Gratitude Visit                0.668 5.18
    ##  t98 Using Signiture Strengths - t14 Gratitude Visit               1.123 5.16
    ##  t98 Using Signiture Strengths - t38 Gratitude Visit               0.032 5.33
    ##  t98 Using Signiture Strengths - t98 Gratitude Visit               0.759 5.54
    ##  t98 Using Signiture Strengths - t189 Gratitude Visit              0.032 5.49
    ##  t189 Using Signiture Strengths - t0 Three Good Things             5.453 4.26
    ##  t189 Using Signiture Strengths - t7 Three Good Things             2.828 4.30
    ##  t189 Using Signiture Strengths - t14 Three Good Things            1.703 4.29
    ##  t189 Using Signiture Strengths - t38 Three Good Things            3.328 4.38
    ##  t189 Using Signiture Strengths - t98 Three Good Things            1.162 4.50
    ##  t189 Using Signiture Strengths - t189 Three Good Things           1.328 4.47
    ##  t189 Using Signiture Strengths - t0 Gratitude Visit               2.866 5.07
    ##  t189 Using Signiture Strengths - t7 Gratitude Visit               0.139 5.15
    ##  t189 Using Signiture Strengths - t14 Gratitude Visit              0.594 5.13
    ##  t189 Using Signiture Strengths - t38 Gratitude Visit             -0.497 5.29
    ##  t189 Using Signiture Strengths - t98 Gratitude Visit              0.230 5.51
    ##  t189 Using Signiture Strengths - t189 Gratitude Visit            -0.497 5.46
    ##  t0 Three Good Things - t7 Three Good Things                      -2.625 1.47
    ##  t0 Three Good Things - t14 Three Good Things                     -3.750 1.74
    ##  t0 Three Good Things - t38 Three Good Things                     -2.125 2.25
    ##  t0 Three Good Things - t98 Three Good Things                     -4.292 2.15
    ##  t0 Three Good Things - t189 Three Good Things                    -4.125 2.12
    ##  t0 Three Good Things - t0 Gratitude Visit                        -2.587 4.53
    ##  t0 Three Good Things - t7 Gratitude Visit                        -5.314 4.61
    ##  t0 Three Good Things - t14 Gratitude Visit                       -4.860 4.59
    ##  t0 Three Good Things - t38 Gratitude Visit                       -5.951 4.77
    ##  t0 Three Good Things - t98 Gratitude Visit                       -5.223 5.01
    ##  t0 Three Good Things - t189 Gratitude Visit                      -5.951 4.95
    ##  t7 Three Good Things - t14 Three Good Things                     -1.125 1.01
    ##  t7 Three Good Things - t38 Three Good Things                      0.500 1.41
    ##  t7 Three Good Things - t98 Three Good Things                     -1.667 1.70
    ##  t7 Three Good Things - t189 Three Good Things                    -1.500 1.81
    ##  t7 Three Good Things - t0 Gratitude Visit                         0.038 4.56
    ##  t7 Three Good Things - t7 Gratitude Visit                        -2.689 4.65
    ##  t7 Three Good Things - t14 Gratitude Visit                       -2.235 4.62
    ##  t7 Three Good Things - t38 Gratitude Visit                       -3.326 4.81
    ##  t7 Three Good Things - t98 Gratitude Visit                       -2.598 5.04
    ##  t7 Three Good Things - t189 Gratitude Visit                      -3.326 4.99
    ##  t14 Three Good Things - t38 Three Good Things                     1.625 1.38
    ##  t14 Three Good Things - t98 Three Good Things                    -0.542 1.48
    ##  t14 Three Good Things - t189 Three Good Things                   -0.375 1.82
    ##  t14 Three Good Things - t0 Gratitude Visit                        1.163 4.55
    ##  t14 Three Good Things - t7 Gratitude Visit                       -1.564 4.64
    ##  t14 Three Good Things - t14 Gratitude Visit                      -1.110 4.61
    ##  t14 Three Good Things - t38 Gratitude Visit                      -2.201 4.80
    ##  t14 Three Good Things - t98 Gratitude Visit                      -1.473 5.03
    ##  t14 Three Good Things - t189 Gratitude Visit                     -2.201 4.98
    ##  t38 Three Good Things - t98 Three Good Things                    -2.167 1.86
    ##  t38 Three Good Things - t189 Three Good Things                   -2.000 2.06
    ##  t38 Three Good Things - t0 Gratitude Visit                       -0.462 4.64
    ##  t38 Three Good Things - t7 Gratitude Visit                       -3.189 4.72
    ##  t38 Three Good Things - t14 Gratitude Visit                      -2.735 4.70
    ##  t38 Three Good Things - t38 Gratitude Visit                      -3.826 4.88
    ##  t38 Three Good Things - t98 Gratitude Visit                      -3.098 5.11
    ##  t38 Three Good Things - t189 Gratitude Visit                     -3.826 5.06
    ##  t98 Three Good Things - t189 Three Good Things                    0.167 1.83
    ##  t98 Three Good Things - t0 Gratitude Visit                        1.705 4.75
    ##  t98 Three Good Things - t7 Gratitude Visit                       -1.023 4.83
    ##  t98 Three Good Things - t14 Gratitude Visit                      -0.568 4.81
    ##  t98 Three Good Things - t38 Gratitude Visit                      -1.659 4.99
    ##  t98 Three Good Things - t98 Gratitude Visit                      -0.932 5.21
    ##  t98 Three Good Things - t189 Gratitude Visit                     -1.659 5.16
    ##  t189 Three Good Things - t0 Gratitude Visit                       1.538 4.72
    ##  t189 Three Good Things - t7 Gratitude Visit                      -1.189 4.80
    ##  t189 Three Good Things - t14 Gratitude Visit                     -0.735 4.78
    ##  t189 Three Good Things - t38 Gratitude Visit                     -1.826 4.96
    ##  t189 Three Good Things - t98 Gratitude Visit                     -1.098 5.19
    ##  t189 Three Good Things - t189 Gratitude Visit                    -1.826 5.13
    ##  t0 Gratitude Visit - t7 Gratitude Visit                          -2.727 2.17
    ##  t0 Gratitude Visit - t14 Gratitude Visit                         -2.273 2.57
    ##  t0 Gratitude Visit - t38 Gratitude Visit                         -3.364 3.33
    ##  t0 Gratitude Visit - t98 Gratitude Visit                         -2.636 3.18
    ##  t0 Gratitude Visit - t189 Gratitude Visit                        -3.364 3.13
    ##  t7 Gratitude Visit - t14 Gratitude Visit                          0.455 1.49
    ##  t7 Gratitude Visit - t38 Gratitude Visit                         -0.636 2.09
    ##  t7 Gratitude Visit - t98 Gratitude Visit                          0.091 2.51
    ##  t7 Gratitude Visit - t189 Gratitude Visit                        -0.636 2.67
    ##  t14 Gratitude Visit - t38 Gratitude Visit                        -1.091 2.04
    ##  t14 Gratitude Visit - t98 Gratitude Visit                        -0.364 2.18
    ##  t14 Gratitude Visit - t189 Gratitude Visit                       -1.091 2.69
    ##  t38 Gratitude Visit - t98 Gratitude Visit                         0.727 2.75
    ##  t38 Gratitude Visit - t189 Gratitude Visit                        0.000 3.04
    ##  t98 Gratitude Visit - t189 Gratitude Visit                       -0.727 2.70
    ##  df lower.CL upper.CL t.ratio p.value
    ##  68    -8.69     3.49  -1.619  0.9935
    ##  68    -9.93     4.53  -1.415  0.9989
    ##  68   -13.15     5.55  -1.541  0.9966
    ##  68   -12.99     4.89  -1.717  0.9868
    ##  68   -11.74     5.84  -1.271  0.9998
    ##  68   -11.88    19.20   0.892  1.0000
    ##  68   -16.81    14.72  -0.252  1.0000
    ##  68   -15.93    15.48  -0.054  1.0000
    ##  68   -19.20    13.22  -0.699  1.0000
    ##  68   -21.20    12.51  -0.976  1.0000
    ##  68   -20.51    12.89  -0.865  1.0000
    ##  68   -12.62    15.90   0.436  1.0000
    ##  68   -15.42    13.45  -0.258  1.0000
    ##  68   -16.50    12.28  -0.555  1.0000
    ##  68   -15.27    14.30  -0.124  1.0000
    ##  68   -17.93    12.63  -0.657  1.0000
    ##  68   -17.65    12.68  -0.621  1.0000
    ##  68   -18.63    16.73  -0.203  1.0000
    ##  68   -21.66    14.32  -0.774  1.0000
    ##  68   -21.13    14.69  -0.681  1.0000
    ##  68   -22.90    14.29  -0.878  1.0000
    ##  68   -23.04    15.88  -0.698  1.0000
    ##  68   -23.56    14.94  -0.849  1.0000
    ##  68    -4.30     4.10  -0.090  1.0000
    ##  68    -7.06     4.66  -0.776  1.0000
    ##  68    -8.51     5.61  -0.778  1.0000
    ##  68    -7.85     7.15  -0.177  1.0000
    ##  68    -9.47    21.99   1.508  0.9974
    ##  68   -14.40    17.51   0.369  1.0000
    ##  68   -13.52    18.27   0.567  1.0000
    ##  68   -16.79    16.01  -0.090  1.0000
    ##  68   -18.77    15.29  -0.387  1.0000
    ##  68   -18.09    15.67  -0.272  1.0000
    ##  68   -10.23    18.71   1.111  1.0000
    ##  68   -13.03    16.26   0.418  1.0000
    ##  68   -14.11    15.09   0.128  1.0000
    ##  68   -12.87    17.10   0.535  1.0000
    ##  68   -15.53    15.43  -0.012  1.0000
    ##  68   -15.24    15.48   0.029  1.0000
    ##  68   -16.20    19.51   0.351  1.0000
    ##  68   -19.23    17.09  -0.224  1.0000
    ##  68   -18.69    17.46  -0.130  1.0000
    ##  68   -20.47    17.05  -0.345  1.0000
    ##  68   -20.59    18.63  -0.190  1.0000
    ##  68   -21.11    17.70  -0.334  1.0000
    ##  68    -6.82     4.62  -0.728  1.0000
    ##  68    -7.47     4.77  -0.836  1.0000
    ##  68    -7.83     7.33  -0.125  1.0000
    ##  68    -9.32    22.04   1.537  0.9967
    ##  68   -14.25    17.56   0.394  1.0000
    ##  68   -13.37    18.32   0.592  1.0000
    ##  68   -16.64    16.06  -0.067  1.0000
    ##  68   -18.63    15.35  -0.366  1.0000
    ##  68   -17.95    15.72  -0.250  1.0000
    ##  68   -10.07    18.76   1.142  1.0000
    ##  68   -12.87    16.31   0.446  1.0000
    ##  68   -13.95    15.13   0.154  1.0000
    ##  68   -12.72    17.15   0.563  1.0000
    ##  68   -15.38    15.48   0.012  1.0000
    ##  68   -15.09    15.53   0.054  1.0000
    ##  68   -16.05    19.56   0.374  1.0000
    ##  68   -19.09    17.14  -0.204  1.0000
    ##  68   -18.55    17.51  -0.109  1.0000
    ##  68   -20.32    17.10  -0.326  1.0000
    ##  68   -20.45    18.69  -0.171  1.0000
    ##  68   -20.97    17.75  -0.315  1.0000
    ##  68    -7.98     7.48  -0.123  1.0000
    ##  68    -7.71     9.41   0.376  1.0000
    ##  68    -8.65    23.57   1.754  0.9830
    ##  68   -13.58    19.09   0.639  1.0000
    ##  68   -12.70    19.85   0.833  1.0000
    ##  68   -15.96    17.58   0.184  1.0000
    ##  68   -17.93    16.85  -0.118  1.0000
    ##  68   -17.25    17.23  -0.003  1.0000
    ##  68    -9.45    20.33   1.386  0.9992
    ##  68   -12.24    17.87   0.709  1.0000
    ##  68   -13.32    16.70   0.427  1.0000
    ##  68   -12.07    18.70   0.817  1.0000
    ##  68   -14.72    17.02   0.275  1.0000
    ##  68   -14.44    17.07   0.317  1.0000
    ##  68   -15.34    21.04   0.595  1.0000
    ##  68   -18.36    18.62   0.026  1.0000
    ##  68   -17.83    18.99   0.120  1.0000
    ##  68   -19.59    18.57  -0.101  1.0000
    ##  68   -19.70    20.14   0.042  1.0000
    ##  68   -20.23    19.21  -0.098  1.0000
    ##  68    -6.48     8.68   0.550  1.0000
    ##  68    -8.96    24.37   1.753  0.9831
    ##  68   -13.87    19.88   0.674  1.0000
    ##  68   -12.99    20.65   0.862  1.0000
    ##  68   -16.23    18.36   0.233  1.0000
    ##  68   -18.19    17.61  -0.062  1.0000
    ##  68   -17.52    17.99   0.051  1.0000
    ##  68    -9.79    21.17   1.394  0.9992
    ##  68   -12.58    18.71   0.743  1.0000
    ##  68   -13.66    17.54   0.472  1.0000
    ##  68   -12.40    19.53   0.847  1.0000
    ##  68   -15.03    17.83   0.323  1.0000
    ##  68   -14.75    17.88   0.364  1.0000
    ##  68   -15.57    21.78   0.630  1.0000
    ##  68   -18.59    19.35   0.075  1.0000
    ##  68   -18.06    19.73   0.167  1.0000
    ##  68   -19.80    19.29  -0.050  1.0000
    ##  68   -19.90    20.84   0.087  1.0000
    ##  68   -20.43    19.91  -0.049  1.0000
    ##  68    -9.92    23.14   1.515  0.9972
    ##  68   -14.84    18.65   0.431  1.0000
    ##  68   -13.96    19.41   0.619  1.0000
    ##  68   -17.21    17.13  -0.008  1.0000
    ##  68   -19.17    16.38  -0.297  1.0000
    ##  68   -18.49    16.77  -0.185  1.0000
    ##  68   -10.74    19.93   1.135  1.0000
    ##  68   -13.53    17.47   0.481  1.0000
    ##  68   -14.62    16.30   0.206  1.0000
    ##  68   -13.36    18.29   0.591  1.0000
    ##  68   -15.99    16.59   0.070  1.0000
    ##  68   -15.71    16.65   0.109  1.0000
    ##  68   -16.55    20.56   0.409  1.0000
    ##  68   -19.58    18.13  -0.145  1.0000
    ##  68   -19.04    18.51  -0.054  1.0000
    ##  68   -20.79    18.07  -0.265  1.0000
    ##  68   -20.89    19.63  -0.118  1.0000
    ##  68   -21.42    18.70  -0.257  1.0000
    ##  68   -11.31     1.90  -2.701  0.5259
    ##  68   -11.72     3.96  -1.876  0.9650
    ##  68   -16.79     3.49  -2.485  0.6824
    ##  68   -17.70     1.70  -3.126  0.2535
    ##  68   -17.01     2.07  -2.968  0.3433
    ##  68   -16.95    12.91  -0.512  1.0000
    ##  68   -19.74    10.46  -1.165  0.9999
    ##  68   -20.82     9.29  -1.452  0.9985
    ##  68   -19.57    11.29  -1.017  1.0000
    ##  68   -22.22     9.60  -1.503  0.9975
    ##  68   -21.94     9.65  -1.474  0.9981
    ##  68   -22.83    13.62  -0.958  1.0000
    ##  68   -25.86    11.19  -1.500  0.9976
    ##  68   -25.32    11.57  -1.413  0.9990
    ##  68   -27.08    11.15  -1.580  0.9952
    ##  68   -27.19    12.71  -1.375  0.9993
    ##  68   -27.72    11.78  -1.529  0.9969
    ##  68    -3.73     5.38   0.685  1.0000
    ##  68    -8.30     4.42  -1.157  1.0000
    ##  68   -10.95     4.36  -1.630  0.9929
    ##  68   -10.90     5.37  -1.288  0.9997
    ##  68   -12.48    17.86   0.672  1.0000
    ##  68   -15.27    15.40   0.016  1.0000
    ##  68   -16.35    14.23  -0.263  1.0000
    ##  68   -15.10    16.23   0.136  1.0000
    ##  68   -17.74    14.53  -0.377  1.0000
    ##  68   -17.46    14.58  -0.340  1.0000
    ##  68   -18.32    18.52   0.021  1.0000
    ##  68   -21.34    16.09  -0.532  1.0000
    ##  68   -20.81    16.47  -0.442  1.0000
    ##  68   -22.56    16.04  -0.641  1.0000
    ##  68   -22.67    17.60  -0.477  1.0000
    ##  68   -23.19    16.67  -0.620  1.0000
    ##  68    -8.97     3.44  -1.688  0.9891
    ##  68   -10.76     2.52  -2.349  0.7724
    ##  68   -11.80     4.63  -1.655  0.9914
    ##  68   -13.24    16.97   0.468  1.0000
    ##  68   -16.03    14.51  -0.189  1.0000
    ##  68   -17.11    13.34  -0.469  1.0000
    ##  68   -15.86    15.34  -0.063  1.0000
    ##  68   -18.50    13.65  -0.572  1.0000
    ##  68   -18.22    13.70  -0.537  1.0000
    ##  68   -19.09    17.65  -0.149  1.0000
    ##  68   -22.11    15.22  -0.700  1.0000
    ##  68   -21.58    15.59  -0.611  1.0000
    ##  68   -23.34    15.16  -0.804  1.0000
    ##  68   -23.44    16.73  -0.634  1.0000
    ##  68   -23.97    15.80  -0.779  1.0000
    ##  68    -9.73     7.03  -0.612  1.0000
    ##  68   -10.10     8.46  -0.336  1.0000
    ##  68   -11.00    20.26   1.122  1.0000
    ##  68   -13.79    17.80   0.481  1.0000
    ##  68   -14.87    16.63   0.212  1.0000
    ##  68   -13.61    18.62   0.589  1.0000
    ##  68   -16.23    16.91   0.077  1.0000
    ##  68   -15.96    16.96   0.116  1.0000
    ##  68   -16.76    20.85   0.412  1.0000
    ##  68   -19.78    18.41  -0.136  1.0000
    ##  68   -19.25    18.79  -0.046  1.0000
    ##  68   -20.99    18.35  -0.255  1.0000
    ##  68   -21.08    19.89  -0.110  1.0000
    ##  68   -21.61    18.97  -0.247  1.0000
    ##  68    -7.69     8.75   0.244  1.0000
    ##  68   -10.31    22.28   1.391  0.9992
    ##  68   -13.09    19.81   0.774  1.0000
    ##  68   -14.18    18.64   0.516  1.0000
    ##  68   -12.90    20.61   0.873  1.0000
    ##  68   -15.51    18.89   0.373  1.0000
    ##  68   -15.23    18.95   0.412  1.0000
    ##  68   -15.97    22.76   0.665  1.0000
    ##  68   -18.98    20.31   0.129  1.0000
    ##  68   -18.45    20.69   0.218  1.0000
    ##  68   -20.17    20.23   0.006  1.0000
    ##  68   -20.24    21.75   0.137  1.0000
    ##  68   -20.77    20.84   0.006  1.0000
    ##  68   -10.68    21.59   1.281  0.9998
    ##  68   -13.46    19.12   0.658  1.0000
    ##  68   -14.55    17.95   0.397  1.0000
    ##  68   -13.27    19.93   0.760  1.0000
    ##  68   -15.89    18.21   0.258  1.0000
    ##  68   -15.61    18.27   0.297  1.0000
    ##  68   -16.36    22.09   0.565  1.0000
    ##  68   -19.37    19.65   0.027  1.0000
    ##  68   -18.84    20.03   0.116  1.0000
    ##  68   -20.57    19.57  -0.094  1.0000
    ##  68   -20.64    21.10   0.042  1.0000
    ##  68   -21.18    20.18  -0.091  1.0000
    ##  68    -8.18     2.93  -1.790  0.9787
    ##  68   -10.35     2.85  -2.154  0.8779
    ##  68   -10.66     6.41  -0.944  1.0000
    ##  68   -12.46     3.87  -1.993  0.9372
    ##  68   -12.15     3.90  -1.947  0.9494
    ##  68   -19.74    14.56  -0.572  1.0000
    ##  68   -22.78    12.15  -1.153  1.0000
    ##  68   -22.24    12.52  -1.060  1.0000
    ##  68   -24.04    12.14  -1.247  0.9998
    ##  68   -24.20    13.75  -1.043  1.0000
    ##  68   -24.71    12.81  -1.202  0.9999
    ##  68    -4.96     2.71  -1.112  1.0000
    ##  68    -4.85     5.85   0.354  1.0000
    ##  68    -8.11     4.78  -0.980  1.0000
    ##  68    -8.35     5.35  -0.830  1.0000
    ##  68   -17.26    17.33   0.008  1.0000
    ##  68   -20.30    14.92  -0.579  1.0000
    ##  68   -19.76    15.29  -0.483  1.0000
    ##  68   -21.56    14.90  -0.692  1.0000
    ##  68   -21.71    16.51  -0.515  1.0000
    ##  68   -22.22    15.57  -0.667  1.0000
    ##  68    -3.60     6.85   1.179  0.9999
    ##  68    -6.13     5.05  -0.367  1.0000
    ##  68    -7.29     6.54  -0.206  1.0000
    ##  68   -16.09    18.42   0.255  1.0000
    ##  68   -19.14    16.01  -0.337  1.0000
    ##  68   -18.60    16.38  -0.240  1.0000
    ##  68   -20.39    15.99  -0.458  1.0000
    ##  68   -20.55    17.60  -0.293  1.0000
    ##  68   -21.06    16.66  -0.442  1.0000
    ##  68    -9.22     4.89  -1.164  0.9999
    ##  68    -9.81     5.81  -0.971  1.0000
    ##  68   -18.05    17.12  -0.100  1.0000
    ##  68   -21.09    14.71  -0.675  1.0000
    ##  68   -20.55    15.08  -0.582  1.0000
    ##  68   -22.33    14.68  -0.784  1.0000
    ##  68   -22.47    16.27  -0.606  1.0000
    ##  68   -22.99    15.34  -0.757  1.0000
    ##  68    -6.75     7.09   0.091  1.0000
    ##  68   -16.30    19.71   0.359  1.0000
    ##  68   -19.34    17.29  -0.212  1.0000
    ##  68   -18.80    17.66  -0.118  1.0000
    ##  68   -20.57    17.25  -0.333  1.0000
    ##  68   -20.69    18.82  -0.179  1.0000
    ##  68   -21.21    17.89  -0.322  1.0000
    ##  68   -16.37    19.45   0.326  1.0000
    ##  68   -19.40    17.02  -0.248  1.0000
    ##  68   -18.87    17.40  -0.154  1.0000
    ##  68   -20.64    16.98  -0.368  1.0000
    ##  68   -20.76    18.56  -0.212  1.0000
    ##  68   -21.28    17.63  -0.356  1.0000
    ##  68   -10.94     5.48  -1.259  0.9998
    ##  68   -12.02     7.48  -0.884  1.0000
    ##  68   -15.97     9.24  -1.012  1.0000
    ##  68   -14.69     9.42  -0.829  1.0000
    ##  68   -15.22     8.49  -1.075  1.0000
    ##  68    -5.21     6.12   0.304  1.0000
    ##  68    -8.54     7.27  -0.305  1.0000
    ##  68    -9.43     9.61   0.036  1.0000
    ##  68   -10.75     9.48  -0.238  1.0000
    ##  68    -8.81     6.63  -0.536  1.0000
    ##  68    -8.62     7.89  -0.167  1.0000
    ##  68   -11.30     9.12  -0.405  1.0000
    ##  68    -9.69    11.14   0.265  1.0000
    ##  68   -11.54    11.54   0.000  1.0000
    ##  68   -10.95     9.50  -0.270  1.0000
    ## 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: tukey method for comparing a family of 24 estimates 
    ## P value adjustment: tukey method for comparing a family of 24 estimates
