Paper Replication of Hypothesis Testing on Web-based Positive Psychology
Interventions
================
2023-05-03

Paper: Web-Based Positive Psychology Interventions: A Reexamination of
Effectiveness

References:

1.  Woodworth, R. J., O’Brien-Malone, A., Diamond, M. R. and Schüz, B.
    (2017). Web-based positive psychology interventions: A reexamination
    of effectiveness. Journal of Clinical Psychology, 73(3), 218–232,
    DOI: <https://doi.org/10.1002/jclp.22328>
2.  Woodworth, R. J., O’Brien-Malone, A., Diamond, M. R. and Schüz, B.
    (2018). Data from, ‘Web-based Positive Psychology Interventions: A
    Reexaminationof Effectiveness’. Journal of Open Psychology Data,
    DOI: <https://doi.org/10.5334.jopd.35>

Goal: Replicate various hypothesis testing methods for no difference
between participants who completed all assessments vs participants who
did not complete all assessments.

Intervention Groups:

- 1 “Using Signature Strengths”

- 2 “Three Good Things”

- 3 “Gratitude Visit”

- 4 “Recording early memories” (control condition)

# Load and Clean Data

## Creating New Variable: Status

- Before testing, I need to load the given datafile and create a new
  ordinal variable of status where a participant is either a ‘dropout’
  or ‘non-dropout’. Dummy variables can also be used afterwards.

Load the data *ahi-cesd.csv* and *participant_info.csv*

``` r
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
library(sjPlot)

library(lsr)
library(psych)
```

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(ggplot2)

# Disable scientific notation:
options(scipen=999, digits=4)

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
participant_info <- read_csv("data/participant-info.csv")
```

    ## Rows: 295 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): id, intervention, sex, age, educ, income
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#View Files
head(ahi_cesd)
```

    ## # A tibble: 6 × 50
    ##      id occasion elaps…¹ inter…² ahi01 ahi02 ahi03 ahi04 ahi05 ahi06 ahi07 ahi08
    ##   <dbl>    <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1     1        0    0          4     2     3     2     3     3     2     3     3
    ## 2     1        1   11.8        4     3     3     4     3     3     4     4     3
    ## 3     2        0    0          1     3     4     3     4     2     3     4     3
    ## 4     2        1    8.02       1     3     4     4     4     3     3     4     4
    ## 5     2        2   14.3        1     3     4     4     4     3     3     4     3
    ## 6     2        3   32.0        1     3     4     4     4     4     4     4     3
    ## # … with 38 more variables: ahi09 <dbl>, ahi10 <dbl>, ahi11 <dbl>, ahi12 <dbl>,
    ## #   ahi13 <dbl>, ahi14 <dbl>, ahi15 <dbl>, ahi16 <dbl>, ahi17 <dbl>,
    ## #   ahi18 <dbl>, ahi19 <dbl>, ahi20 <dbl>, ahi21 <dbl>, ahi22 <dbl>,
    ## #   ahi23 <dbl>, ahi24 <dbl>, cesd01 <dbl>, cesd02 <dbl>, cesd03 <dbl>,
    ## #   cesd04 <dbl>, cesd05 <dbl>, cesd06 <dbl>, cesd07 <dbl>, cesd08 <dbl>,
    ## #   cesd09 <dbl>, cesd10 <dbl>, cesd11 <dbl>, cesd12 <dbl>, cesd13 <dbl>,
    ## #   cesd14 <dbl>, cesd15 <dbl>, cesd16 <dbl>, cesd17 <dbl>, cesd18 <dbl>, …

``` r
head(participant_info)
```

    ## # A tibble: 6 × 6
    ##      id intervention   sex   age  educ income
    ##   <dbl>        <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1     1            4     2    35     5      3
    ## 2     2            1     1    59     1      1
    ## 3     3            4     1    51     4      3
    ## 4     4            3     1    50     5      2
    ## 5     5            2     2    58     5      2
    ## 6     6            1     1    31     5      1

Using ahi_cesd for all known AHi and CESD scores, creating a new
dataframe to perform functions that will help identify each unique
participant’s status (dropout or non-dropout).

- Non-dropout defined as a participant with AHI and CESD scores provided
  in all 6 occasions.

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
```

Some pre-checks: Participants who completed at least the
post-test(occasion == 1)

``` r
# COMPLETED AT LEAST THE POSTTEST
completed_posttest <- c()

for (i in 1:length(h)) {
  #access key
  k <- keys( h[as.character(i)] )
  
  #access values
  v <- values(h[k],USE.NAMES=FALSE, simplify=FALSE)
  v <- v[[1]]
  
  #condition for any participant that completed the posttest (1)
  if (any(v %in% c(1))) {
    completed_posttest = append(completed_posttest,i)
  }
}
```

Now we have the correct counts of participants who completed all
assessments, and an additional check of participants who at least
completed the post-test(Occasion = 1).

``` r
length(completed_posttest)
```

    ## [1] 147

``` r
length(completed)
```

    ## [1] 72

Great! Now that we have created vectors of participant’s id under out
conditions of completed or not, we can add a new column of dropout or
non.dropout to the *participant_info* dataframe.

``` r
# ADD NEW COLUMN OF DROPOUT AND NON.DROPOUT TO PARTICIPANTS 

p.dat <- data.frame(participant_info)

p.dat <- participant_info %>%
  mutate(status = ifelse(id %in% completed, '1', '0'))

#pre_test_dat contains only occurences of Occasion = 0
pre_test_dat <- ahi_cesd %>%
  filter(occasion == 0)

#add column for 
p.dat <- left_join(p.dat, pre_test_dat, by='id')

#select columns needed for the df
p.dat <- p.dat %>%
  select(id, intervention = intervention.x, sex, age, educ, income, status, ahiTotal, cesdTotal)

#ensure columns are the correct data type
p.dat$sex <- as.numeric(p.dat$sex)

head(p.dat)
```

    ## # A tibble: 6 × 9
    ##      id intervention   sex   age  educ income status ahiTotal cesdTotal
    ##   <dbl>        <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>     <dbl>     <dbl>
    ## 1     1            4     2    35     5      3 0            63        14
    ## 2     2            1     1    59     1      1 1            73         7
    ## 3     3            4     1    51     4      3 0            77         3
    ## 4     4            3     1    50     5      2 0            60        31
    ## 5     5            2     2    58     5      2 0            41        27
    ## 6     6            1     1    31     5      1 0            62        25

``` r
# ADD NEW COLUMN OF DROPOUT AND NON.DROPOUT TO AHI_CESD DF 

initial.scores.dat <- ahi_cesd %>%
  filter(occasion == 0) %>%
  mutate(status = ifelse(id %in% completed, '1', '0')) %>%
  select(id, status, occasion, elapsed.days,intervention, ahiTotal, cesdTotal)

head(initial.scores.dat)
```

    ## # A tibble: 6 × 7
    ##      id status occasion elapsed.days intervention ahiTotal cesdTotal
    ##   <dbl> <chr>     <dbl>        <dbl>        <dbl>    <dbl>     <dbl>
    ## 1     1 0             0            0            4       63        14
    ## 2     2 1             0            0            1       73         7
    ## 3     3 0             0            0            4       77         3
    ## 4     4 0             0            0            3       60        31
    ## 5     5 0             0            0            2       41        27
    ## 6     6 0             0            0            1       62        25

Done! This table will be the main df to work on various hypothesis
testing to see if there is a difference between participants that
completed all assessments and participants that did not complete all
assessments. This precaution is done so to move forward with the paper’s
intention-to-treat experiment to test ANOVA on the effects of 3
different positive psychology interventions and a control(placebo) over
time.

# T-Test on Initial Happiness and Depression Scores

## Initial Happiness Scores

AHI Scores with t.test

H_0: No difference between AHI scores of dropout and non.dropouts

Checking data for violations of Assumptions of T-Test - relatively equal
group sizes - relatively equal variances - interval or continuous
variable is normally distributed

Paper: initial happiness scores, t(293) = 0.26, p = .79

``` r
#Check equal variances for T-Test
var.test(ahiTotal ~ status, data = p.dat)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  ahiTotal by status
    ## F = 1.2, num df = 222, denom df = 71, p-value = 0.4
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.808 1.729
    ## sample estimates:
    ## ratio of variances 
    ##              1.204

``` r
# Shapiro-Wilk normality test for NonDropout's ahiTotal
with(p.dat, shapiro.test(ahiTotal[status == "1"]))# p = 0.7
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  ahiTotal[status == "1"]
    ## W = 0.99, p-value = 0.7

``` r
# Shapiro-Wilk normality test for Dropout's ahiTotal
with(p.dat, shapiro.test(ahiTotal[status == "0"])) # p = 0.9
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  ahiTotal[status == "0"]
    ## W = 1, p-value = 0.9

Both tests of equal variance and Shapiro-Wilk normality test provided
p-values that were not statistically significant to reject the null
hypothesis. continuing the t-test.

``` r
res.ahiTotal <- t.test(
  ahiTotal~status,
  data = p.dat,
  var.equal=TRUE,
  conf.level = 0.95,
  alternative = "two.sided"
)
res.ahiTotal
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  ahiTotal by status
    ## t = -0.47, df = 293, p-value = 0.6
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.348  2.686
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##           69.50           70.33

``` r
res.ahiTotal$p.value
```

    ## [1] 0.6423

``` r
ggstatsplot::ggbetweenstats(data = p.dat, x = status, y = ahiTotal,
                            var.equal = TRUE,
                            effsize.type = "d", mean.ci = TRUE,
                            pairwise.comparisons = FALSE, 
                            messages = FALSE, bf.message = FALSE,
                            title = "Initial Happiness Scores", xlab = "Dropout vs Non-Dropout", ylab = "AHI Score")
```

![](PPI_hypothesis_testing_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Initial AHI scores between dropout and non.dropout groups do not show
statistically significant results (p = 0.64) , and we retain the null
hypothesis that there is no difference.**

## Initial Depression Scores

CESD Scores with t.test

H_0: No difference between AHI scores of dropout and non.dropouts

Checking data for violations of Assumptions of T-Test - relatively equal
group sizes - relatively equal variances - interval or continuous
variable is normally distributed

Paper: initial depression scores t(293) = 0.44, p = .66

``` r
#Check equal variances for T-Test
var.test(cesdTotal ~ status, data = p.dat) # p = 0.4
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  cesdTotal by status
    ## F = 1.2, num df = 222, denom df = 71, p-value = 0.4
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.7964 1.7041
    ## sample estimates:
    ## ratio of variances 
    ##              1.187

``` r
# Shapiro-Wilk normality test for NonDropout's ahiTotal
with(p.dat, shapiro.test(cesdTotal[status == "1"]))# p = 0.0003
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  cesdTotal[status == "1"]
    ## W = 0.92, p-value = 0.0003

``` r
# Shapiro-Wilk normality test for Dropout's ahiTotal
with(p.dat, shapiro.test(cesdTotal[status == "0"])) # p = 0.000000007
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  cesdTotal[status == "0"]
    ## W = 0.93, p-value = 0.000000007

``` r
#Since normality test failed, a wilcoxon test serves as another test of differences
wilcox.test(cesdTotal ~ status, data = p.dat, alternative = "two.sided")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  cesdTotal by status
    ## W = 8836, p-value = 0.2
    ## alternative hypothesis: true location shift is not equal to 0

``` r
# Initial Depression Scores
#Paper's initial depression scores t(293) = 0.44, p = .66

res.cesdTotal <- t.test(
  cesdTotal ~ status,
  data = p.dat,
  var.equal=TRUE, 
  conf.level = 0.95,
  alternative="two.sided"
)

res.cesdTotal
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  cesdTotal by status
    ## t = 1.3, df = 293, p-value = 0.2
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9538  4.7989
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##           15.53           13.61

``` r
ggstatsplot::ggbetweenstats(data = p.dat, x = status, y = cesdTotal,
                            var.equal = TRUE,
                            effsize.type = "d", mean.ci = TRUE,
                            pairwise.comparisons = FALSE, 
                            messages = FALSE, bf.message = FALSE,
                            title = "Initial Depression Scores", xlab = "Dropout vs Non-Dropout", ylab = "CESD Score")
```

![](PPI_hypothesis_testing_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Hypothesis Testing on Demographic Variables

## Difference in Sex with Chi-Square Test

Using Chi-Square Test to see comparison of female and male samples.

Hypothesis: There is no difference between the sample of male and female
among dropout and non.dropout participants.

``` r
#H_0: 
#  P_160 = P61 (same probability of male)
#  P_33 = P_11 (same probability of female)

# Create crosstabulations
sexfreq <- xtabs(formula = ~sex + status, data = p.dat)

chisq.test(sexfreq, correct = FALSE)    # X-squared = 0.01, df = 1, p-value = 0.92
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  sexfreq
    ## X-squared = 0.0099, df = 1, p-value = 0.9

``` r
# correct = FALSE. logical indicating whether to apply continuity correction when computing test statistic for 2x2 tables. 
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Differences in Sex
</caption>
<tr>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal; border-bottom:1px solid;" rowspan="2">
sex
</th>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal;" colspan="2">
status
</th>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal; font-weight:bolder; font-style:italic; border-bottom:1px solid; " rowspan="2">
Total
</th>
</tr>
<tr>
<td style="border-bottom:1px solid; text-align:center; padding:0.2cm;">
0
</td>
<td style="border-bottom:1px solid; text-align:center; padding:0.2cm;">
1
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
1
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">190</span><br><span
style="color:#333399;">75.7 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">61</span><br><span
style="color:#333399;">24.3 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">251</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
2
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">33</span><br><span
style="color:#333399;">75 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">11</span><br><span
style="color:#333399;">25 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">44</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  border-bottom:double; font-weight:bolder; font-style:italic; text-align:left; vertical-align:middle;">
Total
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">223</span><br><span
style="color:#333399;">75.6 %</span>
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">72</span><br><span
style="color:#333399;">24.4 %</span>
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">295</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<td style="text-align:right; font-size:0.9em; font-style:italic; padding:0.2cm;" colspan="4">
χ<sup>2</sup>=0.000 · df=1 · φ=0.006 · p=1.000
</td>
</tr>
</table>

## Intervention Groups with Chi-Square Test

X-squared(3, n = 295) = 6.15, p = 0.11

H_0: No difference between intervention groups of dropout and
non.dropouts

Issues: not getting the p = 0.11….could be a round up of a round up….

``` r
intervention.freq <- xtabs(formula = ~intervention+status, data = p.dat)
chisq.test(intervention.freq, ) # X-squared = 6.143, p = 0.1046
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  intervention.freq
    ## X-squared = 6.1, df = 3, p-value = 0.1

``` r
sjPlot::tab_xtab(
  var.row = p.dat$intervention, 
  var.col = p.dat$status, 
  statistics = "cramer",
  title = "Differences in Intervention", 
  show.exp = TRUE,
  show.row.prc = TRUE)
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Differences in Intervention
</caption>
<tr>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal; border-bottom:1px solid;" rowspan="2">
intervention
</th>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal;" colspan="2">
status
</th>
<th style="border-top:double; text-align:center; font-style:italic; font-weight:normal; font-weight:bolder; font-style:italic; border-bottom:1px solid; " rowspan="2">
Total
</th>
</tr>
<tr>
<td style="border-bottom:1px solid; text-align:center; padding:0.2cm;">
0
</td>
<td style="border-bottom:1px solid; text-align:center; padding:0.2cm;">
1
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
1
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">55</span><br><span
style="color:#339999;">54</span><br><span
style="color:#333399;">76.4 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">17</span><br><span
style="color:#339999;">18</span><br><span
style="color:#333399;">23.6 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">72</span><br><span
style="color:#339999;">72</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
2
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">52</span><br><span
style="color:#339999;">57</span><br><span
style="color:#333399;">68.4 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">24</span><br><span
style="color:#339999;">19</span><br><span
style="color:#333399;">31.6 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">76</span><br><span
style="color:#339999;">76</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
3
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">63</span><br><span
style="color:#339999;">56</span><br><span
style="color:#333399;">85.1 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">11</span><br><span
style="color:#339999;">18</span><br><span
style="color:#333399;">14.9 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">74</span><br><span
style="color:#339999;">74</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  text-align:left; vertical-align:middle;">
4
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">53</span><br><span
style="color:#339999;">55</span><br><span
style="color:#333399;">72.6 %</span>
</td>
<td style="padding:0.2cm; text-align:center; ">
<span style="color:black;">20</span><br><span
style="color:#339999;">18</span><br><span
style="color:#333399;">27.4 %</span>
</td>
<td style="padding:0.2cm; text-align:center;  ">
<span style="color:black;">73</span><br><span
style="color:#339999;">73</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<tr>
<td style="padding:0.2cm;  border-bottom:double; font-weight:bolder; font-style:italic; text-align:left; vertical-align:middle;">
Total
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">223</span><br><span
style="color:#339999;">223</span><br><span
style="color:#333399;">75.6 %</span>
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">72</span><br><span
style="color:#339999;">72</span><br><span
style="color:#333399;">24.4 %</span>
</td>
<td style="padding:0.2cm; text-align:center;   border-bottom:double;">
<span style="color:black;">295</span><br><span
style="color:#339999;">295</span><br><span
style="color:#333399;">100 %</span>
</td>
</tr>
<td style="text-align:right; font-size:0.9em; font-style:italic; padding:0.2cm;" colspan="4">
χ<sup>2</sup>=6.149 · df=3 · Cramer’s V=0.144 · p=0.105
</td>
</tr>
</table>

## Difference in Education Level with Wilcoxon Test

Wilcoxon W = 8154, p =.83

H_0: No difference between intervention groups of dropout and
non.dropouts

``` r
wilcox.test(x = p.dat$educ[p.dat$status == '1'], 
            y = p.dat$educ[p.dat$status == '0'],
            alternative = "two.sided")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  p.dat$educ[p.dat$status == "1"] and p.dat$educ[p.dat$status == "0"]
    ## W = 8154, p-value = 0.8
    ## alternative hypothesis: true location shift is not equal to 0

## Difference in Income Level with Wilcoxon Test

Income with wilcoxon test Wilcoxon W = 8552, p = .37

H_0: No difference between intervention groups of dropout and
non.dropouts

``` r
wilcox.test(x = p.dat$income[p.dat$status == '1'], 
            y = p.dat$income[p.dat$status == '0'],
            alternative = "two.sided")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  p.dat$income[p.dat$status == "1"] and p.dat$income[p.dat$status == "0"]
    ## W = 8552, p-value = 0.4
    ## alternative hypothesis: true location shift is not equal to 0

## Difference in Age with T-Test

H_0: No difference between intervention groups of dropout and
non.dropouts

Checking data for violations of Assumptions of T-Test - relatively equal
group sizes - relatively equal variances - interval or continuous
variable is normally distributed

> Dropouts were on average 5.4 years younger than non-dropouts (M = 42.5
> vs. 47.8), Welch t(126.1) = 3.28, p = .001.

``` r
#Test of equal variance
var.test(age ~ status, data = p.dat)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  age by status
    ## F = 1.1, num df = 222, denom df = 71, p-value = 0.6
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.7474 1.5992
    ## sample estimates:
    ## ratio of variances 
    ##              1.114

``` r
t.test(
  age ~ status, 
  data = p.dat,
  var.equal=FALSE, 
  conf.level = 0.95
)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  age by status
    ## t = -3.3, df = 126, p-value = 0.001
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -8.490 -2.105
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##           42.47           47.76
