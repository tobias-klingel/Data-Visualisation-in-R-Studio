Titanic Survival Visualisation
================

# Background

The Titanic was a British ocean liner that sunk in 1912 during traveling
from the United Kingdom to New York City. More than 1,500 of the
estimated 2,224 passengers and crew died in the accident where the
titanc struck an iceberg. This notebook visualize the real data to get
an inside of the surival of the passangers.

# Demographics of Titanic Passengers

``` r
options(digits = 3)    # report 3 significant digits
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::combine() masks gridExtra::combine()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

``` r
library(titanic)
library(gridExtra)


titanic <- titanic_train %>%select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
      mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex))
#titanic
p1 <- titanic %>% ggplot(aes(Age, color=Sex)) + geom_density(alpha = 0.01, position = "stack") + 
  scale_x_continuous(trans='log2') +facet_grid(.~Sex)

p2 <- titanic %>% ggplot(aes(Age, color=Sex)) + geom_density(alpha = 0.01, position = "stack") + 
  scale_x_continuous(trans='log2')

p3 <- titanic %>% ggplot(aes(Sex, Age)) + geom_point()

###############
#Extracting data
number_female <-titanic %>% filter(Sex=="female")
number_male <-titanic %>% filter(Sex=="male")

number_female_Age40 <-titanic %>% filter(Sex=="female" & Age ==40)
number_male_Age40<-titanic %>% filter(Sex=="male"& Age ==40)

number_female_below17 <-titanic %>% filter(Sex=="female" & Age <17)
number_male_below17<-titanic %>% filter(Sex=="male"& Age <17)

number_female_between_18_35 <-titanic %>% filter(Sex=="female" & Age >18 & Age<35)
number_male_between_18_35<-titanic %>% filter(Sex=="male"& Age >18 & Age<35)

##############
#Visualize data
p4 <- titanic %>% ggplot(aes(Age, fill=Sex)) + geom_density(alpha = 0.2, position = "stack") 

grid.arrange(p1, p2, p3, p4,  nrow = 2)
```

![](Titanic-Survival_files/figure-gfm/Demographics%20of%20Titanic%20Passengers-1.png)<!-- -->

``` r
p3o <- titanic %>%ggplot(aes(Age, fill = Sex)) +geom_density(alpha = 0.2)
grid.arrange( p3o, ncol = 1)
```

![](Titanic-Survival_files/figure-gfm/Demographics%20of%20Titanic%20Passengers-2.png)<!-- -->

## Result:

  - Females and males had a similar shape of age distribution.
  - The age distribution was bimodal, with one mode around 25 years of
    age and a second smaller mode around 5 years of age.
  - The number of males of age 40 was higher than the number of females
    of age 40.
  - The proportion of females age 18-35 was lower the the proportion of
    males age 18-35.

# QQ-plot of Age Distribution with identity line

``` r
params <- titanic %>%
    filter(!is.na(Age)) %>%
    summarize(mean = mean(Age), sd = sd(Age))

titanic %>% ggplot(aes(sample=Age)) +geom_qq(dparams = params) +geom_abline()
```

![](Titanic-Survival_files/figure-gfm/QQ-plot%20of%20Age%20Distribution-1.png)<!-- -->

# Survival by Sex

``` r
surrived <- titanic %>% filter(Survived==1)
titanic %>% ggplot(aes(Survived, fill=Sex)) + geom_bar(position = position_dodge() )
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Sex-1.png)<!-- -->

``` r
#plot1 survival filled by sex
p1 <- titanic %>%ggplot(aes(Survived, fill = Sex)) +geom_bar()
# plot 2 - survival filled by sex with position_dodge
p2 <- titanic %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
p3 <-titanic %>%ggplot(aes(Sex, fill = Survived)) + geom_bar()
grid.arrange(p1, p2, p3, nrow = 1)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Sex-2.png)<!-- -->

## Result:

  - Less than half of passengers survived.
  - Most of the survivors are female and most females survived.

# Survival by Age

``` r
titanic %>% ggplot(aes(Age, fill=Survived)) +geom_density(alpha = 0.2)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Age-1.png)<!-- -->

``` r
titanic %>% ggplot(aes(Age, color=Survived)) +geom_density(alpha = 0.2)+
  scale_y_continuous(trans = "log2")
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Age-2.png)<!-- -->

# Survival by Fare

``` r
titanic_fare  <- titanic %>% filter(!Fare==0)

titanic_fare %>% ggplot(aes(Survived, Fare)) + geom_boxplot()
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Fare-1.png)<!-- -->

``` r
titanic %>% ggplot(aes(Survived,Fare)) + geom_jitter(width = 0.1, alpha =0.3)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Fare-2.png)<!-- -->

``` r
titanic_fare %>% ggplot(aes(Survived,Fare)) + geom_jitter(width = 0.1, alpha =0.3)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Fare-3.png)<!-- -->

``` r
titanic_fare %>%ggplot(aes(Age, y = ..count.., fill = Survived)) +geom_density(alpha = 0.2)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Fare-4.png)<!-- -->

``` r
titanic %>%filter(Fare > 0) %>%ggplot(aes(Survived, Fare)) +geom_boxplot() +
  scale_y_continuous(trans = "log2") +geom_jitter(alpha = 0.2)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Fare-5.png)<!-- -->

## Result:

  - Passengers who survived in general payed higher fares.
  - The median fare for pasengers who did not survive was lower.

# Survival by Passenger Class

``` r
p1_7 <- titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar()
p2_7 <- titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar(position = position_fill())
p3_7 <- titanic %>% ggplot(aes(Survived, fill=Pclass)) + geom_bar(position = position_fill())

grid.arrange(p1_7, p2_7, p3_7, nrow = 1)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Passenger%20Class-1.png)<!-- -->

## Result:

  - There were more third class passengers than passengers in the first
    two classes combined.
  - Passengers in the first class have the highest survival rate,
    followed by second class. The third-class had the lowest survival
    rate.

# Survival by Age, Sex and Passenger Class

``` r
titanic %>% ggplot(aes(Age, y = ..count..,fill=Survived)) + geom_density(alpha =0.3) + 
  facet_grid(Pclass ~Sex)
```

![](Titanic-Survival_files/figure-gfm/Survival%20by%20Age,%20Sex%20and%20Passenger%20Class-1.png)<!-- -->

## Result:

  - Males were the largest group in the third class.
  - Most first-class and second-class females survived.
  - Almost all second-class males did not survive, except for children.
