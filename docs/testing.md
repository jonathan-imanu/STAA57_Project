testing
================
Joshua Crisologo: 1009860438
2024-03-31

``` r
# filtering data and lakes to most recent date of test
 df = data %>% 
      group_by(Lake.Name) %>%
      filter(phos_date==max(phos_date)) %>%
      filter(trans_date==max(trans_date))
```

``` r
# t.test sample test
t.test(df$avg_phos_ug_l)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  df$avg_phos_ug_l
    ## t = 34.777, df = 1169, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  10.21926 11.44126
    ## sample estimates:
    ## mean of x 
    ##  10.83026

``` r
# bootstrapping sample, creating a 95% confidence interval for the
# average phosphorous level
boot_function = function() {
  boot_data = df[sample(nrow(df), replace = TRUE), ]
  
  boot_mean = mean(boot_data$avg_phos_ug_l, na.rm = TRUE)
  
  return(boot_mean)
}

quantile(replicate(100,boot_function()), c(0.025, 0.975))
```

    ##     2.5%    97.5% 
    ## 10.25697 11.37912

``` r
# using formulas in regards to TSI to calculate the TSI of each lake
# 0 = oligotrophic
# 1 = mesotrophic
# 2 = eutrophic
df = df %>% mutate(TSI_Depth = (60 - 14.41*log(secchi_depth_m))) %>% 
  mutate(TSI_Phos = (14.42*log(avg_phos_ug_l) + 4.15)) %>% 
  mutate(TSI = ((TSI_Depth + TSI_Phos) / 2)) %>% 
  mutate(classification = case_when(TSI <= 40 ~ "0",
                                    TSI <= 50 ~ "1",
                                    TRUE ~ "2"))

# dropping NA values (8)
df_rm = df %>% drop_na()
```

``` r
# cross validation: splitting df into train and test of 60/40% for model testing
final = df_rm
final = final %>% mutate(group_ind = sample(c("train", "test"),
                                            size=1,
                                            prob = c(0.6, 0.4),
                                            replace = T))

final_train = final %>% filter(group_ind == "train")
final_test = final %>% filter(group_ind == "test")

# implementing decision tree analysis 
# relating classification level (0,1,2) to avg phos lvl and secchi depth
library(rpart)
library(rattle)
```

    ## Loading required package: bitops

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.5.1 Copyright (c) 2006-2021 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

``` r
tree.m = rpart(classification ~ avg_phos_ug_l + secchi_depth_m, data = final_train,
               method = "class")
fancyRpartPlot(tree.m)
```

![](testing_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# another test: using randomForest with the same parameters to predict
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:rattle':
    ## 
    ##     importance

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
rforest.m = randomForest(as.factor(classification) ~ avg_phos_ug_l + secchi_depth_m,
                         data=final_train,
                         ntree=500, importance=TRUE)
```

``` r
# plotting variable importance
varImpPlot(rforest.m)
```

![](testing_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# prediction using decision tree
final_test = final_test %>% ungroup(.) %>% mutate(tree_predictions = 
                                        predict(tree.m, newdata = final_test, type = "class"))

# prediction using random forest

final_test = final_test %>% mutate(rforest_predict = 
                                        predict(rforest.m, newdata = final_test))

glimpse(final_test)
```

    ## Rows: 526
    ## Columns: 19
    ## $ lat              <int> 432922, 440122, 440830, 440953, 441018, 441254, 44160…
    ## $ long             <int> 803130, 790242, 804402, 810304, 810300, 810138, 77311…
    ## $ STN              <int> 7060, 6949, 7522, 7170, 7248, 1138, 6999, 6951, 6919,…
    ## $ Site.ID          <int> 1, 3, 1, 1, 1, 1, 1, 9, 1, 1, 6, 3, 2, 1, 1, 1, 1, 2,…
    ## $ Township         <chr> "WATERLOO", "SCUGOG", "SOUTHGATE", "BRANT", "BRANT", …
    ## $ Lake.Name        <chr> "FOUR WELLS LAKE", "CHALK LAKE", "WILDER LAKE", "MARL…
    ## $ Site.Description <chr> "Main pond, deep spot", "W end, deep spot", "South en…
    ## $ avg_phos_ug_l    <dbl> 14.2, 7.0, 6.2, 11.8, 21.0, 10.2, 20.3, 13.6, 21.2, 2…
    ## $ phos_is_outlier  <chr> "No", "No", "No", "No", "No", "No", "No", "No", "Yes"…
    ## $ phos_date        <date> 2022-05-24, 2022-10-02, 2022-10-23, 2022-10-11, 2022…
    ## $ secchi_depth_m   <dbl> 2.4, 5.0, 5.4, 5.2, 3.0, 3.0, 4.6, 2.7, 2.2, 1.3, 2.0…
    ## $ trans_date       <date> 2022-05-27, 2022-10-02, 2021-10-17, 2022-10-26, 2022…
    ## $ TSI_Depth        <dbl> 47.38450, 36.80800, 35.69899, 36.24283, 44.16900, 44.…
    ## $ TSI_Phos         <dbl> 42.40975, 32.21002, 30.46000, 39.74000, 48.05201, 37.…
    ## $ TSI              <dbl> 44.89712, 34.50901, 33.07950, 37.99141, 46.11051, 40.…
    ## $ classification   <chr> "1", "0", "0", "0", "1", "1", "1", "1", "1", "2", "2"…
    ## $ group_ind        <chr> "test", "test", "test", "test", "test", "test", "test…
    ## $ tree_predictions <fct> 1, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 0, 0, 2, 0, 0, 0, 0,…
    ## $ rforest_predict  <fct> 1, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 0, 0, 2, 0, 0, 0, 0,…

``` r
# create the confusion matrix using decision tree model and test accuracy
conmat = table(final_test$classification, final_test$tree_predictions)
sum(diag(conmat))/sum(conmat)
```

    ## [1] 0.9410646

``` r
# create confusion matrix using random forest model and test accuracy
conmat = table(final_test$classification, final_test$rforest_predict)
sum(diag(conmat))/sum(conmat)
```

    ## [1] 0.9790875

``` r
#lat long map according to classification (W.I.P.)
ggplot(df_rm, aes(y=lat, x=long, col=classification)) + geom_point()
```

![](testing_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# attempt at relating coords to map of Ontario (W.I.P.)
library(ggpubr)
library(jpeg)
img=readJPEG("ontario.jpg")

ggplot(df_rm, aes(y=lat, x=long, col=classification)) + background_image(img) +
  geom_point()
```

![](testing_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->