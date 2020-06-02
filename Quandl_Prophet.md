Quandl\_Prophet
================
MichaelSetyawan
5/17/2020

\#Time Series Project - Quandl Data using Prophet

## Read Data and Import Libraries

``` r
library(tseries)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 3.6.2

``` r
library(Quandl)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(prophet)
```

    ## Warning: package 'prophet' was built under R version 3.6.2

    ## Loading required package: Rcpp

    ## Loading required package: rlang

``` r
library(stringr)

Quandl.api_key("UqYbx-p5DwWzyT8AH2c7")
btc_usd<-Quandl("BCHAIN/MKPRU") #Market price BTC to USD
cost_trans<-Quandl("BCHAIN/CPTRA") #Cost per BTC transaction
mar_cap<-Quandl("BCHAIN/MKTCP") #BTC Market Cap Data
```

Convert to chronological order and time series object for visualization
purpose. (Prophet works with dataframe and not time series objects)

Visualize basic plots for the 3 different time series objects

``` r
ts.plot(btc_usd.ts)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ts.plot(cost_trans.ts)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ts.plot(mar_cap.ts)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
Observation: from a rough observations, seems that btc\_usd have very
similar shape with the market cap but just with very different scale.
Also, since it was very stagnant in the beginning, we will focus
starting from 2016
onwards.

## Prophet Model for BTC-USD

``` r
#Build function to customize dataframe as Prophet input, put start year for the starting year of subset
prophet_df <- function(dat,year){
  colnames(dat) <- c("ds", "y")
  print(summary(dat))
  new = subset(dat, format(as.Date(ds),"%Y")>=year)
  lam = BoxCox.lambda(new$y, method = "loglik")
  new$y = BoxCox(new$y, lam)
  fin_list = list('lam'=lam,'df'=new)
  return(fin_list)
}
btc_usd_prop = prophet_df(btc_usd, year = "2016")
```

    ##        ds                   y            
    ##  Min.   :2009-01-03   Min.   :    0.000  
    ##  1st Qu.:2011-11-07   1st Qu.:    5.165  
    ##  Median :2014-09-11   Median :  314.450  
    ##  Mean   :2014-09-11   Mean   : 2073.011  
    ##  3rd Qu.:2017-07-15   3rd Qu.: 2695.974  
    ##  Max.   :2020-05-19   Max.   :19498.683

``` r
m1 = prophet(btc_usd_prop$df)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(m1, periods = 30)
forecast.m1 <- predict(m1, future)
plot(m1,forecast.m1)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
prophet_plot_components(m1,forecast.m1)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
head(forecast.m1[,c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
```

    ##           ds     yhat yhat_lower yhat_upper
    ## 1 2016-01-01 26.97172   22.28125   31.77630
    ## 2 2016-01-02 26.82333   21.74809   31.62263
    ## 3 2016-01-03 26.97781   22.03949   31.74996
    ## 4 2016-01-04 26.91004   22.14457   31.73419
    ## 5 2016-01-05 26.88334   22.24509   31.75399
    ## 6 2016-01-06 26.91089   22.13617   31.30583

``` r
tail(forecast.m1[,c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
```

    ##              ds     yhat yhat_lower yhat_upper
    ## 1626 2020-06-13 91.53066   86.29735   96.45142
    ## 1627 2020-06-14 91.79977   86.62006   96.79820
    ## 1628 2020-06-15 91.84315   87.24354   96.97671
    ## 1629 2020-06-16 91.92229   86.69527   97.31388
    ## 1630 2020-06-17 92.04897   87.05277   96.57967
    ## 1631 2020-06-18 92.08245   87.14288   96.91357

Analyze Residual for BTC-USD model

``` r
m1.resid = ts(btc_usd_prop$df$y-forecast.m1$yhat[1:1601])
summary(m1.resid)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -16.562539  -2.225380  -0.261840  -0.000611   2.021077  18.455623

``` r
ts.plot(m1.resid)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
qqnorm(m1.resid, pch = 1)
qqline(m1.resid, col = "steelblue", lwd = 2)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
adf.test(m1.resid)
```

    ## Warning in adf.test(m1.resid): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  m1.resid
    ## Dickey-Fuller = -6.7764, Lag order = 11, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
kpss.test(m1.resid)
```

    ## Warning in kpss.test(m1.resid): p-value greater than printed p-value

    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  m1.resid
    ## KPSS Level = 0.028374, Truncation lag parameter = 8, p-value = 0.1

``` r
shapiro.test(m1.resid)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m1.resid
    ## W = 0.97028, p-value < 2.2e-16

Residuals are stationary but the distribution is not normal (looking at
the QQ plot and Shapiro Wilk test results)

### Prophet Model for Cost per Transaction

``` r
cost_trans_prop = prophet_df(cost_trans, year = "2016")
```

    ##        ds                   y          
    ##  Min.   :2009-01-03   Min.   :  0.000  
    ##  1st Qu.:2011-11-07   1st Qu.:  3.372  
    ##  Median :2014-09-10   Median :  8.761  
    ##  Mean   :2014-09-10   Mean   : 20.901  
    ##  3rd Qu.:2017-07-14   3rd Qu.: 32.436  
    ##  Max.   :2020-05-18   Max.   :161.686

``` r
m2 = prophet(cost_trans_prop$df)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(m2, periods = 30)
forecast.m2 <- predict(m2, future)
plot(m2,forecast.m2)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
prophet_plot_components(m2,forecast.m2)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->
Analyze Residual for Cost Per Transaction model

``` r
m2.resid = ts(cost_trans_prop$df$y-forecast.m2$yhat[1:1600])
summary(m2.resid)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -1.6543624 -0.2198541 -0.0027565  0.0000109  0.2335130  1.5788161

``` r
ts.plot(m2.resid)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
qqnorm(m2.resid, pch = 1)
qqline(m2.resid, col = "steelblue", lwd = 2)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
adf.test(m2.resid)
```

    ## Warning in adf.test(m2.resid): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  m2.resid
    ## Dickey-Fuller = -7.1401, Lag order = 11, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
kpss.test(m2.resid)
```

    ## Warning in kpss.test(m2.resid): p-value greater than printed p-value

    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  m2.resid
    ## KPSS Level = 0.037562, Truncation lag parameter = 8, p-value = 0.1

``` r
shapiro.test(m2.resid)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m2.resid
    ## W = 0.98874, p-value = 8.444e-10

### Prophet Model for Market Cap

``` r
mar_cap_prop = prophet_df(mar_cap, year = "2016")
```

    ##        ds                   y            
    ##  Min.   :2009-01-03   Min.   :0.000e+00  
    ##  1st Qu.:2011-11-07   1st Qu.:4.539e+07  
    ##  Median :2014-09-10   Median :4.326e+09  
    ##  Mean   :2014-09-10   Mean   :3.558e+10  
    ##  3rd Qu.:2017-07-14   3rd Qu.:4.418e+10  
    ##  Max.   :2020-05-18   Max.   :3.265e+11

``` r
m3 = prophet(mar_cap_prop$df)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(m3, periods = 30)
forecast.m3 <- predict(m3, future)
plot(m3,forecast.m3)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
prophet_plot_components(m3,forecast.m3)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

Analyze Residual for Market Cap model

``` r
m3.resid = ts(mar_cap_prop$df$y-forecast.m3$yhat[1:1600])
summary(m3.resid)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -13163.490  -1778.530   -202.644     -0.097   1568.971  13848.126

``` r
ts.plot(m3.resid)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
qqnorm(m3.resid, pch = 1)
qqline(m3.resid, col = "steelblue", lwd = 2)
```

![](Quandl_Prophet_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
adf.test(m3.resid)
```

    ## Warning in adf.test(m3.resid): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  m3.resid
    ## Dickey-Fuller = -6.8308, Lag order = 11, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
kpss.test(m3.resid)
```

    ## Warning in kpss.test(m3.resid): p-value greater than printed p-value

    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  m3.resid
    ## KPSS Level = 0.02676, Truncation lag parameter = 8, p-value = 0.1

``` r
shapiro.test(m3.resid)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m3.resid
    ## W = 0.97236, p-value < 2.2e-16
