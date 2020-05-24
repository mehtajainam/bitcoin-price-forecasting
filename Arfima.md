ARFIMA
================
Manaswi Mishra
5/24/2020

``` r
#importing data

library(tseries)
```

    ## Warning: package 'tseries' was built under R version 3.6.3

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 3.6.3

``` r
library(Quandl)
```

    ## Warning: package 'Quandl' was built under R version 3.6.3

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 3.6.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.6.3

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(pracma)
```

    ## Warning: package 'pracma' was built under R version 3.6.3

``` r
Quandl.api_key("UqYbx-p5DwWzyT8AH2c7")
marketpriceUSD<-Quandl("BCHAIN/MKPRU")
```

``` r
chron <- rev(marketpriceUSD$Value)
c(start = tail(marketpriceUSD$Date, 1), end = head(marketpriceUSD$Date, 1))
```

    ##        start          end 
    ## "2009-01-03" "2020-05-25"

``` r
BTC <- ts(chron, start = c(2009, 1, 3), frequency = 365)

# focus on 2017 onwards
train <- window(BTC, start = 2017, end  = 2020)
test <- window(BTC, start = 2020,end = c(2020,30))
```

``` r
plot(BTC)
```

![](Arfima_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plot(train)
```

![](Arfima_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
plot(test)
```

![](Arfima_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
ggseasonplot(train)
```

![](Arfima_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggAcf(train)
```

![](Arfima_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
gglagplot(train)
```

![](Arfima_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
hurstexp(train, d = 50, display = TRUE)
```

    ## Simple R/S Hurst estimation:         0.8290859 
    ## Corrected R over S Hurst exponent:   0.980956 
    ## Empirical Hurst exponent:            1.003595 
    ## Corrected empirical Hurst exponent:  0.9844627 
    ## Theoretical Hurst exponent:          0.5381524

``` r
#As value of the Hurst estimation is greater than 0.5 this is a long memory TS
```

``` r
#testing an Arima model
auto.arima(train,trace = TRUE,stepwise = FALSE, max.order = 1000 )
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(0,1,0)                                : 15976.79
    ##  ARIMA(0,1,0)             with drift         : 15978.52
    ##  ARIMA(0,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,1)                                : 15977.94
    ##  ARIMA(0,1,1)             with drift         : 15979.69
    ##  ARIMA(0,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,2)                                : 15978.66
    ##  ARIMA(0,1,2)             with drift         : 15980.39
    ##  ARIMA(0,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,3)                                : 15980.25
    ##  ARIMA(0,1,3)             with drift         : 15981.99
    ##  ARIMA(0,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,4)                                : 15980.88
    ##  ARIMA(0,1,4)             with drift         : 15982.61
    ##  ARIMA(0,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,5)                                : 15963.99
    ##  ARIMA(0,1,5)             with drift         : 15965.79
    ##  ARIMA(0,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(0,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(0,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(0,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(0,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(0,1,5)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,0)                                : 15978.99
    ##  ARIMA(1,1,0)             with drift         : 15980.74
    ##  ARIMA(1,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,1)                                : 15975.64
    ##  ARIMA(1,1,1)             with drift         : 15977.38
    ##  ARIMA(1,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,2)                                : 15976.7
    ##  ARIMA(1,1,2)             with drift         : 15978.43
    ##  ARIMA(1,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,3)                                : 15978.31
    ##  ARIMA(1,1,3)             with drift         : 15980.06
    ##  ARIMA(1,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,4)                                : 15976.5
    ##  ARIMA(1,1,4)             with drift         : 15978.23
    ##  ARIMA(1,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,5)                                : 15964.59
    ##  ARIMA(1,1,5)             with drift         : 15966.38
    ##  ARIMA(1,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(1,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(1,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(1,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(1,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(1,1,5)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,0)                                : 15980.77
    ##  ARIMA(2,1,0)             with drift         : 15982.51
    ##  ARIMA(2,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,1)                                : 15982.78
    ##  ARIMA(2,1,1)             with drift         : 15984.52
    ##  ARIMA(2,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,2)                                : 15979.69
    ##  ARIMA(2,1,2)             with drift         : 15980.41
    ##  ARIMA(2,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,3)                                : 15980.49
    ##  ARIMA(2,1,3)             with drift         : 15982.3
    ##  ARIMA(2,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,4)                                : 15943.56
    ##  ARIMA(2,1,4)             with drift         : 15945.24
    ##  ARIMA(2,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,5)                                : 15957.11
    ##  ARIMA(2,1,5)             with drift         : 15958.89
    ##  ARIMA(2,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(2,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(2,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(2,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(2,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(2,1,5)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,0)                                : 15983.74
    ##  ARIMA(3,1,0)             with drift         : 15985.49
    ##  ARIMA(3,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,1)                                : 15985.76
    ##  ARIMA(3,1,1)             with drift         : 15987.51
    ##  ARIMA(3,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,2)                                : 15982.04
    ##  ARIMA(3,1,2)             with drift         : 15983.2
    ##  ARIMA(3,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,3)                                : 15973.28
    ##  ARIMA(3,1,3)             with drift         : 15975
    ##  ARIMA(3,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,4)                                : 15957.2
    ##  ARIMA(3,1,4)             with drift         : 15958.9
    ##  ARIMA(3,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,5)                                : 15958.76
    ##  ARIMA(3,1,5)             with drift         : 15960.53
    ##  ARIMA(3,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(3,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(3,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(3,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(3,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(3,1,5)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,0)                                : 15986.08
    ##  ARIMA(4,1,0)             with drift         : 15987.83
    ##  ARIMA(4,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,1)                                : 15976.39
    ##  ARIMA(4,1,1)             with drift         : 15978.11
    ##  ARIMA(4,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,2)                                : 15968.3
    ##  ARIMA(4,1,2)             with drift         : 15947.51
    ##  ARIMA(4,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,3)                                : 15957.12
    ##  ARIMA(4,1,3)             with drift         : 15958.82
    ##  ARIMA(4,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,4)                                : 15930.39
    ##  ARIMA(4,1,4)             with drift         : 15932
    ##  ARIMA(4,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,5)                                : 15948.43
    ##  ARIMA(4,1,5)             with drift         : 15950.2
    ##  ARIMA(4,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(4,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(4,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(4,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(4,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(4,1,5)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,0)                                : 15962.96
    ##  ARIMA(5,1,0)             with drift         : 15964.78
    ##  ARIMA(5,1,0)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,0)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,0)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,0)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,0)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,0)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,1)                                : 15959.85
    ##  ARIMA(5,1,1)             with drift         : 15961.66
    ##  ARIMA(5,1,1)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,1)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,1)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,1)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,1)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,1)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,2)                                : 15948.23
    ##  ARIMA(5,1,2)             with drift         : 15950.08
    ##  ARIMA(5,1,2)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,2)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,2)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,2)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,2)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,2)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,3)                                : 15947.25
    ##  ARIMA(5,1,3)             with drift         : 15949.09
    ##  ARIMA(5,1,3)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,3)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,3)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,3)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,3)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,3)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,4)                                : 15930.14
    ##  ARIMA(5,1,4)             with drift         : 15948.91
    ##  ARIMA(5,1,4)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,4)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,4)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,4)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,4)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,4)(1,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,5)                                : 15932.18
    ##  ARIMA(5,1,5)             with drift         : 15934.01
    ##  ARIMA(5,1,5)(0,0,1)[365]                    : Inf
    ##  ARIMA(5,1,5)(0,0,1)[365] with drift         : Inf
    ##  ARIMA(5,1,5)(1,0,0)[365]                    : Inf
    ##  ARIMA(5,1,5)(1,0,0)[365] with drift         : Inf
    ##  ARIMA(5,1,5)(1,0,1)[365]                    : Inf
    ##  ARIMA(5,1,5)(1,0,1)[365] with drift         : Inf
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ## 
    ## 
    ## 
    ##  Best model: ARIMA(5,1,4)

    ## Series: train 
    ## ARIMA(5,1,4) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4     ar5     ma1     ma2     ma3
    ##       -1.0153  -1.0609  -0.9746  -0.8036  0.0689  1.0662  1.1141  1.0138
    ## s.e.   0.0798   0.1015   0.0791   0.0735  0.0381  0.0732  0.1152  0.0881
    ##          ma4
    ##       0.7743
    ## s.e.  0.0753
    ## 
    ## sigma^2 estimated as 121308:  log likelihood=-7958.62
    ## AIC=15937.25   AICc=15937.45   BIC=15987.23

``` r
#,lambda = "auto", max.order = 500
```

``` r
library(arfima)
```

    ## Loading required package: ltsa

    ## Note that the arfima package has new defaults starting with
    ## 1.4-0: type arfimachanges() for a list, as well as some other notes.
    ## NOTE: some of these are quite important!

    ## 
    ## Attaching package: 'arfima'

    ## The following object is masked from 'package:forecast':
    ## 
    ##     arfima

    ## The following object is masked from 'package:stats':
    ## 
    ##     BIC

``` r
d <-fracdiff::fracdiff(train) #get the fractional d
arfima.ts<-fracdiff::diffseries(train,d$d) # do the fractional difference
acf(arfima.ts, lag=40)
```

![](Arfima_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(arfima.ts,type="l")
```

![](Arfima_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
arfima.model1 <-auto.arima(arfima.ts,stepwise = F,trace = T,lambda = "auto") 
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(0,1,0)                    : 11973.91
    ##  ARIMA(0,1,0) with drift         : 11975.88
    ##  ARIMA(0,1,1)                    : 11767.7
    ##  ARIMA(0,1,1) with drift         : 11769.52
    ##  ARIMA(0,1,2)                    : 11753.44
    ##  ARIMA(0,1,2) with drift         : 11755.21
    ##  ARIMA(0,1,3)                    : 11753.33
    ##  ARIMA(0,1,3) with drift         : 11755.07
    ##  ARIMA(0,1,4)                    : 11749.19
    ##  ARIMA(0,1,4) with drift         : 11750.89
    ##  ARIMA(0,1,5)                    : 11750.79
    ##  ARIMA(0,1,5) with drift         : 11752.51
    ##  ARIMA(1,1,0)                    : 11831.5
    ##  ARIMA(1,1,0) with drift         : 11833.46
    ##  ARIMA(1,1,1)                    : 11720.47
    ##  ARIMA(1,1,1) with drift         : 11722.37
    ##  ARIMA(1,1,2)                    : 11717.71
    ##  ARIMA(1,1,2) with drift         : 11719.67
    ##  ARIMA(1,1,3)                    : 11719.42
    ##  ARIMA(1,1,3) with drift         : 11721.38
    ##  ARIMA(1,1,4)                    : 11718.16
    ##  ARIMA(1,1,4) with drift         : 11720.1
    ##  ARIMA(2,1,0)                    : 11778.72
    ##  ARIMA(2,1,0) with drift         : 11780.68
    ##  ARIMA(2,1,1)                    : 11717.11
    ##  ARIMA(2,1,1) with drift         : 11719.03
    ##  ARIMA(2,1,2)                    : 11707.8
    ##  ARIMA(2,1,2) with drift         : 11709.74
    ##  ARIMA(2,1,3)                    : 11717.46
    ##  ARIMA(2,1,3) with drift         : 11719.45
    ##  ARIMA(3,1,0)                    : 11767.08
    ##  ARIMA(3,1,0) with drift         : 11769.05
    ##  ARIMA(3,1,1)                    : 11712.9
    ##  ARIMA(3,1,1) with drift         : 11714.83
    ##  ARIMA(3,1,2)                    : 11713.56
    ##  ARIMA(3,1,2) with drift         : 11715.49
    ##  ARIMA(4,1,0)                    : 11741.23
    ##  ARIMA(4,1,0) with drift         : 11743.19
    ##  ARIMA(4,1,1)                    : 11715.41
    ##  ARIMA(4,1,1) with drift         : 11717.34
    ##  ARIMA(5,1,0)                    : 11742.7
    ##  ARIMA(5,1,0) with drift         : 11744.67
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ## 
    ## 
    ## 
    ##  Best model: ARIMA(2,1,2)

``` r
arfima.model1
```

    ## Series: arfima.ts 
    ## ARIMA(2,1,2) 
    ## Box Cox transformation: lambda= 0.6625723 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2
    ##       1.0630  -0.1567  -1.5602  0.5740
    ## s.e.  0.1141   0.0767   0.1065  0.0995
    ## 
    ## sigma^2 estimated as 2635:  log likelihood=-5864.58
    ## AIC=11739.17   AICc=11739.22   BIC=11764.16

``` r
#performing the same using arfima
print("Arfima using arfima method")
```

    ## [1] "Arfima using arfima method"

``` r
arfima.model2 <-forecast::arfima(train,lambda = "auto")
summary(arfima.model2)
```

    ## 
    ## Call:
    ##   forecast::arfima(y = train, lambda = "auto") 
    ## 
    ## *** Warning during (fdcov) fit: unable to compute correlation matrix; maybe change 'h'
    ## 
    ## Coefficients:
    ##        Estimate
    ## d         0.064
    ## ar.ar1   -0.104
    ## ar.ar2    0.012
    ## ar.ar3   -0.017
    ## ar.ar4    0.199
    ## ar.ar5    0.838
    ## ma.ma1   -1.096
    ## ma.ma2   -1.001
    ## ma.ma3   -1.004
    ## ma.ma4   -0.707
    ## sigma[eps] = 357801.9 
    ## [d.tol = 0.0001221, M = 100, h = 0.0001641]
    ## Log likelihood: -1.557e+04 ==> AIC = 31162.97 [11 deg.freedom]

``` r
#The lambda ransform seems to work on the split method but not on the arfima procedure
AIC(arfima.model1)
```

    ## [1] 11739.17

``` r
AIC(arfima.model2)
```

    ## [1] 31162.97

``` r
checkresiduals(arfima.model1)
```

![](Arfima_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,1,2)
    ## Q* = 33.517, df = 6, p-value = 8.338e-06
    ## 
    ## Model df: 4.   Total lags used: 10

``` r
#Bad results 
```

``` r
forecast.arfima<-forecast(arfima.model1,h=30)
plot(forecast.arfima)
```

![](Arfima_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
accuracy(arfima.model1) 
```

    ##                   ME     RMSE      MAE      MPE     MAPE      MASE      ACF1
    ## Training set 24.8916 371.5347 209.1332 203.3728 357.1367 0.9034218 0.1062686
