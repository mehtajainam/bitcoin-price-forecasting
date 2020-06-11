# Forecasting Bitcoin Prices

GitHub repository for our final project of the 'Time Series Analysis and Forecasting' course at the University of Chicago. 
https://github.com/mehtajainam/bitcoin-price-forecasting

### Authors: Mananswi Mishra, Jainam Mehta, Andrew Morse, Michael Setyawan
### Date: 11 June 2020


Bitcoin is a digital currency created in January 2009, following the housing market crash, by the mysterious pseudonymous Satoshi Nakamoto. It follows the ideas set out in the whitepaper, where balances are kept on a public ledger than everyone has transparent access to. Despite it not being legal tender, Bitcoin charts high in popularity, and has triggered the launch of hundreds of other virtual currencies collectively referred to as Alt-coins. This allure may be attributed to the promise of lower transaction fees than traditional online payment mechanisms and the fact that Bitcoin is operated by a decentralized authority, unlike government-issued currencies.

These reasons have caught the eye of many investors who look to profit off of the spectacular gains Bitcoin has seen in the past. Furthermore, in times of current market uncertainty, Bitcoin and other cryptocurrencies may be a risky, but highly lucrative alternative to traditional financial instrument. However, one of the key issues has been the major volatility that Bitcoin has been since inception. We plan to decipher this volatility by using basic as well as advanced time series forecasting tools.

This project and repository comprises the following modeling approaches:

1. ARIMA (Direct and Recursive forecasting)
2. ARFIMA
3. ARFIMA + Holt Winters
4. VARMA (Multivariate analysis with ETH and LTC)
5. Prophet by Facebook
6. Neural Network - LSTM

The best model selected using the sMAPE metric was the ARFIMA + Holt Winters model, which was able to predict the price of bitcoin in January 2020 with an average error of US$787.
