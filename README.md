# Dashboard: Rapid stock analysis

###Dashboard - R Shiny app for non-coders
Click [here](https://petaboy.shinyapps.io/dashboard/) to access the Dashboard. I do not claim responsibility for any incorrect financial information provided.    
 
#### Correlation and Cointegration
This module first performs a linear regression on the daily percent change in the price of each of the stock to another stock or index.  This regression’s r-squared value and beta (also known as coefficient) are outputted below: A relatively high r-squared shows a strong correlation between two components, which validates the beta for use in further analysis: a beta higher than one indicates that the component’s price volatility may be higher than that of the index. A beta between one and zero indicates that that component’s price volatility is lower than that of an index.

The second part of this module performs augmented-dickey-fuller test, with a lag of zero, to check for stationarity between the price of a stock to that of another stock or index.  A low p-value (<0.05) indicates a high certainty of stationarity between the two components: This further indicates that the two components may be cointegrated. This information is useful for hedges and spreads.

#### Price, Net Income and Short Interest trends
The trends among stock prices, eps and short interests can all be analyzed here. The prices and net income (if available) are all divided by the most recent price or net income, respectively. The resulting values are all given as percent of the most recent value: This transformation also allows for a more straightforward comparison of price movements of different stocks because base prices of instruments are already taken into account. 

Short interests, if available, can be plotted against the eps and stock price trends. The short interest is the volume of stocks shorted divided by total traded volume of stock. Both of these values are taken from the FINRA Trading Reporting Facility (TRF). Reported as a percent, a relatively high short interest on a given day, may indicate an anticipation of breakout, whether positive or negative.   

#### Daily Value-at-Risk assessment

Here, the daily percent changes in stock (or index) prices are plotted in a density curve. The graph on top plots the density curves of all the requested stocks and indices. A wide density curve indicates a higher variability in daily stock price movements, while a narrow density curve indicates a lower variability in daily stock price movements: Large cap stocks in general have a lower variability in daily price movements than that of small cap stocks.

The second and third plots are the individual density curves for the first two stocks requested on the sidebar. By design, this program only plots the density curves of the first two stocks (indices and other requested stocks won’t be plotted). These density curves however are more detailed: The curve is divided into regions as shown in the legend. The area covered by the 5%-95% region correlate to the 90% of expected daily returns. Area covered by the 0%-5% and 95%-100% correspond to the tail regions, of low and high daily returns respectively. Let’s say for example that for a given stock, a daily return of -5% falls within the 0-5% area (or right on the 5% area line). If future daily returns are extrapolated (assuming conditions are met) from this density curves, then, we can say that on any given day, there is at least a 5% chance that the daily return of that stock will be <5% or in a span of 20 days, there will be at least one day where the daily return of the stock will be <5%. 

#### Max Drawdown analysis 
This module calculates and plots the max drawdown of a given stock or index. The max drawdown or max loss statistic is defined as the maximum value drop after one of the peaks. For financial instruments the max drawdown represents the worst investment loss for a buy-and-hold strategy invested in the stock. The maxdrawdown function, as given in the CRAN package, ‘tseries’ is used. 

#### Data Sources and further readings
**Stock, Index, EPS and Short Interest data are sourced from Quandl:**

**Stock Price data -** https://www.quandl.com/data/WIKI-Wiki-EOD-Stock-Prices

**Index data -** https://www.quandl.com/data/NASDAQOMX-NASDAQ-OMX-Global-Index-Data

**EPS data -** https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data

**Short interest data -** https://www.quandl.com/data/FINRA-Financial-Industry-Regulatory-Authority

**Maxdrawdown function -** ftp://cran.r-project.org/pub/R/web/packages/tseries/tseries.pdf 

**Cointegration with R -** http://gekkoquant.com/2012/12/17/statistical-arbitrage-testing-for-cointegration-augmented-dicky-fuller/

### smallempire.R - for coders
This file contains all the functions encapsulated within the Dashboard. Individual components can be run easily on R, given that all the necessary packages are installed. 

#### Import
Import function imports data from Quandl. This function can be used to import stock EOD prices. 
```
aapl <- import('AAPL',"stock") #import end-of-day stock price of one stock
combo <- import (c('GOOGL','AAPL'),"stock") #Import EOD stock price of multiple stocks 

```

NASDAQ Indices can also be imported with this function.

```
index <- import('NQUSB',"index") #Import NASDAQ US Benchmark Index.
```

FINRA TRF Short interest as well as Net income (with subscription to [this](https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data) Quandl Database ) can be imported

```
income <- import('AAPL',"eps")
short_interest <- import('AAPL',"short")

```

#### cointegration

Perform the cointegration and correlation test. Returns command line output with regression and ADF results.

```
stock <- import('AAPL',"stock")
index <- import('NQUSB',"index")

result <- cointegration(stock,index)
```

This function works without an index, provided there are two or more stocks datasets.

#### trend_plot

Plot the stock data, eps (if available) and short interest in one ggplot. Start and End dates can be provided to focus the plot on given time period.

```
stock <- import('AAPL',"stock")
index <- import('NQUSB',"index")
data <- c(stock,index)

trend_plot(data,start='2010-10-10',end='2011-11-11')
```

#### red_flags
Plot maxdrawdown results with this function.

```
data <- import('AAPL',"stock")
trend_plot(data)

```

#### var
Plot the density curves of daily stock and/or index returns. These density curves are necessary for a qualitative daily Value-at-Risk evaluation.

```
stock <- import(c('AAPL','GOOGL'),"stock"))

var(data) #Requires two or more datasets. 
```


