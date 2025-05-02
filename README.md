Hi! This repo is the code I used for my final project with DSC-260! I've uploaded a python file (YahooFinance.py). 
This pyton code encorporates the yfinance package to help pull stocks from the yahoofinance website.
It also tidys the unclean dataset, fixes up the columns, and adds a new index (Date).

I also made two quarto documents to visulizie this data! Attached are a couple of functions that I've created with some R code 
that create Candlesticks & Step graphs, along with volumne plots, Bollinger Bands, and Simple Moving Averages. I also have a function that predicts
stock prices using the nnetar (Neural Network Auto Regression) model based on certain timeframes. 

You can change this code around to pull different stocks (i.e the "tickers" list), or include different timeframes (i.e "timeframes" list).
It is also important to change the filepath to any read_csv() functions to your current working directory.

Important to note that the nnetar models may take awhile to run!

Thankyou for taking a look at my code! 
If you'd like to contact me, my linkedin is: 
https://www.linkedin.com/in/brandon-leslie-5b6b81273/
