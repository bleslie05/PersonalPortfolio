import yfinance as yf
from datetime import datetime
import pandas as pd


# Ticker & Metric list
tickers = ['NVDA', 'COST', 'AMZN', 'INLF', 'WMT', 'JNJ', 'UNH', 'ZSPC']
#metrics = ['_Close', '_Open', '_Low', '_High', '_Volume']

#Uses most Recent Date
end_date = datetime.today().strftime('%Y-%m-%d')

#Downloading tickr history from yahoo finance (yf)
data = yf.download(tickers, end=end_date, interval='1d', group_by='ticker')

#downloading raw data
data.to_csv('unclean_stocks.csv')


#flattening column multi-index
data.columns = [f"{metric}_{ticker}" for metric, ticker in data.columns]

#date becomes a regular column
data = data.reset_index()

# Pivoting data into longer format (like pivot_longer() in R)
data = data.melt(id_vars="Date", var_name="Metric_Ticker", value_name="Price")

#Splitting columns with expand=TRUE and str.split()
data[['Ticker', 'Metric']] = data['Metric_Ticker'].str.split('_', expand=True)

#Dropping the original combined column
data = data.drop(columns='Metric_Ticker')

#expansion of metrics for visulization
data = data.pivot(index=["Date", "Ticker"], columns="Metric", values="Price").reset_index()

#R data
data.to_csv('stocks.csv', index=False)
