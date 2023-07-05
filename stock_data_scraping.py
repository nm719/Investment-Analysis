# pip install yfinance

import pandas as pd
import yfinance as yf

# Set the start and end dates
training_start_date = '2014-01-01'
training_end_date = '2019-12-31'
test_start_date = '2020-01-01'
test_end_date = '2022-12-31'

# Stock tickers
stocks = 'AAPL,MSFT,NVDA,INTC,ADBE,CSCO,AMZN,TSLA,HD,NKE,\
    MCD,LOW,GOOGL,META,DIS,CMCSA,VZ,NFLX,V,JPM,\
    MA,BAC,WFC,C,WMT,PG,KO,PEP,COST,PM'

# Get closing prices for the training data
training_prices = yf.download(stocks, training_start_date, training_end_date)['Close']
training_prices.insert(0, "Date", training_prices.index)
training_prices.reset_index(drop=True, inplace=True)

# Get closing prices for the test data
test_prices = yf.download(stocks, test_start_date, test_end_date)['Close']
test_prices.insert(0, "Date", test_prices.index)
test_prices.reset_index(drop=True, inplace=True)

# Export data into csv
training_prices.to_csv("training.csv")
test_prices.to_csv("test.csv")

print(training_prices.head())
print(test_prices.head())
