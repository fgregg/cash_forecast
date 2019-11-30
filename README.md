# Forecast Cash
Forecast how much Accounts Receivable will be realized in next 30 days (using [Freshbooks](https://www.freshbooks.com/))

## System requirements
- Python 3
- R

You'll also need an Freshbooks account that has sufficiently privileges to view invoices.

## Installation
```
pip install -r requirements
R -e 'install.packages("timeDate", repos="http://cran.us.r-project.org")'
cp secrets.py.example secrets.py
```

Add your secrets to `secrets.py`. The `secrets.py` file has instructions.

## To run
```
make all
```

Generates a pdf plotting the distribution of simulated outcomes for realized cash over the next 30 days.
