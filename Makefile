.PHONY : all
all : expected_realized_cash.pdf

expected_realized_cash.pdf : invoice_age.csv
	Rscript invoice_survival.R

invoice_age.csv : freshbooks_api_token.json
	python freshbooks.py > $@

freshbooks_api_token.json :
	python freshbooks_auth.py > $@
