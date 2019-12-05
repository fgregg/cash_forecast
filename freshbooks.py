import json
import datetime
import csv
import sys
import collections

from requests_oauthlib import OAuth2Session

class FreshbooksSession(OAuth2Session):
    BASE_URL = 'https://api.freshbooks.com'

    def __init__(self,
                 client_id=None,
                 client_secret=None,
                 token_path=None,
                 account_id=None,
                 business_id=None):

        self.token_path = token_path
        self.account_id = account_id
        self.business_id = business_id

        with open(self.token_path) as f:
            token = json.load(f)

        super().__init__(client_id=client_id,
                         token=token,
                         auto_refresh_url=self.BASE_URL + '/auth/oauth/token',
                         auto_refresh_kwargs={'client_id': client_id,
                                              'client_secret': client_secret},
                         token_updater=self._token_saver)

        self.headers.update({'Api-Version': 'alpha'})

    def _token_saver(self, token):
        with open(self.token_path, 'w') as f:
            json.dump(token, f)

    def paginate(self, endpoint):

        response = self.get(endpoint).json()['response']['result']

        page = response['page']
        pages = response['pages']

        focus, = response.keys() - {'page', 'pages', 'per_page', 'total'}

        yield from response[focus]

        page += 1
        while page <= pages:
            response = self.get(endpoint, params={'page': page}).json()['response']['result']
            assert response['page'] == page
            page += 1

            yield from response[focus]

    def invoices(self):
        invoices = self.paginate(self.BASE_URL + '/accounting/account/{}/invoices/invoices'.format(self.account_id))

        yield from invoices

    def payments(self):
        payments = self.paginate(self.BASE_URL + '/accounting/account/{}/payments/payments'.format(self.account_id))

        yield from payments

    def projects(self):

        projects = self.project_paginate(self.BASE_URL + '/projects/business/{}/projects'.format(self.business_id))

        yield from projects

    def project_paginate(self, endpoint):

        response = self.get(endpoint).json()

        page = response['meta']['page']
        pages = response['meta']['pages']

        focus, = response.keys() - {'meta'}

        yield from response[focus]

        page += 1
        while page <= pages:
            response = self.get(endpoint, params={'page': page}).json()

            assert response['meta']['page'] == page
            page += 1

            yield from response[focus]


def features(invoices, credit_card_payments):
    today = datetime.datetime.now().date()

    for invoice in invoices:

        row = {'invoice_id': invoice['id'],
               'client_id': invoice['customerid'],
               'organization': invoice['current_organization'],
               'description': invoice['description'],
               'amount': float(invoice['amount']['amount']),
               }

        if invoice['id'] in credit_card_payments:
            row['payment_type'] = 'credit card'
        else:
            row['payment_type'] = 'other'

        date_invoiced = invoice['create_date']
        row['invoice_created'] = datetime.datetime.strptime(date_invoiced,
                                                             '%Y-%m-%d').date()

        date_paid = invoice['date_paid'] or None

        if date_paid:
            date_paid=datetime.datetime.strptime(date_paid, '%Y-%m-%d').date()
            paid = True
        else:
            paid = False

        row['invoice_closed'] = date_paid
        row['paid'] = paid

        yield row

            
if __name__ == '__main__':

    import secrets

    client = FreshbooksSession(client_id=secrets.CLIENT_ID,
                               client_secret=secrets.CLIENT_SECRET,
                               token_path='freshbooks_api_token.json',
                               account_id=secrets.ACCOUNT_ID)

    credit_card_payments = collections.defaultdict(list)
    for payment in client.payments():
       if payment['type'] in {'VISA', 'MASTERCARD', 'AMEX', 'Credit Card'}:
           credit_card_payments[payment['invoiceid']].append(payment)

    header = ('invoice_id',
              'client_id',
              'organization',
              'invoice_created',
              'invoice_closed',
              'amount',
              'paid',
              'payment_type',
              'description')
    writer = csv.DictWriter(sys.stdout,
                            fieldnames=header)
    writer.writeheader()
    writer.writerows(features(client.invoices(), credit_card_payments))
