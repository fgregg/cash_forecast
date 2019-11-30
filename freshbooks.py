import json
import datetime
import csv
import sys

from requests_oauthlib import OAuth2Session

class FreshbooksSession(OAuth2Session):
    BASE_URL = 'https://api.freshbooks.com'

    def __init__(self,
                 client_id=None,
                 client_secret=None,
                 token_path=None,
                 account_id=None):

        self.token_path = token_path
        self.account_id = account_id

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
        invoices = client.paginate(self.BASE_URL + '/accounting/account/{}/invoices/invoices'.format(self.account_id))

        yield from invoices


def features(invoices):
    today = datetime.datetime.now().date()

    for invoice in invoices:
        date_invoiced = invoice['create_date']

        date_invoiced = datetime.datetime.strptime(date_invoiced, '%Y-%m-%d').date()
        amount = float(invoice['amount']['amount'])
        date_paid = invoice['date_paid'] 

        if date_paid:
            date_paid=datetime.datetime.strptime(date_paid, '%Y-%m-%d').date()
            paid = True
        else:
            paid = False

        yield date_invoiced, date_paid, amount, paid

            
if __name__ == '__main__':

    import secrets

    client = FreshbooksSession(client_id=secrets.CLIENT_ID,
                               client_secret=secrets.CLIENT_SECRET,
                               token_path='freshbooks_api_token.json',
                               account_id=secrets.ACCOUNT_ID)

    writer = csv.writer(sys.stdout)
    writer.writerow(('invoice_created','invoice_closed', 'amount', 'paid'))
    
    writer.writerows(features(client.invoices()))
