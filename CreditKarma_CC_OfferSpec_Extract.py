# import packages for web scraping/parsing
import requests  # functions for interacting with web pages
from lxml import html  # functions for parsing HTML
from bs4 import BeautifulSoup  # DOM html manipulation
from __future__ import division, print_function
import csv, os, json
from time import sleep
import pandas as pd


# -----------------------------------------------
# get HTML tags using the requests and lxml packages
# -----------------------------------------------

# standard html ( we use this page as reference for all html structure)
web_page = requests.get('https://www.creditkarma.com/creditcard/CCBarclays1201?originEvent=12412488223/', auth=('user', 'pass'))
# obtain the entire HTML text for the page of interest
# show the status of the page - should be 200 (no error)
web_page.status_code
# show the encoding of the page... should be utf8
web_page.encoding
# show tags
web_page_text = web_page.text
print(web_page_text)


# -----------------------------------------------
# create parser function
# -----------------------------------------------
def credKarmaParser(url):
    headers = {
        'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36'}
    page = requests.get ( url, headers=headers )
    while True:
        sleep ( 3 )
        try:
            doc = html.fromstring ( page.content )
            XPATH_NAME = '//h1[@ck-header="page"]//text()'
            XPATH_ANNUAL_FEE = '//div[contains(text(),' \
                               '"Annual Fee")]/following-sibling::div/text()'
            XPATH_PURCHASE_INTRO_APR = '//div[contains(text(),"Purchase Intro ' \
                                       'APR")]/following-sibling::div/text()'
            XPATH_PURCHASE_REGULAR_APR = '//div[contains(text(),"Purchase ' \
                                         'Regular ' \
                                         'APR")]/following-sibling::div/text()'
            XPATH_BALANCE_TRANSFER_INTRO_APR = '//div[contains(text(),"Balance Transfer Intro APR")]/following-sibling::div/text()'
            XPATH_BALANCE_TRANSFER_REGULAR_APR = '//div[contains(text(),"Balance Transfer Regular APR")]/following-sibling::div/text()'
            XPATH_CASH_ADVANCE_RATE = '//td[contains(text(),"Cash Advance Rate")]/following-sibling::td/text()'
            XPATH_CASH_ADVANCE_FEE = '//td[contains(text(),"Cash Advance ' \
                                   'Fee")]/following-sibling::td/text()'
            XPATH_LATE_DEE = '//td[contains(text(),"Late ' \
                      'Fee")]/following-sibling::td/text()'
            XPATH_PENALTY_APR = '//td[contains(text(),"Penalty ' \
                      'APR")]/following-sibling::td/text()'
            XPATH_BALANCE_TRANSFER_FEE = '//td[contains(text(),"Balance ' \
                                       'Transfer Fee")]/following-sibling::td/text()'
            XPATH_FOREIGN_TRANSACTION_FEE = '//td[contains(text(),"Foreign ' \
                                    'Transaction ' \
                      'Fee")]/following-sibling::td/text()'
            XPATH_RETURN_PAYMENT_FEE = '//td[contains(text(),"Return Payment ' \
                      'Fee")]/following-sibling::td/text()'

            RAW_NAME = doc.xpath ( XPATH_NAME )
            RAW_ANNUAL_FEE = doc.xpath ( XPATH_ANNUAL_FEE )
            RAW_PURCHASE_INTRO_APR = doc.xpath ( XPATH_PURCHASE_INTRO_APR )
            RAW_PURCHASE_REGULAR_APR = doc.xpath ( XPATH_PURCHASE_REGULAR_APR )
            RAW_BALANCE_TRANSFER_INTRO_APR = doc.xpath (XPATH_BALANCE_TRANSFER_INTRO_APR )
            RAW_BALANCE_TRANSFER_REGULAR_APR = doc.xpath (XPATH_BALANCE_TRANSFER_REGULAR_APR )
            RAW_CASH_ADVANCE_RATE = doc.xpath ( XPATH_CASH_ADVANCE_RATE )
            RAW_CASH_ADVANCE_FEE = doc.xpath ( XPATH_CASH_ADVANCE_FEE )
            RAW_LATE_DEE = doc.xpath ( XPATH_LATE_DEE )
            RAW_PENALTY_APR = doc.xpath ( XPATH_PENALTY_APR )
            RAW_BALANCE_TRANSFER_FEE = doc.xpath ( XPATH_BALANCE_TRANSFER_FEE )
            RAW_FOREIGN_TRANSACTION_FEE = doc.xpath ( XPATH_FOREIGN_TRANSACTION_FEE )
            RAW_RETURN_PAYMENT_FEE = doc.xpath ( XPATH_RETURN_PAYMENT_FEE )

            NAME = ' '.join ( ''.join ( RAW_NAME ).split ( ) ) if RAW_NAME else None
            ANNUAL_FEE = ' '.join (''.join ( RAW_ANNUAL_FEE ).split ( ) ) if RAW_ANNUAL_FEE else None
            PURCHASE_INTRO_APR = ' '.join ( ''.join (RAW_PURCHASE_INTRO_APR ).split ( ) ) if RAW_PURCHASE_INTRO_APR else None
            PURCHASE_REGULAR_APR = ' '.join ( ''.join (RAW_PURCHASE_REGULAR_APR ).split ( ) ) if RAW_PURCHASE_REGULAR_APR else None
            BALANCE_TRANSFER_INTRO_APR = ' '.join ( ''.join (RAW_BALANCE_TRANSFER_INTRO_APR ).split ( ) ) if RAW_BALANCE_TRANSFER_INTRO_APR else None
            BALANCE_TRANSFER_REGULAR_APR = ' '.join ( ''.join (RAW_BALANCE_TRANSFER_REGULAR_APR ).split ( ) ) if RAW_BALANCE_TRANSFER_REGULAR_APR else None
            CASH_ADVANCE_RATE = ' '.join ( ''.join (RAW_CASH_ADVANCE_RATE ).split ( ) ) if RAW_CASH_ADVANCE_RATE else None
            CASH_ADVANCE_FEE = ' '.join ( ''.join (RAW_CASH_ADVANCE_FEE
                                                 ).split ( ) ) if \
                RAW_CASH_ADVANCE_FEE else None
            PENALTY_APR = ' '.join ( ''.join (
                RAW_PENALTY_APR ).split ( ) ) if RAW_PENALTY_APR else None
            BALANCE_TRANSFER_FEE = ' '.join ( ''.join (
                RAW_BALANCE_TRANSFER_FEE ).split ( ) ) if RAW_BALANCE_TRANSFER_FEE else None
            FOREIGN_TRANSACTION_FEE = ' '.join ( ''.join (
                RAW_FOREIGN_TRANSACTION_FEE ).split ( ) ) if RAW_FOREIGN_TRANSACTION_FEE else None
            RETURN_PAYMENT_FEE = ' '.join ( ''.join (
                RAW_RETURN_PAYMENT_FEE ).split ( ) ) if RAW_RETURN_PAYMENT_FEE else None

            data = {
                'NAME': NAME,
                'ANNUAL_FEE': ANNUAL_FEE,
                'PURCHASE_INTRO_APR': PURCHASE_INTRO_APR,
                'PURCHASE_REGULAR_APR': PURCHASE_REGULAR_APR,
                'BALANCE_TRANSFER_INTRO_APR': BALANCE_TRANSFER_INTRO_APR,
                'BALANCE_TRANSFER_REGULAR_APR': BALANCE_TRANSFER_REGULAR_APR,
                'CASH_ADVANCE_RATE': CASH_ADVANCE_RATE,
                'CASH_ADVANCE_FEE': CASH_ADVANCE_FEE,
                'PENALTY_APR': PENALTY_APR,
                'BALANCE_TRANSFER_FEE': BALANCE_TRANSFER_FEE,
                'FOREIGN_TRANSACTION_FEE': FOREIGN_TRANSACTION_FEE,
                'RETURN_PAYMENT_FEE': RETURN_PAYMENT_FEE,
                'URL': url
            }

            return data
        except Exception as e:
            print
            e


def readCredCard():
    CredCardList = ['CCUSAA02?originEvent=12418908468',
'CCAmericanExpressCard101?originEvent=12418909624',
'CCAmericanExpress122014?originEvent=12418910480',
'CCCapitalOne773?originEvent=12418911416',
'CCCapitalOne775?originEvent=12418911828',
'CCCitiBank1314?originEvent=12418997555',
'CCUSAA06?originEvent=12418997704',
'CCBankofAmerica12?originEvent=12418998756',
'LuxuryCard02?originEvent=12418999194',
'USBank13?originEvent=12419000373',
'chaseSapphirePreferred?originEvent=12419000702',
'CCCapitalOne1007?originEvent=12419001816',
'chaseSouthwest?originEvent=12419002216',
'CCAmericanExpressCard1456?originEvent=12419003340',
'ChaseSapphireReserve?originEvent=12419003599',
'CCCitiBank1794?originEvent=12419004608',
'CCAmericanExpressCard116?originEvent=12419005099',
'CCUSAA04?originEvent=12419025109',
'CCUSAA12?originEvent=12419025391',
'CCAmericanExpress22034660?originEvent=12419026415',
'CCUSAA07?originEvent=12419026866',
'CCUSAA08?originEvent=12419028073',
'CCUSAA09?originEvent=12419028430',
'CCCapitalOne582?originEvent=12419029540',
'CCCapitalOne1009?originEvent=12419029832',
'CCAmericanExpressCard54?originEvent=12419030918',
'chaseFreedomUnlimited?originEvent=12419031283',
'coCash?originEvent=12419032425',
'CCAmericanExpressCard1201?originEvent=12419032760',
'CCAmericanExpressCard57?originEvent=12419051962',
'chaseUnitedMileagePlusExplorer?originEvent=12419052334',
'CCBarclays1204?originEvent=12419053554',
'chaseMarriottPremier?originEvent=12419053879',
'CCAmericanExpressCard155?originEvent=12419054980',
'CCMerrickBank1302?originEvent=12419055332',
'CCAmericanExpressCard82?originEvent=12419056346',
'sonyCard?originEvent=12419056646',
'CCBarclays1400?originEvent=12419058131',
'CCCitiBank1796?originEvent=12419058503',
'CCAmericanExpressCard548?originEvent=12419059540',
'CCBankofAmerica07?originEvent=12419059816',
'CCCapitalOne649?originEvent=12419060962',
'CCCapitalOne648?originEvent=12419061242',
'CCUSAA10?originEvent=12419079178',
'CCCapitalOne1012?originEvent=12419079463',
'CCAmericanExpressCard154?originEvent=12419080432',
'CCBarclays1201?originEvent=12419080734',
'chaseFreedom?originEvent=12419082161',
'coJourney?originEvent=12419082527',
'CCBarclays1205?originEvent=12419084020',
'CCCitiBank1299?originEvent=12419084300',
'CCCapitalOne1076?originEvent=12419085356',
'CCBankofAmerica03?originEvent=12419085650',
'capitalbank01?originEvent=12419086955',
'CCMerrickBank1301?originEvent=12419087280',
'CCBarclays1244?originEvent=12419088852',
'CCCitiBank211?originEvent=12419089191',
'CCCapitalOnePQ?originEvent=12419110748',
'CCBarclays1300?originEvent=12419110918',
'CCCitiBank1317?originEvent=12419112198',
'CCCapitalOne1017?originEvent=12419112464',
'CCBankofAmerica02?originEvent=12419113380',
'CCAmericanExpressCard832?originEvent=12419113686',
'CCCapitalOne1015?originEvent=12419114679',
'CCAmericanExpressCard923?originEvent=12419114972',
'CCCitiBank1312?originEvent=12419116064',
'CCWellsFargo04?originEvent=12419116372',
'CCCitiBank1308?originEvent=12419117325',
'CCWellsFargo03?originEvent=12419117616',
'USBank04?originEvent=12419118642',
'chaseSlate?originEvent=12419119402',
'CCCitiBank1306?originEvent=12419120448',
'CCBankofAmerica01?originEvent=12419137754',
'CCCitiBank1200?originEvent=12419138024',
'FirstAccess01?originEvent=12419139154',
'primorSecuredVisaGoldCard?originEvent=12419139404',
'CCBankofAmerica11?originEvent=12419140448',
'CCWellsFargo02?originEvent=12419140799',
'CCUSAA013?originEvent=12419141785',
'CCAmericanExpress22034405?originEvent=12419142013',
'CCCreditOneBank01?originEvent=12419142983',
'CCCitiBank1859?originEvent=12419143279',
'FirstProgress01?originEvent=12419144254',
'FirstProgress02?originEvent=12419144571',
'FirstProgress03?originEvent=12419145482',
'inkBusiness?originEvent=12419145765',
'CCCitiBank1307?originEvent=12419163648',
'CCAmericanExpressCard1245?originEvent=12419163855',
'CCBankofAmerica06?originEvent=12419164818',
'CCBarclays1401?originEvent=12419165115',
'CCAmericanExpress22034407?originEvent=12419166164',
'USBank05?originEvent=12419166801',
'Milestonegold?originEvent=12419167834',
'TotalVisa?originEvent=12419168116',
'CCBarclays1246?originEvent=12419169083',
'CCCitiBank127?originEvent=12419169406',
'IndigoPlatinum?originEvent=12419170544',
'CCUSAA05?originEvent=12419170871',
'CCBankofAmerica08?originEvent=12419185056',
'CCBankofAmerica15?originEvent=12419185351',
'CCBankofAmerica16?originEvent=12419186243',
'USBank14?originEvent=12419186495',
'CCAmericanExpressCard196?originEvent=12419187396',
'CCCapitalOne1020?originEvent=12419187762',
'LuxuryCard01?originEvent=12419188878',
'CCCitiBank1316?originEvent=12419189155',
'CCCitiBank1315?originEvent=12419190133',
'LuxuryCard03?originEvent=12419190405',
'CCAmericanExpressCard160?originEvent=12419191453',
'CCAmericanExpressCard781?originEvent=12419191740',
'CCCitiBank1201?originEvent=12419204966',
'USBank21?originEvent=12419205242',
'CCCreditOneBank04?originEvent=12419206188'
]

    extracted_data = [ ]
    for i in CredCardList:
        url = "https://www.creditkarma.com/creditcard/" + i
        print
        "Processing: " + url
        extracted_data.append ( credKarmaParser ( url ) )
        sleep ( 5 )
    f = open ( 'credKarma_file.json', 'w' )
    json.dump ( extracted_data, f, indent=4 )


if __name__ == "__main__":
    readCredCard ( )

print('__web_scraping_done__')

# read json file
file_01 = pd.read_json('credKarma_file.json')
# output json to csv
file_02 = file_01.to_csv('CreditKarma_CC_Info.csv')

print('__done__')