# This code filters for relevant credit cards per user input, gets the latest tweets on each credit card and applies a sentiment on them
# The output is 1) a full list of all relevant credit cards
#               2) a list of the most positive 10 credit cards
#               3) a list of the top 5 most positive tweets about each credit card
#               4) a list of the top 5 most negative tweets about each credit card

# PARAMETERS
max_annual_fee = 0 # This is in $
min_annual_fee = 0 # This is in $
max_cash_advance_rate = 100 # This is in %
min_cash_advance_rate = 0 # This is in %
max_balance_transfer_rate = 100 # This is in %
min_balance_transfer_rate = 0 # This is in %
max_apr = 100 # This is in %
min_apr = 16 # This is in %
min_star_rating = 0 
min_reviews = 0


# Imports
import pandas as pd
import numpy

# let's make our program compatible with Python 3.0/1/2/3
from __future__ import division, print_function
from future_builtins import ascii, filter, hex, map, oct, zip 

import re  # regular expressions
import nltk  # draw on the Python natural language toolkit
from nltk.corpus import PlaintextCorpusReader
from twython import Twython
import sys
import string
import simplejson
import json
import datetime

import TwitterAPI
from TwitterAPI import TwitterAPI, TwitterOAuth, TwitterRestPager


# Load Data
attr_df = pd.read_csv('CreditKarma_CC_OfferSpec_Extract.csv')
star_df = pd.read_csv('CreditKarma_CC_Star_Reviews_Extract.txt', sep='\t')
positive_words = pd.read_csv('positive_words.txt', sep='\t')
negative_words = pd.read_csv('negative_words.txt', sep='\t')
liu_positive_words = pd.read_csv('./000_sentiment_jump_start/Hu_Liu_positive_word_list.txt', sep='\t', header=None)
liu_negative_words = pd.read_csv('./000_sentiment_jump_start/Hu_Liu_negative_word_list.txt', sep='\t', header=None)

liu_positive_words.columns = ['word_list']
liu_negative_words.columns = ['word_list']


# Helper functions
def isRelevant(name, relevant_list):
    return any(word in name.lower() for word in relevant_list) > 0

# The below is used to extract only the values with a % in a list
def isPercent(list):
    return [x for x in list if '%' in x]

# The below is used to compare the actual input with each of the values in any of the $ value columns of df
def compareFee(act_fee, min_fee, max_fee):
    fee = 0 if act_fee == '' else float(act_fee.replace('$', '').split('-')[0])
    return max_fee >= fee and min_fee <= fee

# The below is used to compare the actual input with each of the values in any of the % value columns of df
def compareRate(act_fee, min_fee, max_fee):
    if(isinstance(act_fee, list)):
        fee = float((act_fee[0]).replace('%', '')) if len(act_fee) > 0 else 0
    else:
        fee = 0 if act_fee == '' else float(act_fee.replace('%', ''))
    return max_fee >= fee and min_fee <= fee

# The below is used to compare actual input with each of the value in any of the integer columns of df
def compareInt(min_fee, act_fee):
    fee = act_fee if isinstance(act_fee, int) or isinstance(act_fee, float) else 0
    return min_fee >= fee

# The below is used to extract tweets for each of the relevant credit cards
def getTweets(search):
    #TwitterAPI(consumer_key, consumer_secret, access_token_key, access_token_secret)
    twitter = TwitterAPI('nFAN3KefmjSIZQQVfLpM742Kp',
        'm2pDfA6IWbT6HE1vqC6ZBuob2tthyoOccclbB50ALBeGfOFIRC',
        '267491009-igAP7cHDpJCsCtW0tWk3uvxkUiorP03hL3q7Bsvd',
        'x72nzS9XTP0SYiGFhGkj3MFlkM5ogIjo5V8s0V9wW0ZHm')
    
    r = twitter.request('search/tweets', {'q': search,'count':100})

    my_tweets = []
    if(r.status_code == 200):
        for item in r.get_iterator():
            my_tweets.append(item['text'])
    else: print("Couldn't connect to Twitter")
    
    print(search)
    return my_tweets

# Function to clean up the tweets
def text_parse(string):
    # Get a list of stop words
    stop_words = nltk.corpus.stopwords.words('english')
    codelist = ['\r', '\n', '\t']
    # replace non-alphanumeric with space
    temp_string = re.sub('[^a-zA-Z]', ' ', string)
    # replace codes with space
    for i in range(len(codelist)):
        stopstring = ' ' + codelist[i] + ' '
        temp_string = re.sub(stopstring, ' ', temp_string)
    # replace single-character words with space
    temp_string = re.sub('\s.\s', ' ', temp_string)
    # convert uppercase to lowercase
    temp_string = temp_string.lower()
    # replace selected character strings/steo-words with space
    for i in range(len(stop_words)):
        stopstring = ' ' + str(stop_words[i]) + ' '
        temp_string = re.sub(stopstring, ' ', temp_string)
    # replace multiple blank characters with one blank character
    temp_string = re.sub('\s+', ' ', temp_string)
    return(temp_string)

# Define counts of positive or negative words
def sentiment_score(text, word_list):
    count = 0
    all_words = [w for w in text.split()]
    for word in all_words:
        count = count + any(word in s for s in word_list)
    return(round(count/len(all_words), 4))


## DATA CLEANSING
base_url = "https://www.creditkarma.com/reviews/credit-card/single/id/"
star_df['Clean Title'] = [unicode(x, errors='ignore') for x in star_df['Title']]
attr_df['Clean Title'] = [unicode(x, errors='ignore') for x in attr_df['NAME']]
input_df = pd.merge(attr_df, star_df, how='outer', on='Clean Title')

df = pd.DataFrame()
df['Name'] = input_df['Clean Title']
df['First Year Fee'] = [(str(x).split(' ')[0])
                        .replace('Introductory', '')
                        .replace('See', '')
                        .replace(';', '')
                        .replace('*', '')
                        .replace(',', '')
                        .replace('nan', '')
                  for x in (input_df['ANNUAL_FEE'])]
df['Cash Advance Rate'] = [(str(x).split(' ')[0]).replace('*', '').replace('See', '').replace('nan', '')
                  for x in (input_df['CASH_ADVANCE_RATE'])]
df['Balance Transfer Rate']  = [isPercent(str(x).replace('*', '').split(' '))
                  for x in input_df['BALANCE_TRANSFER_REGULAR_APR']] ## Note: Returns a list of lists
df['Regular APR'] = [isPercent(str(x).replace('*', '').replace('See Terms', '').split('-')) 
                  for x in input_df['PURCHASE_REGULAR_APR']] ## Note: Returns a list of lists
df['Star Rating'] = [str(x).replace('outof5stars', '').replace('nan', '') for x in input_df['Star Rating']]
df['# of Reviews'] = [str(x).replace('Review', '').replace('nan', '') for x in input_df['# of Reviews']]


# Filter the list further for relevant results
relevant_list = ['american', 'amex', 'citi', 'chase']
df = df[[isRelevant(x, relevant_list) for x in df['Name']]]
df = df.reset_index(drop=True)


###############################################
# Step 1: Filter for the relevant results     #
#                                             #
###############################################
# 
right_fee = [compareFee(x, min_annual_fee, max_annual_fee) for x in df['First Year Fee']]
right_advance = [compareRate(x, min_cash_advance_rate, max_cash_advance_rate) for x in df['Cash Advance Rate']]
right_transfer = [compareRate(x, min_balance_transfer_rate, max_balance_transfer_rate) for x in df['Balance Transfer Rate']]
right_apr = [compareRate(x, min_apr, max_apr) for x in df['Regular APR']]
right_stars = [compareInt(min_star_rating, x) for x in df['Star Rating']]
right_reviews = [compareInt(min_reviews, x) for x in df['# of Reviews']]
results = df[[a and b and c and d and e and f 
             for a, b, c, d, e, f in zip(right_fee, right_advance, right_transfer, right_apr, right_stars, right_reviews)]]
results = results.reset_index(drop=True)


###############################################
# Step 2: Sentiment Analysis on Tweets        #
#                                             #
###############################################
# Create a list of positive and negative words
positive_word_list = positive_words['positive_words[-which(positive_words %in% negative_words)]'].append(liu_positive_words['word_list'])
negative_word_list = negative_words['negative_words[-which(negative_words %in% positive_words)]'].append(liu_negative_words['word_list'])
negative_word_list = [unicode(x, errors='ignore') for x in negative_word_list]

# Pull all relevant tweets for each credit card name
tweet_list = [getTweets(x) for x in results['Name']]

# Clean up the tweet list and extract lists of lists
tweet_list_clean = []
for i in range(len(tweet_list)):
    bank_name = results['Name'][i]
    for tweet in tweet_list[i]:
        tweet_list_clean.append((bank_name, text_parse(tweet)))

# Create a dataframe with positive and negative score for each credit card tweet
positive_scores = pd.DataFrame([(x[0], x[1], sentiment_score(x[1], positive_word_list)) for x in tweet_list_clean])
negative_scores = pd.DataFrame([(x[0], x[1], sentiment_score(x[1], negative_word_list)) for x in tweet_list_clean])

sentiment_df = pd.merge(positive_scores, negative_scores, how='inner', on=1)
sentiment_df.columns = ['Credit Card Name', 'Tweet', 'Positive Score', 'Credit Card Name Again', 'Negative Score']
sentiment_df['Overall Score'] = sentiment_df['Positive Score'] - sentiment_df['Negative Score']

# Create a dataframe with the top credit card results based on average sentiment score
top_results = sentiment_df.groupby(by=['Credit Card Name']).agg({'Overall Score':numpy.mean}).sort('Overall Score', ascending=False)[:10]

# Create a mapping between the modified tweets and original
tweet_mapping = []
for i in range(len(tweet_list)):
    bank_name = results['Name'][i]
    for tweet in tweet_list[i]:
        tweet_mapping.append((tweet, text_parse(tweet)))

tweet_map_df = pd.DataFrame(tweet_mapping)
tweet_map_df.columns = ['Raw Tweet', 'Tweet']

# Add the raw tweets to the sentiment dataframe
final_sentiment_df = pd.merge(sentiment_df, tweet_map_df, how='inner', on='Tweet')
# Create a final dataframe with star ratings, 
final_results = pd.merge(results.rename(columns={'Name': 'Credit Card Name'}), final_sentiment_df, how='outer', on='Credit Card Name')
# Create a dataframe with the top 5 positive/negative tweets for each credit card
top_positive = final_results.sort('Positive Score', ascending=False).groupby('Credit Card Name').head(5)
top_negative = final_results.sort('Negative Score').groupby('Credit Card Name').head(5)

# Save results to file
user_type = 'credit_seeker'
all_cc_file = user_type + '_all_cc.csv'
top_results_file = user_type + '_top10_results.csv'
positive_tweets_file = user_type + '_positive_tweets.csv'
negative_tweets_file = user_type + '_negative_tweets.csv'

results.sort('Star Rating', ascending=False, na_position='last').to_csv(all_cc_file)
top_results.to_csv(top_results_file)
top_positive[['Credit Card Name', 'Raw Tweet']].to_csv(positive_tweets_file, encoding='utf-8')
top_negative[['Credit Card Name', 'Raw Tweet']].to_csv(negative_tweets_file, encoding='utf-8')


# Display the results
results['Star Rating'] = [x.replace('nan', '') for x in results['Star Rating']]
print("The relevant result-set per the criteria is: ")
results.sort('Star Rating', ascending=False, na_position='last')

print("The top 10 recommended credit cards and their relative sentiment score are:")
top_results

print("The top 5 most positive tweets for each of the relevant credit cards are: ")
top_positive[['Credit Card Name', 'Raw Tweet']]

print("The top 5 most negative tweets for each of the relevant credit cards are: ")
top_negative[['Credit Card Name', 'Raw Tweet']]
