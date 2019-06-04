
# coding: utf-8

# # INITIAL CREDITKARMA DATA EXPLORATION
# Imports
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import plotly.plotly as py


# Read in data
df = pd.read_csv('CreditKarma_CC_Info.csv')
star_df = pd.read_csv('CreditKarma_CC_AvgReview.txt', sep='\t')


# In[3]:

## Data Cleanup
df['Bank'] = 'null'
df.ix[df['NAME'].str.contains('Bank of America'), 'Bank'] = 'Bank of America'
df.ix[df['NAME'].str.contains('American Express'), 'Bank'] = 'American Express'
df.ix[df['NAME'].str.contains('Chase'), 'Bank'] = 'Chase'
df.ix[df['NAME'].str.contains('Citi'), 'Bank'] = 'Citi'

star_df['Star Rating'] = star_df['Star Rating'].str.replace('outof5stars', '').astype(float)
star_df['# of Reviews'] = star_df['# of Reviews'].str.replace('Review', '')
star_df['# of Reviews'] = star_df['# of Reviews'].fillna(0).astype(int)

star_df['Name'] = star_df['Bank']
star_df['Bank'] = 'null'
star_df.ix[star_df['Name'].str.contains('bank-of-america'), 'Bank'] = 'Bank of America'
star_df.ix[star_df['Name'].str.contains('bankofamerica'), 'Bank'] = 'Bank of America'
star_df.ix[star_df['Name'].str.contains('americanexpress'), 'Bank'] = 'American Express'
star_df.ix[star_df['Name'].str.contains('american-express'), 'Bank'] = 'American Express'
star_df.ix[star_df['Name'].str.contains('chase'), 'Bank'] = 'Chase'
star_df.ix[star_df['Name'].str.contains('citi'), 'Bank'] = 'Citi'


# ## OFFERINGS
# Create a dataframe with average star ratings
offerings = star_df.groupby(['Bank']).aggregate(np.count_nonzero)
offerings = offerings[offerings.index != 'null']

# Create a dictionary between bank and # of offerings for plotting
offer_dict = {}
for i in range(len(offerings.index)):
    offer_dict[offerings.index[i]] = offerings['Name'][i]


# AVERAGE STAR RATING PLOT
mpl_fig = plt.figure()
ax = mpl_fig.add_subplot(111)

N = 4
ind = np.arange(N)    # the x locations for the groups
width = 0.35       # the width of the bars: can also be len(x) sequence

ax.bar(range(len(offer_dict)), offer_dict.values(), align='center')

ax.set_ylabel('# of Offerings')
ax.set_xlabel('Bank')
ax.set_title('Offerings by Bank')

ax.set_xticks(ind + width/2.)
#ax.set_yticks(np.arange(0, 81, 10))
ax.set_xticklabels(offerings.index)

plt.savefig('CreditKarma_CC_OfferingsComparison.png')


# ## TOTAL REVIEWS
# Create a dataframe with average star ratings
reviews = star_df.groupby(['Bank']).sum()
reviews = reviews[reviews.index != 'null']

# Create a dictionary between bank and # of offerings for plotting
review_dict = {}
for i in range(len(reviews.index)):
    review_dict[reviews.index[i]] = reviews['# of Reviews'][i]


# AVERAGE STAR RATING PLOT
mpl_fig = plt.figure()
ax = mpl_fig.add_subplot(111)

N = 4
ind = np.arange(N)    # the x locations for the groups
width = 0.35       # the width of the bars: can also be len(x) sequence

ax.bar(range(len(review_dict)), review_dict.values(), align='center')

ax.set_ylabel('# of Reviews')
ax.set_xlabel('Bank')
ax.set_title('Total Reviews by Bank')

ax.set_xticks(ind + width/2.)
#ax.set_yticks(np.arange(0, 81, 10))
ax.set_xticklabels(reviews.index)

plt.savefig('CreditKarma_CC_ReviewsComparison.png')


# ## STAR RATING
# Create a dataframe with average star ratings
star_averages = star_df.groupby(['Bank']).aggregate(np.average)
star_averages = star_averages[star_averages.index != 'null']

# Create a dictionary between bank and average star rating for plotting
bank_dict = {}
for i in range(len(star_averages.index)):
    bank_dict[star_averages.index[i]] = star_averages['Star Rating'][i]


# AVERAGE STAR RATING PLOT
mpl_fig = plt.figure()
ax = mpl_fig.add_subplot(111)

N = 4
ind = np.arange(N)    # the x locations for the groups
width = 0.35       # the width of the bars: can also be len(x) sequence

ax.bar(range(len(bank_dict)), bank_dict.values(), align='center')

ax.set_ylabel('Average Star Rating')
ax.set_xlabel('Bank')
ax.set_title('Average Star Rating by Bank')

ax.set_xticks(ind + width/2.)
#ax.set_yticks(np.arange(0, 81, 10))
ax.set_xticklabels(star_averages.index)

plt.savefig('CreditKarma_CC_StarComparison.png')




