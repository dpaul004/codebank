## This code will extract reviews and star ratings from CreditKarma.com
# The output will consist of 2 files: one with the reviews and the star rating given for the review
# The other is the average rating for each of the credit card offerings

# prepare for Python version 3x features and functions
from __future__ import division, print_function
import math 

# import packages for web scraping/parsing
import requests  # functions for interacting with web pages
from lxml import html  # functions for parsing HTML
from bs4 import BeautifulSoup  # DOM html manipulation
import xml.etree.ElementTree # for parsing the xml site-map
import pandas as pd # to create a data frame for pretty output in the end
from os import write

# Load up the CreditKarma sitemap in order to get a full list of available credit cards to parse
e = xml.etree.ElementTree.parse('CreditKarma_SiteMap.xml').getroot()

# Create a list of URLs based on the sitemap above
url_list = []
for i in range(len(e)):
    url = e[i][0].text
    if("credit-card/single/id/" in url):
        if("citi" in url or "amex" in url or "america" in url or "chase" in url):
            url_list.append(url)

# Define the base URL 
base_url = "https://www.creditkarma.com/reviews/credit-card/single/id/"


#### PART 1: EXTRACT REVIEWS ####
# Create a helper function to get the review, star rating, marked helpful/not and review date from each URL
def get_reviews(full_url): 
    print("Parsing the following page: ", full_url)
    web_page = requests.get(full_url)
    web_page_text = web_page.text
    
    my_soup = BeautifulSoup(web_page_text, "lxml")
    my_page = [x.extract() for x in my_soup.find_all('article')]
    try:
        review = [x.find_all("p") for x in my_page]
        stars = [(str(y[0]).split("\"")[1]).replace(" blue", "") for y in [x.find_all("span") for x in my_page] if "ck-stars" in str(y)]
        helpful = [x.find_all("div", { "class" : "helpful-wrap" })[0].text.split("Helpful")[1].split(" ")[0] for x in my_page]
        not_helpful = [x.find_all("div", { "class" : "helpful-wrap" })[0].text.split("Helpful")[2] for x in my_page]
        review_date = [x.find_all("span", { "class" : "review-date" })[0].text.replace(" ", "").replace("\n", "") for x in my_page]
        cc = url.replace(base_url, "")
    except IndexError:
        review = ''
        stars = 0
        helpful = 0
        not_helpful = 0
        review_date = ''
        cc = url.replace(base_url, "")

    res = []
    for j in range(len(review)):
        res.append((cc, review[j], stars[j], helpful[j], not_helpful[j], review_date[j]))
    return res

# Apply the above helper function to each of the URLs created in the list
all_reviews = [get_reviews(url) for url in url_list[:5]]


# Load the results into a pandas dataframe and save
df = pd.DataFrame()
for row in all_reviews:
    for review in row:
        df = pd.concat([df, pd.DataFrame([review])])

df.columns = ['Credit Card', 'Review', 'Star Rating', '# Marked Helpful', '# Marked Not Helpful', 'Review Date']
df[:10]
df.to_csv('CreditKarma_CC_IndividualReviews.txt', sep='\t')

#### PART 2: STAR RATINGS ####
# Define helper function to extract star ratings, number of reviews for each credit card 
def get_ratings(full_url): 
    web_page = requests.get(full_url)
    web_page_text = web_page.text
    
    my_soup = BeautifulSoup(web_page_text, "lxml")
    my_page = [x.extract() for x in my_soup.find_all('div', { "class" : "review-rating-widget"})]
    
    try:
        star_rating = [x.find_all("div", { "class" : "rating-stars"})[0].text for x in my_page][0]
        star_rating = star_rating.replace(' ','').replace('\n','')
        reviews = [x.find_all("span")[1].text for x in my_page][0]
        review = reviews.replace(' ', '').replace('Reviews', '').replace('\n', '')
        five_star_reviews = [x.find_all("li", { "id" : "star5"})[0] for x in my_page][0]
        four_star_reviews = [x.find_all("li", { "id" : "star4"})[0] for x in my_page][0]
        three_star_reviews = [x.find_all("li", { "id" : "star3"})[0] for x in my_page][0]
        two_star_reviews = [x.find_all("li", { "id" : "star2"})[0] for x in my_page][0]
        one_star_reviews = [x.find_all("li", { "id" : "star1"})[0] for x in my_page][0]

        five_star = get_star_reviews(five_star_reviews)
        four_star = get_star_reviews(four_star_reviews)
        three_star = get_star_reviews(three_star_reviews)
        two_star = get_star_reviews(two_star_reviews)
        one_star = get_star_reviews(one_star_reviews)
    except IndexError:
        star_rating = 0
        review = ''
        five_star = 0
        four_star = 0
        three_star = 0
        two_star = 0
        one_star = 0
        
    cc = full_url.replace(base_url, "")
    #print(cc)
    return cc, star_rating, review, five_star, four_star, three_star, two_star, one_star

def get_star_reviews(page):
    reviews = page.find_all("span")[0]
    if(len(reviews) < 2):
        return 0
    else:
        return reviews[1].text.replace(' ', '').replace('\n', '')

# Apply above helper function to each of the URLs in the list
star_output = [get_ratings(x) for x in url_list]

# Load the results into a pandas dataframe and save
df_star_output = pd.DataFrame(star_output, columns=['Bank', 'Star Rating', '# of Reviews', '% 5 Star Reviews', 
                                    '% 4 Star Reviews', '% 3 Star Reviews', '% 2 Star Reviews', '% 1 Star Reviews'])
df_star_output.to_csv('CreditKarma_CC_AvgReview.txt', sep='\t')



