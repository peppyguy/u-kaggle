"""
Homework 2

Harkirat Gill
"""
import numpy as np
import pandas as pd

import requests
from bs4 import BeautifulSoup

from nltk.tokenize import sent_tokenize,word_tokenize
from nltk.corpus import stopwords
from string import punctuation
import nltk

from nltk.probability import FreqDist

from heapq import nlargest

# Needed this in my IDE
#nltk.download('punkt')
#nltk.download('stopwords')

# Run code on default article
print_summary()

# Print out top three sentences of an article and output the metrics of the output
def print_summary(url="https://arstechnica.com/cars/2018/10/honda-will-use-gms-self-driving-technology-invest-2-75-billion/"):
    processed = process_article(url)
    metrics = compute_metrics(processed[0], processed[1])
    
    print("\nTop Sentences\n")
    print(processed[0][0])
    print("...")
    print(processed[0][1])
    print("...")
    print(processed[0][2])
    
    # Prints out word count of article, percentage of the article each sentence takes up, and recuction in size of reading by the summary as metrics for how efficient the summarizer was
    print("\nMetrics of Summarization\n")
    print("Word Count        : " + str(metrics[0]))
    print("Protion of First  : " + str(round(100 * metrics[1], 2)) + "%")
    print("Protion of Second : " + str(round(100 * metrics[2], 2)) + "%")
    print("Protion of Third  : " + str(round(100 * metrics[3], 2)) + "%")
    print("Reduction in Size : " + str(round(100 - (100 * metrics[4]), 2)) + "%")
    
# Parse article and return top three sentences based on highest density of popular words
def process_article(url):
    articleURL=url
    response = requests.get(articleURL)
    response.encoding = 'utf-8'
    data = response.text
    soup = BeautifulSoup(data)
    #print(soup)
    
    soup.find('article').text
    
    text = ' '.join(map(lambda p: p.text, soup.find_all('article')))
    #print(text)
    
    text.encode('ascii', 'ignore')
    #print(text)
    
    ####
    
    sents = sent_tokenize(text)
    #print(sents)
    
    word_sent = word_tokenize(text.lower())
    #print(word_sent)
    
    _stopwords = set(stopwords.words('english') + list(punctuation))
    #print(_stopwords)
    
    # Filter out stopword
    word_sent=[word for word in word_sent if word not in _stopwords]
    #print(word_sent)
    
    freq = FreqDist(word_sent)
    #print(freq)
    
    nlargest(10, freq, key=freq.get)
    
    # We want to create a signifcant score ordered by highest frequency
    from collections import defaultdict
    ranking = defaultdict(int)
    for i,sent in enumerate(sents):
        for w in word_tokenize(sent.lower()):
            if w in freq:
                ranking[i] += freq[w]
    #print(ranking)
    
    # Top 3 Sentences
    sents_idx = nlargest(4, ranking, key=ranking.get)
    #print(sents_idx)
    
    # Clean sentences by removing new lines and extra spaces
    sents = cleanSents(sents)
    
    # Collect top three sentences and order them to keep structure of article preserved
    top_idx = [sents_idx[0], sents_idx[1], sents_idx[2]]
    top_idx.sort()
    top = (sents[top_idx[0]], sents[top_idx[1]], sents[top_idx[2]])
    
    # Return line numbers of the top three sentences as well as the entire text of the article for use in calculaating metrics
    return (top, text)

# Clean each sentence
def cleanSents(sents):
    new_sents = []
    for s in sents:
        new_sents.append(clean(s))
    return new_sents

# Remove new lines and extra spaces
def clean(s):
    s = s.replace("\n", "")
    l = list(s)
    i = 0
    while i < len(l):
        if (l[i] == " " and l[i + 1] == " "):
            l[i:i + 1] = ""
        else : 
            i += 1
        
    return "".join(l)

# Compute and return metrics
# The best measure I could think of was efficiency. Meaning, what percentage of the article is the summary
def compute_metrics(top_sents, text):
    length = len(text)
    eff_1 = len(top_sents[0]) / length
    eff_2 = len(top_sents[1]) / length
    eff_3 = len(top_sents[2]) / length
    total_eff = eff_1 + eff_2 + eff_3
    return(length, eff_1, eff_2, eff_3, total_eff)
    
    
