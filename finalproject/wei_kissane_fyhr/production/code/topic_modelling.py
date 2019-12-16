"""
This file is responsible for topic modelling
"""

import pandas as pd
import numpy as np
import re
from gensim import corpora, models, similarities
import gensim
import nltk
nltk.download('punkt')
nltk.download('wordnet')
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords
import pyLDAvis.gensim

import data_parser as db


"""#############################################################################################"""
"""
                                        GETTING EMAILS
"""
"""#############################################################################################"""
# Get Emails, From Community
def get_community_emails(comm_id):
    return db.get_community_emails(comm_id)
    
# Get Emails, From Sub-Community
def get_sub_community_emails(comm_id, sub_comm_id):
    return db.get_sub_community_emails(comm_id, sub_comm_id)

# Get Important Emails, From Topics & Emails
def get_most_relevant_emails(emails, topics, n=30):
    # make key word bank from topics
    key_word_bank = list([])
    for topic in topics:
        words = topic[1].split("+")
        for word in words:
            key = word.split("*")[1]
            key = key.replace('"','')
            key_word_bank.append(key)

    print(key_word_bank)

    # make list of tuple pairs of email_topic_count & email_id
    email_dict = dict()
    for email in emails:
        # count key words in emails
        text = str(email[5])

        wordlist = [word for word in word_tokenize(text.lower()) if word.isalpha()]
                
        matches = 0
        for word in wordlist:
            for key in key_word_bank:
                if key == word:
                    matches += 1

        email_dict[email[0]] = [int(email[0]), matches]

    # sort list based on email_topic_count
    pairs = list(email_dict.values())
    m = len(pairs)
 
    # Traverse through all array elements
    for i in range(m):
 
        # Last i elements are already in place
        for j in range(0, m-i-1):
 
            # traverse the array from 0 to n-i-1
            # Swap if the element found is greater
            # than the next element
            if pairs[j][1] < pairs[j+1][1]:
                temp_pair = pairs[j].copy()

                pairs[j] = pairs[j+1].copy()
                pairs[j+1] = temp_pair
    
    # take 'n' emails with the highest email_topic_count
    most_relevant = list([])
    for i in range(n):
        most_relevant.append(pairs[i][0])

    # return these emails
    return most_relevant.copy()

"""#############################################################################################"""
"""
                                        GETTING TOPICS
"""
"""#############################################################################################"""
# Get Topics, From Emails
def do_topic_modelling( emails, n_topics, n_words=10, debug=True ):

    # define stopwords
    stopwords = nltk.corpus.stopwords.words('english')
    stopwords.append("thank")
    stopwords.append("thanks")

    # define lemmatizer
    lemmatizer = WordNetLemmatizer()

    # gather valid words from emails
    wordlist = list([])
    for email in emails:
        text = str(email[5])
        
        alpha_only = [word for word in word_tokenize(text.lower()) if word.isalpha()]
        non_stopwords = [word for word in alpha_only if word not in stopwords]
        lemmatized = [lemmatizer.lemmatize(word) for word in non_stopwords]
        valid_words = [word for word in lemmatized if len(word) > 3]
                
        wordlist.append(valid_words)

    # prepare LDA input
    dictionary = corpora.Dictionary(wordlist)
    corpus = [dictionary.doc2bow(text) for text in wordlist]

    print("Stopwords : {}".format(len(stopwords)))
    print("Words : {}".format(len(dictionary)))
    print("Emails : {}".format(len(corpus)))
    
    # perform LDA
    lda = gensim.models.ldamodel.LdaModel(corpus=corpus, id2word=dictionary, num_topics=n_topics)
    topics = lda.print_topics(num_topics=n_topics, num_words=n_words)

    for topic in topics:
        words_only = list([])
        words = topic[1].split("+")
        for word in words:
            key = word.split("*")[1]
            words_only.append(key)
        print("Topic {} : {}.".format(topic[0],words_only))
            

    # print 'n' topics; if debug
    #if debug:
        #show_topic_modelling(lda, corpus, dictionary)

    # get top 30 emails by id
    top_30_email_ids = get_most_relevant_emails(emails, topics)
    print("Top Emails : {}.".format(top_30_email_ids))

    # return LDA topics
    return None

"""#############################################################################################"""
"""
                                        SHOWING TOPICS
"""
"""#############################################################################################"""
def show_topic_modelling( lda, corpus, dictionary ):

    # prepare visualizations
    lda_display = pyLDAvis.gensim.prepare(lda, corpus, dictionary, sort_topics=False)

    # make visualization of topics
    pyLDAvis.display(lda_display)

    # return nothing
    return


# Community 0
comm_id = 0
comm_emails = get_community_emails(comm_id)
do_topic_modelling(comm_emails, 3)
"""
# Community 2
comm_id = 2
comm_emails = get_community_emails(comm_id)
do_topic_modelling(comm_emails, 3)
"""
# Community 5
comm_id = 5
comm_emails = get_community_emails(comm_id)
do_topic_modelling(comm_emails, 3)

"""
comms = [0,2,5]
sub_comm_count = 4
for comm_id in comms:
    for sub_comm_id in range(sub_comm_count):
        print("\nAnalysis : Community {} : Sub-Community {}.".format(comm_id,sub_comm_id))
        comm_emails = get_sub_community_emails(comm_id, sub_comm_id)
        do_topic_modelling(comm_emails, 3)
        print()
"""
        
    
