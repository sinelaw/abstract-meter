import sqlite3
import re

import sys

nonletters_pattern = re.compile('[^a-zA-Z \t]*')
max_frequency = 1
min_frequency = 0

def filter_words(word):
    if (len(word) < 3):
        return False
    return True

def uniq(inlist): 
    # order preserving
    uniques = {}
    for item in inlist:
        if item not in uniques:
            uniques[item] = 0
    return uniques.keys()

def build_histogram_from_db(sqlite_filename):
    conn = sqlite3.connect(sqlite_filename)
    c = conn.cursor()

    # Get data
    results = c.execute('''SELECT abstract_text FROM results''')

    histogram = {}
    for abstract in results:
        text = nonletters_pattern.sub('', abstract[0].encode('ascii', 'ignore'))
        for word in uniq(filter(filter_words, text.lower().split())):
            histogram[word] = 1 + histogram.setdefault(word, 0)
        

    # We can also close the cursor if we are done with it
    c.close()

    return histogram

def main(sqlite_filename):
    histogram = build_histogram_from_db(sqlite_filename)
    print 'window.words_histogram = ['
    total_words = float(len(histogram.keys()))
    for (word,num) in histogram.items():
        if word.endswith('s') and word[:-1] in histogram:
            histogram[word[:-1]] += num
            del histogram[word]
            continue
        pass

    for (word,num) in histogram.iteritems():
        frequency = num / total_words
        if num < 3:
            continue
        if frequency < min_frequency:
            continue
        if frequency > max_frequency:
            continue
        print "['" + word + "', " + str(num) + "],"
    print '];'

    
if __name__=='__main__':
    main(*sys.argv[1:])
