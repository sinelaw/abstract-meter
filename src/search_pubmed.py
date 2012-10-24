import urllib
import amara
import lxml.html
import tempfile
import csv
import codecs
import cStringIO
import os
import re
import sqlite3
#import pprint
from time import sleep


XMLS_OUTPUT_DIR = 'data/xmls'

import os, errno

# thanks to http://stackoverflow.com/a/600612/562906
def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

def log(*args):
    import sys
    print >>sys.stderr, ' '.join(map(unicode, args))

def getXmlValue(root, tagname):
    for elem in root.xml_select(tagname):
        for xml_child in elem.xml_children:
            return xml_child.xml_value.strip()
    return ''

def getSearchWebEnv(mindate, maxdate):
    for datetext in [mindate, maxdate]:
        if (not re.match(r'\d{4}/\d{2}(/\d{2}){0,1}', datetext)):
            raise Exception('Date string must be YYYY/MM or YYYY/MM/DD. Invalid date string: %s' % (datetext,))
    query_params = {'db':'pubmed', 
                    'usehistory':'y',
                    'term': 'e[abstract]', # dummy term to prevent articles with empty abstract
                    'datetype':'pdat',
                    'mindate':mindate,
                    'maxdate':maxdate}
    query = '&'.join(map(lambda (a,b): a+'='+b, query_params.items()))
    url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?' + query
    log('getting search url...', url)
    data = urllib.urlopen(url).read()

    #fd, name = tempfile.mkstemp(suffix='.xml')
    #os.fdopen(fd, 'w').write(data)

    #log('done. Search data saved to: ' + name)
    search_data = amara.parse(data, standalone=True)
    webEnv_str = search_data.xml_select('.//WebEnv').asString()
    query_key = search_data.xml_select('.//QueryKey').asString()
    count = int(getXmlValue(search_data.xml_select('.//eSearchResult')[0], './/Count'))
    return query_key, webEnv_str, count

def fetchWebEnv(query_key, web_env, start_result, num_results, file_suffix):
    url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&mode=xml&query_key=%s&WebEnv=%s&retstart=%d&retmax=%d' % (query_key, web_env, start_result, num_results)
    log('getting fetch url...', url)
    while True:
        sleep(0.5) # Don't do more than twice a second...
        data = urllib.urlopen(url).read()
        if len(data) < 100:
            if 'Unable to obtain query #1' in data:
                log('got error, retrying...')
                continue
        break
    mkdir_p(XMLS_OUTPUT_DIR)
    fd, name = tempfile.mkstemp(prefix='%s' % (file_suffix,), suffix='.xml', dir=XMLS_OUTPUT_DIR)
    os.fdopen(fd, 'w').write(data)

    log('done, xml saved to "%s"' % (name,))

def show_date(dateStr):
    return dateStr.replace("/","-")

def getResults(mindate, maxdate, start_chunk = 1):
    chunk_size = 1000
    start_chunk = int(start_chunk) - 1
    query_key, webEnv, count = getSearchWebEnv(mindate, maxdate)
    log('Total results:', count)
    num_chunks = (count / chunk_size) + 1
    results = {}

    for i in xrange(start_chunk, num_chunks):
        log('Chunk ', i+1, 'of', num_chunks)
        fetchWebEnv(query_key, webEnv, i*chunk_size, chunk_size, '%s-to-%s_%d_of_%d' % (show_date(mindate), show_date(maxdate), i+1, num_chunks))

    log('Done.')

if __name__ == '__main__':
    import sys
    args = sys.argv[1:]
    if (len(args) < 2):
        log('Usage: %s <start date YYYY/MM/DD> <end date YYYY/MM/DD> [start chunk number, default=0]' % (sys.argv[0],))
        sys.exit(1)
    getResults(*args)
