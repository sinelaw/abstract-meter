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

RESULT_FILE = 'data/results.sqlite'
XMLS_OUTPUT_DIR = 'data/xmls'

columns = [('volume', 'text'),
           ('issue', 'text'),
           ('pub_year', 'integer'),
           ('pub_month', 'text'),
           ('journal_title', 'text'),
           ('article_title', 'text'),
           ('abstract_text', 'text'),
           ('affiliation',   'text'),
           ('abstract_hash', 'integer')]

column_headers = map(lambda c : c[0], columns)

import os, errno

# thanks to http://stackoverflow.com/a/600612/562906
def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

def fix_unicode(s):
    if type(s) == unicode:
        return s
    elif type(s) != str:
        return unicode(s)
    try:
        return s.decode('utf-8')
    except:
        log('Error decoding value:', type(s), repr(s))
        raise

            
def log(*args):
    import sys
    print >>sys.stderr, ' '.join(map(unicode, args))
    
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
    count = int(getXmlValue(search_data.xml_select('.//eSearchResult')[0], './/Count'))
    return webEnv_str, count

def fetchWebEnv(web_env, start_result, num_results):
    url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&mode=xml&query_key=1&WebEnv=%s&retstart=%d&retmax=%d' % (web_env, start_result, num_results)
    log('getting fetch url...', url)
    data = urllib.urlopen(url).read()
    mkdir_p(XMLS_OUTPUT_DIR)
    fd, name = tempfile.mkstemp(suffix='.xml', dir=XMLS_OUTPUT_DIR)
    os.fdopen(fd, 'w').write(data)

    log('done, xml saved to "%s". parsing...' % (name,))
    res = amara.parse(data, standalone=True)
    log('done parsing.')
    return res

def searchAndFetch(mindate, maxdate, start_result, num_results):
    return fetchWebEnv(getSearchWebEnv(mindate, maxdate), start_result, num_results)

def getXmlValue(root, tagname):
    for elem in root.xml_select(tagname):
        for xml_child in elem.xml_children:
            return xml_child.xml_value.strip()
    return ''

def authorDataToRow(author_data):
    return [author_data[k] for k in column_headers]
    
def extractInfo(fetch_result):
    log('Extracting data...')
    article_sets = fetch_result.xml_select('.//PubmedArticleSet')
    if (len(article_sets) == 0):
        log('no result')
        return []

    articles = article_sets[0].xml_select('PubmedArticle')
    return map(authorDataToRow, filter(lambda x: x is not None, map(process_article, articles)))

def strip_tags(text):
    return lxml.html.fromstring(text).text_content()

emailRegex = re.compile(r"(?:^|\s)(?P<email>[-a-z0-9_.]+@(?:[-a-z0-9]+\.)+[a-z]{2,6})\.?",re.IGNORECASE)

def process_article(article):
    article_meta = article.xml_select('.//Article')[0]
    journal_node = article_meta.xml_select('.//Journal')[0]
    journal_title = getXmlValue(journal_node, './/Title')
    journal_issue_node = journal_node.xml_select('.//JournalIssue')[0]

    journal_pub_date = journal_issue_node.xml_select('.//PubDate')[0]
    pub_year = getXmlValue(journal_issue_node, './/Year')
    pub_month = getXmlValue(journal_issue_node, './/Month')
    volume = getXmlValue(journal_issue_node, './/Volume')
    issue = getXmlValue(journal_issue_node, './/Issue')

    article_title = strip_tags(article_meta.xml_select('.//ArticleTitle')[0].xml_encode())
    affiliation = getXmlValue(article_meta, './/Affiliation')
    email_match = emailRegex.search(affiliation)
    if email_match:
        email = email_match.groupdict()['email']
    else:
        email = ''
    try:
        abstract_xml = article_meta.xml_select('.//AbstractText')[0]
    except IndexError:
        abstract_text = ''
    else:
        abstract_text = strip_tags(abstract_xml.xml_encode())

    if 0 == len(abstract_text.strip()):
        return None # don't care about articles with no abstract.

    return dict(volume = volume, 
                issue = issue,
                pub_year = pub_year,
                pub_month = pub_month,
                journal_title = journal_title,
                article_title = article_title,
                abstract_text = abstract_text,
                affiliation = affiliation,
                abstract_hash = hash(abstract_text))


def getArticleRows(webEnv, start_result=0, num_results = 10):
    return extractInfo(fetchWebEnv(webEnv, start_result, num_results))

def create_sql_connection():
    conn = sqlite3.connect(RESULT_FILE)
    c = conn.cursor()
    # Create table
    table_names = map(lambda x : x[0], c.execute("select name from sqlite_master where type = 'table'").fetchall())
    if 'results' not in table_names:
        c.execute(getCreateTableQuery('results', False))

    c.execute(getCreateTableQuery('temp_new_results', True))
    return (conn, c)
    
def getResults(mindate, maxdate):
    chunk_size = 10000
    webEnv, count = getSearchWebEnv(mindate, maxdate)
    log('Total results:', count)
    num_chunks = (count / chunk_size) + 1
    results = {}

    (conn, c) = create_sql_connection()

    for i in xrange(num_chunks):
        log('Chunk ', i, 'of', num_chunks)
        row_data = getArticleRows(webEnv, start_result = i*chunk_size, num_results = chunk_size)
        log('\tAbstracts: ', len(row_data))
        c.execute('DELETE FROM temp_new_results')
        c.executemany('INSERT INTO temp_new_results VALUES (' + ','.join(map(lambda x : '?', column_headers)) + ')', row_data)
        res = c.execute('INSERT INTO results SELECT * FROM temp_new_results WHERE abstract_hash NOT IN (SELECT abstract_hash FROM results)')
        log('\tInsert result: ', res.rowcount)
        # Save (commit) the changes
        conn.commit()

    c.execute('DROP TABLE temp_new_results')
    conn.commit()
    # We can also close the cursor if we are done with it
    c.close()

    log('Written.')

def getCreateTableQuery(table_name, is_temp):
    return 'CREATE ' + ('TEMP' if is_temp else '') + ' TABLE ' + table_name + ' (' \
        + (', '.join(map(lambda x : x[0] + ' ' + x[1], columns))) + ')'

if __name__ == '__main__':
    import sys
    args = sys.argv[1:]
    if (len(args) < 2):
        log('Usage: %s <start date YYYY/MM/DD> <end date YYYY/MM/DD>' % (sys.argv[0],))
        sys.exit(1)
    getResults(*args)
