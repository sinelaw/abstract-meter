mongo < <(echo "load('query_frequency_mongo.js'); get_frequencies_for_all_years(2008,2010,[/brain/i, /cancer/i, /heart/i]);")
