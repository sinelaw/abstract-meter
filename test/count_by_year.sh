sqlite3 ../data/results.sqlite 'select pub_year, count(*) from results group by pub_year order by pub_year'
