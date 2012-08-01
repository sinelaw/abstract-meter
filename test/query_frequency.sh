#!/bin/bash
sqlite3 -csv ../data/results.sqlite "select results.pub_year, (100.0*count(*))/r2.total from results  join (select pub_year, count(*) as total from results group by pub_year) r2 on results.pub_year = r2.pub_year where results.abstract_text like \"$1\" and total > 1000 and results.pub_year!=\"\" group by results.pub_year"
