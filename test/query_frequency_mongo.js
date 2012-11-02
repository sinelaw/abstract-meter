
// Returns an array of { year : '123', count : 123 }
function get_years()
{
    return db.articles.aggregate({$group:{year:"$PubmedArticle.MedlineCitation.Article.Journal.JournalIssue.PubDate.Year", count:{$sum:1}}})
                      .result;
}

function count_by_year(year)
{
    return db.articles.find({'PubmedArticle.MedlineCitation.Article.Journal.JournalIssue.PubDate.Year':year.toString()})
                      .count();
}

function count_abstracts(year, regex)
{
    return db.articles.find({'PubmedArticle.MedlineCitation.Article.Journal.JournalIssue.PubDate.Year':year.toString(), 
                             'PubmedArticle.MedlineCitation.Article.Abstract.AbstractText.value':regex})
	              .count();

}

function get_yearly_frequencies(year, regexes)
{
    var total = count_by_year(year);
    var results = {};
    for (var i in regexes)
    {
	var regex = regexes[i];
	results[regex] = count_abstracts(year, regex) / total;
    }
    return results;
}

function get_frequencies_years(start_year, end_year, regexes)
{
    results = {};
    for (var year = start_year; year < end_year; year++) {
	results[year] = get_yearly_frequencies(year, regexes);
    }
    return results;
}
