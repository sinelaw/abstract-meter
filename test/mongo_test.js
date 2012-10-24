db.articles.aggregate(
    { $match: {
	$or: [{"PubmedArticle.MedlineCitation.Article.Abstract.AbstractText.value":/brain/i}, 
	      {"PubmedArticle.MedlineCitation.Article.Abstract.AbstractText":/brain/i} ]
    }}, 
    { $project: {"PubmedArticle.MedlineCitation.Article.ArticleTitle": 1}}
).result.length
