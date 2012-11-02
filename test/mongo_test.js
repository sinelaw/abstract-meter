db.articles.aggregate(
    { $match: {
	"PubmedArticle.MedlineCitation.Article.Abstract.AbstractText.value":/brain/i
    }}, 
    { $project: {"PubmedArticle.MedlineCitation.Article.ArticleTitle": 1}}
).result.length
