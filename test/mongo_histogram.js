map = function() {
    if (!this.PubmedArticle.MedlineCitation.Article.Abstract) {
        return;
    }
    var abstract = this.PubmedArticle.MedlineCitation.Article.Abstract.AbstractText.value;
    if (!abstract) {
        return;
    }
    var words = abstract.toLowerCase().replace(/[^a-z -]/g,"").split(" ");
    var histogram = {};
    for (i in words) {
        var word = words[i];
        if (word === '') {
            continue;
        }
        if (undefined == histogram[word]) {
            histogram[word] = 1;
        }
        else {
            histogram[word] = 1 + histogram[word];
        }
    }
    emit(1, { histogram: histogram });
}

reduce = function(key, values) {
    histogram = {};
    for (i in values) {
        var currentHistogram = values[i].histogram;
        for (word in currentHistogram) {
            currentWordCount = currentHistogram[word];
            if (undefined == histogram[word]) {
                histogram[word] = currentWordCount;
            }
            else {
                histogram[word] = currentWordCount + histogram[word];
            }
        }
    }
    return { histogram: histogram };
}

resultToArray = function(result)
{
    var histogram = result.histogram;
    var resultArray = [];
    for (word in histogram) {
        resultArray.push({count: histogram[word], word: word});
    }
    return resultArray;
}

sortHistogramResult = function(ar)
{
    ar.sort(function(a,b){return a.count-b.count});
    return ar;
}

process = function(limit)
{
    var mapReduceResult = db.articles.mapReduce(map, reduce, { limit: limit, out:'histogram'});
    return {meta: mapReduceResult, data: sortHistogramResult(resultToArray(db.histogram.findOne().value))};
}