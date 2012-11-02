var results = get_frequencies_years(2009,2012,[/brain|cortex/i, /cancer|malignant/i, /heart|cardio/i]);

function getValues(obj)
{
    var values = [];
    for (var key in obj) {
	values.push(obj[key]);
    }
    return values;
}
function getKeys(obj)
{
    var keys = [];
    for (var key in obj) {
	keys.push(key);
    }
    return keys;
}

var isFirst = true;

for (var year in results)
{
    if (isFirst) {
	print('year,',getKeys(results[year]).join());
	isFirst = false;
    }
    print(year + ',', getValues(results[year]).join()); 
}
