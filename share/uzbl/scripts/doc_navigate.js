doc_next = function() {
    var next_elem = $("a:contains(Thread Next),a:contains(Next),a:contains(next),a:contains(Suivant),a:text(>),a[title='Next Page'],a[rel='next'],li:contains('Next message: ') a");
    if(next_elem)
    {
		next_elem[0].click();
    }
}

doc_previous = function() {
    var prev_elem = $("a:contains(Thread Previous), a:contains(Prev), a:contains(prev), a:contains(previous), a:contains(Précédent),a[title='Previous Page'],a[rel='previous'],li:contains('Previous message: ') a");
    if(prev_elem)
    {
		prev_elem[0].click();
    }
}

doc_toc = function() {
    $("a:contains(ToC)")[0].click();
}

doc_current = function() {
    $("a:contains(Current)")[0].click();
}

doc_home = function() {
    $("a:contains(Home)")[0].click();
}

doc_up = function() {
    var up_elem = $("a[rel='up'], a:contains(Home)");
    if(up_elem)
	{
		up_elem[0].click();
	}
	else
	{
		if(document.URL.match(/\/$/))
		{
			window.location.href = document.URL + "..";
		}
		else
		{
			window.location.href = document.URL + "/..";
		}
	}
}
