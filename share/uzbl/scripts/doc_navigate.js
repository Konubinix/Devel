doc_next = function() {
    $("a:contains(Thread Next), a:contains(Next), a:contains(next), a:contains(Suivant), a:text(>)")[0].click();
}

doc_previous = function() {
    $("a:contains(Thread Previous), a:contains(Prev), a:contains(previous), a:contains(Précédent)")[0].click();
}

doc_toc = function() {
    $("a:contains(ToC)")[0].click();
}

doc_home = function() {
    $("a:contains(Home)")[0].click();
}

doc_up = function() {
    if(document.URL.match(/\/$/))
    {
	window.location.href = document.URL + "..";
    }
    else
    {
	window.location.href = document.URL + "/..";
    }
}
