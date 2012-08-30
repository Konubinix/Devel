doc_next = function() {
    $("a:contains(Next)")[0].click();
}

doc_previous = function() {
    $("a:contains(Prev)")[0].click();
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
