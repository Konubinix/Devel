function highlight_anchor_toggle ()
{
    $("a[name]").each(
	function()
	{
	    if($(this).text() == "")
	    {
		$(this).attr("href", "#"+$(this).attr("name")).text("#");
	    }
	    else
	    {
		$(this).text("");
	    }
	}
    );
}
