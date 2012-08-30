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

    $("[id]:not(a[name])").each(
	function()
	{
	    if($(this).text())
	    {
		var tag = $(this).find(".konix_name_"+$(this).attr("id"));
		if(tag.length == 0)
		{
	    	    $(this).prepend("<a class='konix_name_"+$(this).attr("id")+"' href='#"+$(this).attr("id")+"'>#</a>");
		}
		else
		{
	    	    tag.remove();
		}
	    }
	}
    );
}
