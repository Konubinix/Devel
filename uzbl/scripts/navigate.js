var navigate = {
    links: null,
    old_css: null,
    selected: -1,

    init: function()
    {
	navigate.links = $("a");
	navigate.old_css = null;
	navigate.selected = -1;
    },

    next: function ()
    {
	if(navigate.selected > -1)
	{
	    $(navigate.links[navigate.selected]).css("background", navigate.old_css);
	}
	navigate.selected = (navigate.selected + 1) % (navigate.links.length);
	navigate.old_css = $(navigate.links[navigate.selected]).css("background");
	$(navigate.links[navigate.selected]).css("background", "yellow");
	var ypos = findPos(navigate.links[navigate.selected])[1];
	window.scroll(0, ypos - 200);
    },

    prev: function ()
    {
	if(navigate.selected > -1)
	{
	    $(navigate.links[navigate.selected]).css("background", navigate.old_css);
	}
	var l = navigate.links.length;
	navigate.selected = (((navigate.selected - 1) % l)+l)%l;
	navigate.old_css = $(navigate.links[navigate.selected]).css("background");
	$(navigate.links[navigate.selected]).css("background", "yellow");
	var ypos = findPos(navigate.links[navigate.selected])[1];
	window.scroll(0, ypos - 200);
    },

    click: function()
    {
	navigate.links[navigate.selected].click();
    }

}
navigate.init();
//alert("OK");
