function findPos(obj) {
    var curleft = curtop = 0;
    if (obj.offsetParent) {
		do {
			curleft += obj.offsetLeft;
			curtop += obj.offsetTop;
		} while (obj = obj.offsetParent);
		return [curleft,curtop];
    }
}

var my_scroll_tid = null;
var scroll_diff = 1200;
var scroll_sense = 0;
var scroll_delta = 100;
var scroll_max = 2000;
var scroll_size = 10;

function my_scroll()
{
    window.scroll(0, window.scrollY - scroll_size * scroll_sense);
}

function update_scroll()
{
    if(scroll_diff > scroll_max)
    {
		scroll_diff = scroll_max;
    }
    if(scroll_diff < 0)
    {
		scroll_diff = 0;
    }
    clearTimeout(my_scroll_tid);
    my_scroll_tid = null;
    if(scroll_diff * scroll_sense != 0)
    {
		my_scroll_tid = setInterval(my_scroll, scroll_max - Math.abs(scroll_diff));
    }
	return [scroll_diff, scroll_sense];
}

function scroll_down ()
{
    scroll_sense = scroll_sense - 1;
    if(scroll_sense < -1)
    {
		scroll_sense = -1;
    }
    return update_scroll();
}

function scroll_up ()
{
    scroll_sense = scroll_sense + 1;
    if(scroll_sense > 1)
    {
		scroll_sense = 1;
    }
    return update_scroll();
}

function scroll_faster ()
{
    scroll_diff = scroll_diff + scroll_delta;
    return update_scroll();
}

function scroll_slower ()
{
    scroll_diff = scroll_diff - scroll_delta;
    return update_scroll();
}
