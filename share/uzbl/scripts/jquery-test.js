var jquery_test = function()
{
	var res = "jquery_test\n";
	try
	{
		var jQuery = jQuery;
		res += "OUI\n";
	}
	catch(e)
	{
		res += "NON\n";
	}
	res += "jquery_test_end\n";
	return res;
}
