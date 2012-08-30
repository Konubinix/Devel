add_to_pocket = function()
{
    ISRIL_H='7a1a';
    PKT_D='getpocket.com';
    ISRIL_SCRIPT=document.createElement('SCRIPT');
    ISRIL_SCRIPT.type='text/javascript';
    ISRIL_SCRIPT.src='http://'+PKT_D+'/b/r.js';
    document.getElementsByTagName('head')[0].appendChild(ISRIL_SCRIPT);
}
