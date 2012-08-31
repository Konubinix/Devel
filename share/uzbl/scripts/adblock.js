var blocklist = new Array();

blocklist["IMG"] = [["src", /.*doubleclick.net.*/],
                    ["src", /.*last.fm\/adserver.*/]];

blocklist["SCRIPT"] = [["src", /.*doubleclick.net.*/],
		       //["src", /.*ads.*/]
		      ];

blocklist["IFRAME"] = [["name", /.*google_ads.*/],
                       ["id", /.*ad-google.*/]];

blocklist["DIV"] = [["id", /.*ads.*/],
                       ];

function adblock(event)
{
    var tag = event.target.tagName;
    if (!blocklist[tag])
        return;
    for(var i = 0; i < blocklist[tag].length; i++) {
        if (event.target.getAttribute(blocklist[tag][i][0])) {
            if (event.target.getAttribute(blocklist[tag][i][0]).match(blocklist[tag][i][1])) {
                event.preventDefault();
                return;
            }
        }
    }
}

document.addEventListener("beforeload", adblock, true);
