-- [[id:094cf0ac-1400-4ba2-87d1-db8c37df932f::screencam-awesome][screencam-awesome]]
-- Required by rc.lua: the clk x screencam frame floats above everything.
local awful = require("awful")

table.insert(awful.rules.rules, {
	rule = { class = "screencam" },
	properties = {
		floating = true,
		ontop = true,
		sticky = true,
		titlebars_enabled = true,
		border_width = 0,
		size_hints_honor = true,
		placement = awful.placement.no_offscreen,
	},
})
-- screencam-awesome ends here
