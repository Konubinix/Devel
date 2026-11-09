-- [[id:c728373c-08d5-4125-a83c-06821b155923][a game frozen while it has no focus:2]]
-- Required by rc.lua: a listed game loses the focus, it freezes; it gets it back, it thaws.
local awful = require("awful")
local freeze_on_unfocus = { steam_app_7000 = true }
local freeze_script = "/home/sam/perso/konixwork/bin/konix_game-freeze.sh"
client.connect_signal("unfocus", function(c)
  if c.pid and freeze_on_unfocus[c.class or ""] then
    awful.spawn({ freeze_script, "STOP", tostring(c.pid) })
  end
end)
client.connect_signal("focus", function(c)
  if c.pid and freeze_on_unfocus[c.class or ""] then
    awful.spawn({ freeze_script, "CONT", tostring(c.pid) })
  end
end)
-- a game frozen while it has no focus:2 ends here
