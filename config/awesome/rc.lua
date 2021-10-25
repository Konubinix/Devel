-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

local cyclefocus = require("awesome-cyclefocus")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify(
      {
         preset = naughty.config.presets.critical,
         title = "Oops, there were errors during startup!",
         text = awesome.startup_errors
      }
   )
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal(
      "debug::error",
      function (err)
         -- Make sure we don't go into an endless error loop
         if in_error then return end
         in_error = true
         naughty.notify({ preset = naughty.config.presets.critical,
                          title = "Oops, an error happened!",
                          text = tostring(err) })
         in_error = false
      end
   )
end

-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
terminal = "terminator"
terminal_class = "X-terminal-emulator"
terminal_class = "Terminator"
browser = os.getenv("BROWSER") or "x-www-browser"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.tile,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.corner.nw,
   awful.layout.suit.corner.ne,
   awful.layout.suit.corner.sw,
   awful.layout.suit.corner.se,
   awful.layout.suit.floating,
}

-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

local menu_awesome = {
   "awesome",
   myawesomemenu,
   beautiful.awesome_icon
}
local menu_terminal = {
   "open terminal",
   terminal
}

if has_fdo then
   mymainmenu = freedesktop.menu.build(
      {
         before = {
            menu_awesome
         },
         after = {
            menu_terminal
         }
      }
   )
else
   mymainmenu = awful.menu(
      {
         items = {
            menu_awesome,
            {
               "Debian",
               debian.menu.Debian_menu.Debian
            },
            menu_terminal,
         }
      }
   )
end


mylauncher = awful.widget.launcher(
   {
      image = beautiful.awesome_icon,
      menu = mymainmenu
   }
)

menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Create a textclock widget
mytextclock = wibox.widget.textclock()

myorgmodeline = awful.widget.watch('bash -c "echo \\"Org: $(redis-cli get org-modeline|sed \'s/^$/NA/\')\\""', 1)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            c:emit_signal(
               "request::activate",
               "tasklist",
               {raise = true}
            )
         end
   end),
   awful.button({ }, 3, function()
         awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

local function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist {
         screen  = s,
         filter  = awful.widget.taglist.filter.all,
         buttons = taglist_buttons
      }

      -- Create a tasklist widget
      s.mytasklist = awful.widget.tasklist {
         screen  = s,
         filter  = awful.widget.tasklist.filter.currenttags,
         buttons = tasklist_buttons
      }

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
	    myorgmodeline,
            s.mytaglist,
            s.mypromptbox,
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            -- wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
         },
      }
end)
-- }}}

-- put the systray of the last screen, generally the external one that I use
-- most
local trayscreen = screen[screen.count()]
local traybar = awful.wibar({
      position = "bottom",
      screen = trayscreen,
      width = trayscreen.geometry.width
})

local tray = wibox.widget.systray()
tray:set_screen(trayscreen)
traybar:setup {
   layout = wibox.layout.fixed.horizontal,
   tray
}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

function open_or_join (class, program)
   return function(c)
      function move_to_client(c_)
         awful.screen.focus(c_.screen)
         client.focus = c_
         c_:raise()
         c_.first_tag:view_only()
         c_:swap(awful.client.getmaster())
      end

      function find_candidate(class)
         local tag = awful.screen.focused().selected_tag
         local candidates = {}
         local local_candidates = {}
         for _, c_ in ipairs(client.get()) do
            if c_.class == class then
               table.insert(candidates, c_)
               if c_.first_tag == tag
               then
                  table.insert(local_candidates, c_)
               end
            end
         end

         function compare_candidates(c1, c2)
            return c1.first_tag.name < c2.first_tag.name
         end

         table.sort(candidates, compare_candidates)
         table.sort(local_candidates, compare_candidates)

         local result = nil
         if #local_candidates > 0
         then
            result =local_candidates[1]
         elseif #candidates > 0
         then
            result = candidates[1]
         else
            result = nil
         end
         if result
         then
            say("Found " .. class .. " (in " .. result.first_tag.name .. ")")
         end
         return result
      end

      local result = nil
      if client.focus and client.focus.class == class
      then
         say("Nothing to do, already on a window with class " .. class)
         -- moving anyway to focus on the correct screen and have a coherent
         -- behavior about the tags
         result = client.focus
      else
         result = find_candidate(class)
      end

      if result
      then
         move_to_client(result)
      else
         say("Did not find " .. class .. "..., Opening " .. program .. " instead")
         awful.spawn(program)
      end
   end
end

function tell_pulse_volume()
   awful.spawn.easy_async (
      "clk pulse show",
      function (stdout, stderr, exitreason, exitcode)
         naughty.notify(
            {
               preset = naughty.config.presets.normal,
               title = "Volume",
               text = stdout
            }
         )
      end
   )
end


function say(message)
   naughty.notify({text = message})
end

-- {{{ Key bindings
globalkeys = gears.table.join(
   awful.key({ modkey, "Control"  }, "Left",   function () awful.client.focus.bydirection("left") end,
      {description = "focus by direction", group = "client"}),
   awful.key({ modkey, "Control"  }, "Right",   function () awful.client.focus.bydirection("right") end,
      {description = "focus by direction", group = "client"}),
   awful.key({ modkey, "Control"  }, "Down",   function () awful.client.focus.bydirection("down") end,
      {description = "focus by direction", group = "client"}),
   awful.key({ modkey, "Control"  }, "Up",   function () awful.client.focus.bydirection("up") end,
      {description = "focus by direction", group = "client"}),


   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
      {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   awful.key({ modkey,          }, "Next",
      function ()
         awful.client.cycle()
         local master = awful.client.getmaster()
         client.focus = master
         master:raise()
      end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key({ modkey,           }, "Prior",
      function ()
         awful.client.cycle(true)
         local master = awful.client.getmaster()
         client.focus = master
         master:raise()
      end,
      {description = "focus previous by index", group = "client"}
   ),
   cyclefocus.key({ modkey, }, "Tab", {
         -- cycle_filters as a function callback:
         -- cycle_filters = { function (c, source_c) return c.screen == source_c.screen end },

         -- cycle_filters from the default filters:
         cycle_filters = { cyclefocus.filters.same_screen, cyclefocus.filters.common_tag },
         keys = {'Tab', 'ISO_Left_Tab'}  -- default, could be left out
   }),
   cyclefocus.key({ modkey, "Shift" }, "Tab", {
         -- cycle_filters as a function callback:
         -- cycle_filters = { function (c, source_c) return c.screen == source_c.screen end },

         -- cycle_filters from the default filters:
         cycle_filters = { cyclefocus.filters.same_screen, cyclefocus.filters.common_tag },
         keys = {'Tab', 'ISO_Left_Tab'}  -- default, could be left out
   }),
   awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   awful.key({ modkey, "Shift"          }, "Left",
      function ()
         local t = client.focus and client.focus.first_tag or nil
         if t == nil then
            return
         end
         local tag = client.focus.screen.tags[(t.name - 2) % 9 + 1]
         awful.client.movetotag(tag)
         tag:view_only()
      end,
      {description = "focus previous by index", group = "client"}
   ),
   awful.key({ modkey, "Shift"          }, "Right",
      function ()
         local t = client.focus and client.focus.first_tag or nil
         if t == nil then
            return
         end
         local tag = client.focus.screen.tags[t.name % 9 + 1]
         awful.client.movetotag(tag)
         tag:view_only()
      end,
      {description = "focus previous by index", group = "client"}
   ),


   -- Layout manipulation
   awful.key({ modkey, "Mod1"   }, "Down", function ()
         awful.spawn("clk pulse down")
         tell_pulse_volume()
   end,
      {description = "change volume", group = "client"}),
   awful.key({ modkey, "Mod1"   }, "Up", function ()
         awful.spawn("clk pulse up")
         tell_pulse_volume()
   end,
      {description = "change volume", group = "client"}),
   awful.key({ modkey, "Mod1" }, "Right", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Mod1" }, "Left", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),

   awful.key({ modkey,           }, ".", naughty.destroy_all_notifications,
      {description = "destroy all notif", group = "notif"}),

   awful.key({ modkey        }, "i",
      function()
         awful.spawn("konix_print_screen.sh")
      end,
      {description = "Print screen", group = "screen"}),

   awful.key({ modkey        }, "e",
      function()
         awful.spawn("emacsclient --eval '(emacs-everywhere)'")
      end,
      {description = "Modify the current content in emacs", group = "emacs"}),

   awful.key({ modkey , "Shift"       }, "i",
      function()
         awful.spawn("konix_print_screen_n_insert.sh")
      end,
      {description = "Print screen", group = "screen"}),

   -- Standard program
   awful.key({ modkey,   "Control"        }, "q", open_or_join("qutebrowser", browser),
      {description = "open a browser", group = "launcher"}),
   awful.key({ modkey,   "Control"        }, "e", open_or_join("Emacs", "ec"),
      {description = "open emacs", group = "launcher"}),
   awful.key({ modkey,   "Control"        }, "f", open_or_join("Firefox", "firefox"),
      {description = "open firefox", group = "launcher"}),
   awful.key({ modkey,   "Control"        }, "c", open_or_join("Chromium", "chromium"),
      {description = "open firefox", group = "launcher"}),
   awful.key({ modkey,   "Control"        }, "v", open_or_join("vlc", "vlc"),
      {description = "open firefox", group = "launcher"}),
   awful.key({ modkey,   "Control"        }, "i", open_or_join("Firefox", "firefox"),
      {description = "open firefox", group = "launcher"}),
   awful.key({ modkey, "Control"   }, "m", open_or_join("Gmpc", "gmpc"),
      {description = "Open the music controler", group = "musique"}),
   awful.key({ modkey,   "Control"        }, "s", open_or_join("Slack", "slack --no-sandbox"),
      {description = "open emacs", group = "launcher"}),
   awful.key({ modkey,           },
      "Return",
      open_or_join(
	 terminal_class,
	 terminal
      ),
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control"          }, "t", open_or_join(
	 terminal_class,
	 terminal
							      ),
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   awful.key({ modkey,           }, "p",     function ()
         awful.spawn.easy_async(
            "clk mpc toggle",
            function (stdout, stderr, exitreason, exitcode)
               naughty.notify(
                  {
                     preset = naughty.config.presets.normal,
                     title = "MPD",
                     text = stdout
                  }
               )
            end
         )
   end,
      {description = "toggle play", group = "music"}),
   awful.key({ modkey, "Shift"          }, "p",     function ()
         awful.spawn.easy_async(
            "clk pulse toggle-mute",
            function (stdout, stderr, exitreason, exitcode)
	       tell_pulse_volume()
            end
         )
   end,
      {description = "toggle mute", group = "music"}),
   awful.key({ modkey, "Control"          }, "p", open_or_join("Pavucontrol", "pavucontrol"),
      {description = "pavucontrol", group = "music"}),
   awful.key({ modkey,  "Shift"         }, "Up",     function () awful.tag.incmwfact( 0.05)          end,
      {description = "increase master width factor", group = "layout"}),
   awful.key({ modkey,   "Shift"        }, "Down",     function () awful.tag.incmwfact(-0.05)          end,
      {description = "decrease master width factor", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
      {description = "increase the number of master clients", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
      {description = "decrease the number of master clients", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
      {description = "increase the number of columns", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
      {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            c:emit_signal(
               "request::activate", "key.unminimize", {raise = true}
            )
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,
      {description = "lua execute prompt", group = "awesome"}),
   -- Menubar
   awful.key({ modkey,  }, "l", function () awful.spawn("xscreensaver-command -lock") end,
      {description = "lock screen", group = "screen"})

)

clientkeys = gears.table.join(
   awful.key(
      {
         modkey,
      },
      "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {
         description = "toggle fullscreen",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey, "Shift"
      },
      "c",
      function (c)
         c:kill()
      end,
      {
         description = "close",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "F4",
      function (c)
         c:kill()
      end,
      {
         description = "close",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey, "Control"
      },
      "space",
      awful.client.floating.toggle,
      {
         description = "toggle floating",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey, "Control"
      },
      "Return",
      function (c)
         c:swap(awful.client.getmaster())
      end,
      {
         description = "move to master",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "o",
      function (c)
         c:move_to_screen()
      end,
      {
         description = "move to screen",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,   "Control"
      },
      "*",
      function (c)
         c.ontop = not c.ontop
      end,
      {
         description = "toggle keep on top",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "n",
      function (c)
	 awful.spawn("konix_org_capture_screenshot.sh")
      end ,
      {
         description = "capture a screenshot",
         group = "org-mode"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "F9",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {
         description = "minimize",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "d",
      function(c)
	 awful.spawn("clk x scroll-down")
      end,
      {
         description = "scroll down",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "j",
      function (c)
         awful.spawn("ec --eval '(konix/org-capture-interruption)'")
      end ,
      {
         description = "interruption",
         group = "org-mode"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "y",
      function (c)
         awful.spawn("impass_gui.sh")
      end ,
      {
         description = "Run the impass",
         group = "impass"
      }
   ),
   awful.key(
      {
         modkey,
	 "Shift",
      },
      "j",
      function (c)
         awful.spawn("ec --eval '(konix/org-capture-external-interruption)'")
      end ,
      {
         description = "interruption",
         group = "org-mode"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {
         description = "(un)maximize",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,
      },
      "F10",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {
         description = "(un)maximize",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey, "Control"
      },
      "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
      end ,
      {
         description = "(un)maximize vertically",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey, "Shift"
      },
      "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c:raise()
      end ,
      {
         description = "(un)maximize horizontally",
         group = "client"
      }
   ),
   awful.key(
      {
         modkey,  "Control"
      },
      "e",
      {
         description = "(un)maximize horizontally",
         group = "client"
      }
   )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(
      globalkeys,
      -- View tag only.
      awful.key(
         {
            modkey
         },
         "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               tag:view_only()
            end
         end,
         {
            description = "view tag #"..i,
            group = "tag"
         }
      ),
      -- Toggle tag display.
      awful.key(
         {
            modkey, "Control"
         },
         "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               awful.tag.viewtoggle(tag)
            end
         end,
         {
            description = "toggle tag #" .. i,
            group = "tag"
         }
      ),
      -- Move client to tag.
      awful.key(
         {
            modkey, "Shift"
         },
         "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:move_to_tag(tag)
               end
            end
         end,
         {
            description = "move focused client to tag #"..i,
            group = "tag"
         }
      ),
      -- Toggle tag on focused client.
      awful.key(
         {
            modkey, "Control", "Shift"
         },
         "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:toggle_tag(tag)
               end
            end
         end,
         {
            description = "toggle focused client on tag #" .. i,
            group = "tag"
         }
      )
   )
end

clientbuttons = gears.table.join(
   awful.button(
      { }, 1,
      function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
      end
   ),
   awful.button(
      {"Mod1", "Mod4", "Control", "Shift"},
      1,
      function (c)
         if c.pid then
            awful.spawn("kill -9 " .. c.pid)
         end
      end
   ),
   awful.button(
      {
         modkey
      },
      1,
      function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.move(c)
      end
   ),
   awful.button(
      {
         modkey
      },
      3,
      function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.resize(c)
      end
   )
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   {
      rule = { },
      properties = {
         border_width = beautiful.border_width,
         border_color = beautiful.border_normal,
         focus = awful.client.focus.filter,
         raise = true,
         keys = clientkeys,
         buttons = clientbuttons,
         screen = awful.screen.preferred,
         placement = awful.placement.no_overlap+awful.placement.no_offscreen,
         floating = false,
         maximized = false,
      }
   },

   -- Floating clients.
   {
      rule_any = {
         instance = {
            "DTA",  -- Firefox addon DownThemAll.
            "copyq",  -- Includes session name in class.
            "pinentry",
         },
         class = {
            "Konix_gtk_entry.py",
            "Arandr",
            "Blueman-manager",
            "Gpick",
            "vlc",
            "Kruler",
            "MessageWin",  -- kalarm.
            "Sxiv",
            "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
            "Wpa_gui",
            "veromix",
            "xtightvncviewer"},

         -- Note that the name property shown in xprop might be set slightly after creation of the client
         -- and the name shown there might not match defined rules here.
         name = {
            "Event Tester",  -- xev.
	    "Terminator Preferences",
	    "Choose A Terminal Font",
         },
         role = {
            "AlarmWindow",  -- Thunderbird's calendar.
            "ConfigManager",  -- Thunderbird's about:config.
            "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
         },
      },
      properties = {
         floating = true,
      }
   },
   {
      rule = {
         class = "qutebrowser",
	 name = "Print",
      },
      properties = {
         floating = true,
         ontop = true,
      }
   },

   {
      rule_any = {
         name = {"Slack | mini panel", "Pick a Font", "Change Foreground Color", "Post Processing Plugin"},
	 class = {"Impass"}
      },
      properties = {
         floating = true,
         ontop = true,
      }
   },

   -- Add titlebars to normal clients and dialogs
   {
      rule_any = {
         type = { "normal", "dialog" },
      },
      properties = {
         titlebars_enabled = true,
      }
   },
   {
      rule = {
         class = "Slack"
      },
      properties = {
         tag = "9"
      },
   },
   {
      rule_any = {
         class = {terminal, "qutebrowser", "Chromium"}
      },
      properties = {
         tag = "1"
      },
   },
   -- move the emacs spawn so that it will come back when ready
   {
      rule = {
         class = "Emacs"
      },
      properties = {
         tag = "8"
      },
   },
   {
      rule = {
         class = "Emacs",
         name = "Ediff",
      },
      properties = {
         tag = "1",
	 floating = true,
	 ontop = true,
      },
   },
   {
      rule = {
         class = "Pavucontrol",
      },
      properties = {
         tag = "2",
      },
   },
   {
      rule = {
         class = "Emacs",
         name = "konix_emacs.+",
      },
      properties = {
         tag = "1",
      },
   },
   {
      rule = {
         class = "Emacs",
         name = "emacs-everywhere",
      },
      properties = {
         tag = "1",
      },
   },

   -- make sure that the rules is applyed on emacs even though the name is not
   -- set immediately
   {
      rule = {
         class = "Emacs"
      },
      callback = function(c)

         local maybe_move_to_tag

         maybe_move_to_tag = function (c)
            if c.name == "konix_emacs_batch"
            then
               move_to_tag(c, c.screen.tags[8])
            elseif string.find(c.name, "konix_emacs")
            then
               move_to_tag(c, c.screen.tags[1])
            end
         end

         function move_to_tag(c, tag)
            c:move_to_tag(tag)
            client.focus = c
            c:raise()
            c:disconnect_signal("property::name", maybe_move_to_tag)
         end

         c:connect_signal("property::name", maybe_move_to_tag)
      end
   },
}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                         -- Set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end

                         if awesome.startup
                            and not c.size_hints.user_position
                            and not c.size_hints.program_position then
                            -- Prevent clients from being unreachable after screen count changes.
                            awful.placement.no_offscreen(c)
                         end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
                         -- buttons for the titlebar
                         local buttons = gears.table.join(
                            awful.button({ }, 1, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.move(c)
                            end),
                            awful.button({ }, 3, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.resize(c)
                            end)
                         )

                         awful.titlebar(c) : setup {
                            { -- Left
                               awful.titlebar.widget.iconwidget(c),
                               buttons = buttons,
                               layout  = wibox.layout.fixed.horizontal
                            },
                            { -- Middle
                               { -- Title
                                  align  = "center",
                                  widget = awful.titlebar.widget.titlewidget(c)
                               },
                               buttons = buttons,
                               layout  = wibox.layout.flex.horizontal
                            },
                            { -- Right
                               awful.titlebar.widget.floatingbutton (c),
                               awful.titlebar.widget.maximizedbutton(c),
                               awful.titlebar.widget.stickybutton   (c),
                               awful.titlebar.widget.ontopbutton    (c),
                               awful.titlebar.widget.closebutton    (c),
                               layout = wibox.layout.fixed.horizontal()
                            },
                            layout = wibox.layout.align.horizontal
                                                   }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
                         c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
