--- Hammerspoon configuration reload
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")

--- external scripts
dofile("./dark-mode.lua")
dofile("./windows.lua")
dofile("./safari.lua")
dofile("./itunes.lua")
dofile("./emacs-and-term.lua")

--- Spoons:
hs.loadSpoon("Action")
--hs.loadSpoon("GitSync")
--hs.loadSpoon("Shade")

-- Consider installing URLDispatcher to set up link-following:
-- http://www.hammerspoon.org/Spoons/URLDispatcher.html
-- Good ideas here: https://github.com/dcreemer/dotfiles/blob/master/.hammerspoon/init.fnl
