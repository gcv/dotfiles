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
-- hs.loadSpoon("Shade")
-- hs.loadSpoon("GitSync")
