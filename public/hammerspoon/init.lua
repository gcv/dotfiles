--- Hammerspoon configuration reload
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")


--- KeePassXC launcher:
hs.hotkey.bind(
   {"cmd", "alt"}, "\\",
   function()
      hs.application.launchOrFocus("KeePassXC")
      local app = hs.application.find("KeePassXC")
   end
)


--- external scripts
dofile("./windows.lua")
dofile("./ctrl-esc.lua")


--- Spoons (not using these right now):
--hs.loadSpoon("something in the Spoons directory")
