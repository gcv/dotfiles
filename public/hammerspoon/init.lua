--- Hammerspoon configuration reload
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")


--- KeePassXC launcher:
if not hs.application.infoForBundleID("com.1password.1password") then
  hs.hotkey.bind(
    {"cmd", "alt"}, "\\",
    function()
      hs.application.launchOrFocus("KeePassXC")
    end
  )
end


--- external scripts
dofile("./windows.lua")
dofile("./ctrl-esc.lua")


--- Spoons (not using these right now):
--hs.loadSpoon("something in the Spoons directory")
