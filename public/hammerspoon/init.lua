--- Hammerspoon configuration reload
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")

-- forward delete key should be escape on 60% keyboards
-- but this code does not work in Emacs and iTerm2, not sure why
-- eventtapHandler = hs.eventtap.new(
--    {hs.eventtap.event.types.keyDown},
--    function (evt)
--       local kbdType = evt:getProperty(hs.eventtap.event.properties.keyboardEventKeyboardType)
--       local kbdEvtCode = evt:getProperty(hs.eventtap.event.properties.keyboardEventKeycode)
--       if 46 == kbdType then
--          if kbdEvtCode == hs.keycodes.map.forwarddelete then
--             evt:setKeyCode(hs.keycodes.map.escape)
--          elseif kbdEvtCode == hs.keycodes.map.escape then
--             evt:setKeyCode(hs.keycodes.map["`"])
--          end
--       end
--    end
-- )
-- eventtapHandler:start()

-- KeePassXC launcher:
hs.hotkey.bind(
   {"cmd", "alt"}, "\\",
   function()
      hs.application.launchOrFocus("KeePassXC")
      local app = hs.application.find("KeePassXC")
   end
)

--- external scripts
dofile("./dark-mode.lua")
dofile("./windows.lua")
dofile("./safari.lua")
dofile("./itunes.lua")
dofile("./emacs-and-term.lua")
--dofile("./ctrl-esc.lua")

--- Spoons:
--hs.loadSpoon("Action")
--hs.loadSpoon("GitSync")
--hs.loadSpoon("Shade")

-- Consider installing URLDispatcher to set up link-following:
-- http://www.hammerspoon.org/Spoons/URLDispatcher.html
-- Good ideas here: https://github.com/dcreemer/dotfiles/blob/master/.hammerspoon/init.fnl
