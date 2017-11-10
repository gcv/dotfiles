-- Hammerspoon configuration reload

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")


-- trying to isolate Spaces keyboard switching bug

-- hs.hotkey.bind(
--    {"ctrl"}, "5",
--    function()
--       -- hs.alert("ctrl 1 hit")
--       -- hs.osascript.applescript([[tell application "System Events" to key code 18 using control down]])
--    end
-- )

-- hs.hotkey.bind(
--    {"ctrl"}, "2",
--    function()
--       hs.alert("ctrl 2 hit")
--    end
-- )

-- hs.hotkey.bind(
--    {"ctrl"}, "3",
--    function()
--       hs.alert("ctrl 3 hit")
--    end
-- )


-- brightness control

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "b",
   function()
      hs.brightness.set(1)
      -- local screens = hs.screen.allScreens()
      -- hs.fnutils.each(
      --    screens,
      --    function(screen)
      --       screen:setBrightness(0.01)
      --    end
      -- )
      -- hs.alert("brightness updated")
   end
)


-- helpers

function appSwitchEmacs()
   local emacs = hs.application.get("Emacs")
   if emacs then
      emacs:activate()
   end
end

function appSwitchTerminal()
   local term = hs.application.get("iTerm2") or hs.application.get("Terminal")
   if term then
      term:activate()
   end
end


-- window sizing

previousFrames = {}

function alterOrRestoreFrame(win, alterCb, restoreCb)
   local restoreCb = restoreCb or
      function(win, previousFrame)
         win:move(previousFrame)
      end
   local previousFrame = previousFrames[win:id()]
   if previousFrame then
      restoreCb(win, previousFrame)
      previousFrames[win:id()] = nil
   else
      previousFrames[win:id()] = win:frame()
      alterCb(win)
   end
end

function frameFillMostOfScreenUp(windowFrame, screenFrame)
   windowFrame.x = screenFrame.x + (screenFrame.w / 8)
   windowFrame.y = screenFrame.y
   windowFrame.w = (screenFrame.w / 8) * 6
   windowFrame.h = screenFrame.h
end

function frameFillMostOfScreenCenter1(windowFrame, screenFrame)
   local c = 100
   windowFrame.x = screenFrame.x + (screenFrame.w / 8) + c
   windowFrame.y = screenFrame.y + c
   windowFrame.w = (screenFrame.w / 8) * 6 - (2 * c)
   windowFrame.h = screenFrame.h - (2 * c)
end

function frameFillMostOfScreenCenter2(windowFrame, screenFrame)
   local c = 25
   windowFrame.x = screenFrame.x + (screenFrame.w / 8)
   windowFrame.y = screenFrame.y + (screenFrame.h / 8) + c
   windowFrame.w = (screenFrame.w / 8) * 6
   windowFrame.h = (screenFrame.h / 8) * 6 - (2 * c)
end

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "left",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
   end
)

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "right",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x + (max.w / 2)
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
   end
)

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "up",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      frameFillMostOfScreenUp(f, max)
      win:setFrame(f)
   end
)

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "down",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      frameFillMostOfScreenCenter1(f, max)
      win:setFrame(f)
   end
)

hs.hotkey.bind(
   {"shift", "ctrl", "alt", "cmd"}, "down",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      frameFillMostOfScreenCenter2(f, max)
      win:setFrame(f)
   end
)

function makeScreenFillHotkeysForScreen(keyString, screenId)
   hs.hotkey.bind(
      {"alt", "cmd"}, keyString,
      function()
         local win = hs.window.focusedWindow()
         local f = win:frame()
         local screen = hs.screen.allScreens()[screenId]
         local max = screen:frame()
         frameFillMostOfScreenUp(f, max)
         win:setFrame(f)
      end
   )
   hs.hotkey.bind(
      {"ctrl", "alt", "cmd"}, keyString,
      function()
         local win = hs.window.focusedWindow()
         local f = win:frame()
         local screen = hs.screen.allScreens()[screenId]
         local max = screen:frame()
         frameFillMostOfScreenCenter1(f, max)
         win:setFrame(f)
      end
   )
   hs.hotkey.bind(
      {"shift", "alt", "cmd"}, keyString,
      function()
         local win = hs.window.focusedWindow()
         local f = win:frame()
         local screen = hs.screen.allScreens()[screenId]
         local max = screen:frame()
         f.x = max.x
         f.y = max.y
         f.w = (max.w / 8) * 7
         f.h = max.h
         win:setFrame(f)
      end
   )
   hs.hotkey.bind(
      {"shift", "ctrl", "alt", "cmd"}, keyString,
      function()
         local win = hs.window.focusedWindow()
         local f = win:frame()
         local screen = hs.screen.allScreens()[screenId]
         local max = screen:frame()
         frameFillMostOfScreenCenter2(f, max)
         win:setFrame(f)
      end
   )
end

makeScreenFillHotkeysForScreen("1", 1)
makeScreenFillHotkeysForScreen("2", 2)
makeScreenFillHotkeysForScreen("3", 3)
makeScreenFillHotkeysForScreen("4", 4)

-- full-screen on other display
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "f",
   function()
      alterOrRestoreFrame(
         hs.window.focusedWindow(),
         -- alter
         function(win)
            if win:isFullScreen() then
               -- this branch occurs if the window was full-screened but not by
               -- this other-display logic; so just un-full-screen the window
               win:setFullScreen(false)
            else
               local currentScreen = win:screen()
               local nextScreen = currentScreen:next()
               win:centerOnScreen(nextScreen, true)
               win:setFullScreen(true)
            end
         end,
         -- restore
         function(win, previousFrame)
            win:setFullScreen(false)
            hs.timer.waitWhile(
               function() return win:isFullScreen(); end,
               function()
                  if previousFrame then
                     win:move(previousFrame)
                  else
                     win:centerOnScreen(nextScreen, true)
                  end
               end,
               0.5)
         end
      )
   end
)


-- f11: iTerm2 or Terminal

hs.hotkey.bind({}, "f11", appSwitchTerminal)


-- f12: Emacs

hs.hotkey.bind({}, "f12", appSwitchEmacs)


-- Ctrl-Alt-Cmd-s: Safari

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "s",
   function()
      hs.osascript.applescript(
         [[tell application "Safari"
              make new document
              activate
           end tell]]
      )
      local w = hs.window.frontmostWindow()
      w:centerOnScreen()
   end
)


-- iTerm2: C-c C-z flips to Emacs.

ctrlC = hs.hotkey.modal.new("ctrl", "c")

function ctrlC:entered()
   local app = hs.application.frontmostApplication()
   local app_name = app:name()
   if "iTerm2" ~= app_name and "Terminal" ~= "app_name" then
      -- forward C-c to underlying app
      hs.eventtap.keyStroke({"ctrl"}, "c")
      ctrlC:exit()
   else
      -- timeout
      hs.timer.doAfter(
         1,
         function()
            ctrlC:exit()
         end
      )
   end
end

ctrlC:bind(
   {"ctrl"}, "z",
   function()
      appSwitchEmacs()
      ctrlC:exit()
   end
)

function ctrlC:exited()
   -- nothing
end
