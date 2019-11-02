-- Hammerspoon configuration reload

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "r",
   function()
      hs.reload()
   end
)
hs.alert("Reloading Hammerspoon configuration")


-- alt-tab window switcher

switcher_space = hs.window.switcher.new(
   -- current Space only
   hs.window.filter.new():setCurrentSpace(true)
)

hs.hotkey.bind(
   {"alt"}, "tab",
   function()
      switcher_space:next()
   end
)

hs.hotkey.bind(
   {"alt", "shift"}, "tab",
   function()
      switcher_space:previous()
   end
)


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


-- iTunes

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "p",
   function()
      if (hs.itunes.isRunning() and hs.itunes.isPlaying()) then
         hs.alert("Will pause iTunes at the end of the current track.")
         local trackNameStart = hs.itunes.getCurrentTrack()
         local status = "default"
         hs.timer.waitWhile(
            function()
               if ((not hs.itunes.isRunning()) or (not hs.itunes.isPlaying())) then
                  status = "itunes-changed"
                  return false
               elseif (trackNameStart ~= hs.itunes.getCurrentTrack()) then
                  status = "track-changed"
                  return false
               else
                  return true
               end
            end,
            function()
               if (hs.itunes.isRunning() and hs.itunes.isPlaying() and status == "track-changed") then
                  hs.itunes.pause()
               end
            end,
            0.5
         )
      end
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

function frameFillScreen(windowFrame, screenFrame)
   windowFrame.x = screenFrame.x
   windowFrame.y = screenFrame.y
   windowFrame.w = screenFrame.w
   windowFrame.h = screenFrame.h
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
         -- position 7/8 screen width frame on the left
         -- bounce to the right if already on left
         if (f.x == max.x and
             f.y == max.y and
             f.w == (max.w / 8) * 7 and
             f.h == max.h) then
            f.x = f.x + (max.w / 8)
         else
            f.x = max.x
            f.y = max.y
            f.w = (max.w / 8) * 7
            f.h = max.h
         end
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

-- stretch fully but not full-screen on current display
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "f",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      frameFillScreen(f, max)
      win:setFrame(f)
   end
)

-- full-screen on other display
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "f",
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
      local w = hs.window.get("Untitled")
      w:focus()
      w:centerOnScreen()
   end
)


-- Ctrl-Alt-Cmd-d: Toggle Dark Mode

hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "d",
   function()
      hs.osascript.applescript(
         [[set isdark to false
           tell application "System Events"
             tell appearance preferences
               set dark mode to not dark mode
               set isdark to (get dark mode)
             end tell
             if isdark then
               tell every desktop
                 set picture to "/Library/Desktop Pictures/Solid Colors/Black.png"
               end tell
             else
               tell every desktop
                 set picture to "/Library/Desktop Pictures/Aqua Graphite.jpg"
               end tell
             end if
           end tell]]
      )
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
