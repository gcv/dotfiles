--- Ctrl-Alt-Cmd-<left>: Take up the left half of the screen
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

--- Ctrl-Alt-Cmd-<right>: Take up the right half of the screen
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

--- Ctrl-Alt-Cmd-<up>: Take up the central part of the screen, leaving sides open, but stretched vertically
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

--- Ctrl-Alt-Cmd-<down>: Take up the central part of the screen, leaving sides and top and bottom open
-- hs.hotkey.bind(
--    {"ctrl", "alt", "cmd"}, "down",
--    function()
--       local win = hs.window.focusedWindow()
--       local f = win:frame()
--       local screen = win:screen()
--       local max = screen:frame()
--       frameFillMostOfScreenCenter1(f, max)
--       win:setFrame(f)
--    end
-- )

--- Ctrl-Alt-Cmd-Shift-<down>: Take up the central part of the screen, leaving sides and top and bottom open
hs.hotkey.bind(
   {"ctrl", "alt", "cmd"}, "down",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      frameFillMostOfScreenCenter2(f, max)
      win:setFrame(f)
   end
)


--- Ctrl-Alt-Cmd-Shift-z: Zoom to the other screen's max size
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "z",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local currentScreen = win:screen()
      local nextScreen = currentScreen:next()
      local screen = nextScreen
      local max = screen:frame()
      f.w = max.w
      f.h = max.h
      win:setFrame(f)
   end
)


--- Ctrl-Alt-Cmd-Shift-<up>: Take up the upper left quarter of the screen
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "up",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h / 2
      win:setFrame(f)
   end
)

--- Ctrl-Alt-Cmd-Shift-<left>: Take up the lower left quarter of the screen
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "left",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x
      f.y = max.y + (max.h / 2)
      f.w = max.w / 2
      f.h = max.h / 2
      win:setFrame(f)
   end
)

--- Ctrl-Alt-Cmd-Shift-<down>: Take up the lower right quarter of the screen
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "down",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x + (max.w / 2)
      f.y = max.y + (max.h / 2)
      f.w = max.w / 2
      f.h = max.h / 2
      win:setFrame(f)
   end
)

--- Ctrl-Alt-Cmd-Shift-<right>: Take up the lower right quarter of the screen
hs.hotkey.bind(
   {"ctrl", "alt", "cmd", "shift"}, "right",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x + (max.w / 2)
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h / 2
      win:setFrame(f)
   end
)

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

--- stretch fully but not full-screen on current display
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

--- full-screen on other display
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


--- alt-tab window switcher
-- switcher_space = hs.window.switcher.new(
--    -- current Space only
--    hs.window.filter.new():setCurrentSpace(true)
-- )
--
-- hs.hotkey.bind(
--    {"alt"}, "tab",
--    function()
--       switcher_space:next()
--    end
-- )
--
-- hs.hotkey.bind(
--    {"alt", "shift"}, "tab",
--    function()
--       switcher_space:previous()
--    end
-- )


--- brightness control (does not really work)
-- hs.hotkey.bind(
--    {"ctrl", "alt", "cmd"}, "b",
--    function()
--       hs.brightness.set(1)
--       -- local screens = hs.screen.allScreens()
--       -- hs.fnutils.each(
--       --    screens,
--       --    function(screen)
--       --       screen:setBrightness(0.01)
--       --    end
--       -- )
--       -- hs.alert("brightness updated")
--    end
-- )
