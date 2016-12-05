-- Hammerspoon configuration reload

hs.hotkey.bind(
   {"cmd", "alt", "ctrl"}, "r",
   function()
      hs.reload()
end)
hs.alert("Reloading Hammerspoon configuration")


-- helpers

function app_switch_emacs()
   local emacs = hs.application.get("Emacs")
   if emacs then
      emacs:activate()
   end
end

function app_switch_terminal()
   local term = hs.application.get("iTerm2") or hs.application.get("Terminal")
   if term then
      term:activate()
   end
end


-- window sizing

hs.hotkey.bind(
   {"cmd", "alt", "ctrl"}, "left",
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
end)

hs.hotkey.bind(
   {"cmd", "alt", "ctrl"}, "right",
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
end)

hs.hotkey.bind(
   {"cmd", "alt", "ctrl"}, "up",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      f.x = max.x + (max.w / 8)
      f.y = max.y
      f.w = (max.w / 8) * 6
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind(
   {"cmd", "alt", "ctrl"}, "down",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      local c = 100
      f.x = max.x + (max.w / 8) + c
      f.y = max.y + c
      f.w = (max.w / 8) * 6 - (2 * c)
      f.h = max.h - (2 * c)
      win:setFrame(f)
end)


-- f11: iTerm2 or Terminal

hs.hotkey.bind({}, "f11", app_switch_terminal)


-- f12: Emacs

hs.hotkey.bind({}, "f12", app_switch_emacs)


-- iTerm2: C-c C-z flips to Emacs.

ctrl_c = hs.hotkey.modal.new("ctrl", "c")

function ctrl_c:entered()
   local app = hs.application.frontmostApplication()
   local app_name = app:name()
   if "iTerm2" ~= app_name and "Terminal" ~= "app_name" then
      -- forward C-c to underlying app
      hs.eventtap.keyStroke({"ctrl"}, "c")
      ctrl_c:exit()
   else
      -- timeout
      hs.timer.doAfter(
         1,
         function()
            ctrl_c:exit()
      end)
   end
end

ctrl_c:bind(
   {"ctrl"}, "z",
   function()
      app_switch_emacs()
      ctrl_c:exit()
end)

function ctrl_c:exited()
   -- nothing
end
