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


--- f11: iTerm2 or Terminal
hs.hotkey.bind({}, "f11", appSwitchTerminal)


--- f12: Emacs
hs.hotkey.bind({}, "f12", appSwitchEmacs)


--- iTerm2 or Terminal: C-c C-z flips to Emacs and back
-- ctrlC = hs.hotkey.modal.new("ctrl", "c")
--
-- function ctrlC:entered()
--    local app = hs.application.frontmostApplication()
--    local app_name = app:name()
--    if "iTerm2" ~= app_name and "Terminal" ~= "app_name" then
--       -- forward C-c to underlying app
--       hs.eventtap.keyStroke({"ctrl"}, "c")
--       ctrlC:exit()
--    else
--       -- timeout
--       hs.timer.doAfter(
--          1,
--          function()
--             ctrlC:exit()
--          end
--       )
--    end
-- end
--
-- ctrlC:bind(
--    {"ctrl"}, "z",
--    function()
--       appSwitchEmacs()
--       ctrlC:exit()
--    end
-- )
--
-- function ctrlC:exited()
--    -- nothing
-- end
