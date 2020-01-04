--- Ctrl-Alt-Cmd-s: Open a new Safari window on current screen
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
