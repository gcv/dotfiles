--- Ctrl-Alt-Cmd-d: Toggle Dark Mode
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
