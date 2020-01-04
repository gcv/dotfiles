--- Ctrl-Alt-Cmd-p: pause at the end of the current track
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
