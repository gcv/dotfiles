local wezterm = require "wezterm"
local act = wezterm.action
local conf = wezterm.config_builder()


local meta
if wezterm.target_triple:find("linux") or wezterm.target_triple:find("windows") then
   font_size = 10
   meta = "ALT"
   conf.window_decorations = "RESIZE"
elseif wezterm.target_triple:find("darwin") then
   font_size = 14
   meta = "CMD"
   conf.window_decorations = "RESIZE|INTEGRATED_BUTTONS"
end


-- conf.color_scheme = "Tokyo Night"
conf.audible_bell = "Disabled"
conf.adjust_window_size_when_changing_font_size = false
conf.window_background_opacity = 0.9
conf.macos_window_background_blur = 30
conf.font = wezterm.font("Hack Nerd Font")
conf.font_size = font_size
conf.window_frame = {
  font = wezterm.font("SF Pro", { weight = "Bold" }),
  font_size = font_size,
}
conf.colors = {
   cursor_bg = "#2F79A1"
}


conf.keys = {
   { key = "n", mods = meta .. "|SHIFT", action = act.SpawnWindow },
   { key = "c", mods = meta .. "|SHIFT", action = act.CopyTo "Clipboard" },
   { key = "v", mods = meta .. "|SHIFT", action = act.PasteFrom "Clipboard" },
   { key = "t", mods = meta .. "|SHIFT", action = act.SpawnTab "CurrentPaneDomain" },
   { key = "w", mods = meta .. "|SHIFT", action = act.CloseCurrentTab { confirm = true } },
   { key = "h", mods = meta, action = act.Hide },
   { key = "{", mods = meta .. "|SHIFT", action = act.ActivateTabRelative(-1) },
   { key = "}", mods = meta .. "|SHIFT", action = act.ActivateTabRelative(1) },
   { key = "+", mods = meta .. "|SHIFT", action = act.IncreaseFontSize },
   { key = "-", mods = meta, action = act.DecreaseFontSize },
   { key = "=", mods = meta, action = act.ResetFontSize },
   { key = "1", mods = meta, action = act.ActivateTab(0) },
   { key = "2", mods = meta, action = act.ActivateTab(1) },
   { key = "3", mods = meta, action = act.ActivateTab(2) },
   { key = "4", mods = meta, action = act.ActivateTab(3) },
   { key = "5", mods = meta, action = act.ActivateTab(4) },
   { key = "6", mods = meta, action = act.ActivateTab(5) },
   { key = "7", mods = meta, action = act.ActivateTab(6) },
   { key = "8", mods = meta, action = act.ActivateTab(7) },
   { key = "9", mods = meta, action = act.ActivateTab(8) },
   -- disable bindings that mess with everything
   { key = "r", mods = "CTRL", action = act.DisableDefaultAssignment },
   -- disable bindings that mess with Emacs
   { key = "-", mods = "CTRL", action = act.DisableDefaultAssignment },
   { key = "+", mods = "CTRL", action = act.DisableDefaultAssignment },
   { key = "_", mods = "CTRL|SHIFT", action = act.DisableDefaultAssignment },
   { key = "+", mods = "CTRL|SHIFT", action = act.DisableDefaultAssignment },
}


if wezterm.target_triple:find("darwin") then
   local meta_keys = {
      -- exclude h
      "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
      "a", "s", "d", "f", "g", "j", "k", "l",
      "z", "x", "c", "v", "b", "n", "m",
      "[", "]", "\\", ":", ";", "'", "\"", ",", "<", ".", ">", "?", "/",
      "UpArrow", "DownArrow", "LeftArrow", "UpArrow"
   }
   local meta_shift_keys = {
      -- exclude n, c, v, t, w
      "q", "e", "r", "y", "u", "i", "o", "p",
      "a", "s", "d", "f", "g", "h", "j", "k", "l",
      "z", "x", "b", "m",
      "[", "]", "\\", ":", ";", "'", "\"", ",", "<", ".", ">", "?", "/",
      "UpArrow", "DownArrow", "LeftArrow", "UpArrow"
   }
   for _, key in ipairs(meta_keys) do
      table.insert(conf.keys, { key = key, mods = meta, action = act.SendKey { key = key, mods = "META" }})
      table.insert(conf.keys, { key = key, mods = meta .. "|CTRL", action = act.SendKey { key = key, mods = "META|CTRL" }})
   end
   for _, key in ipairs(meta_shift_keys) do
      table.insert(conf.keys, { key = key, mods = meta .. "|SHIFT", action = act.SendKey { key = key, mods = "META|SHIFT" }})
      table.insert(conf.keys, { key = key, mods = meta .. "|SHIFT|CTRL", action = act.SendKey { key = key, mods = "META|SHIFT|CTRL" }})
   end
end


-- wezterm.on('update-status', function(window)
--   local SOLID_LEFT_ARROW = utf8.char(0xe0b2)
--   -- Grab the current window's configuration, and from it the
--   -- palette (this is the combination of your chosen colour scheme
--   -- including any overrides).
--   local color_scheme = window:effective_config().resolved_palette
--   local bg = color_scheme.background
--   local fg = color_scheme.foreground
--
--   window:set_right_status(wezterm.format({
--     -- first, we draw the arrow...
--     { Background = { Color = 'none' } },
--     { Foreground = { Color = bg } },
--     { Text = SOLID_LEFT_ARROW },
--     -- then we draw our text
--     { Background = { Color = bg } },
--     { Foreground = { Color = fg } },
--     { Text = ' ' .. wezterm.hostname() .. ' ' },
--   }))
-- end)


return conf
