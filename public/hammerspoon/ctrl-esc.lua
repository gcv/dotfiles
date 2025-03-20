--- Make Ctrl and Esc share a key: tap for Esc, hold as a modifier for Ctrl

local ctrl_esc_send_escape = false
local ctrl_esc_last_mods = {}

ctrl_esc_ctrl_table = {
    handler = function(evt)
        local new_mods = evt:getFlags()
        if ctrl_esc_last_mods["ctrl"] == new_mods["ctrl"] then
            return false
        end
        if new_mods["ctrl"] then
            -- control key pressed
            ctrl_esc_last_mods = new_mods
            ctrl_esc_send_escape = true
        else
            -- control key released
            ctrl_esc_last_mods = new_mods
            if ctrl_esc_send_escape then
                hs.eventtap.keyStroke({}, "escape")
            end
        end
        return false
    end,
    flags = {ctrl = true}
}

ctrl_esc_ctrl_key_tap = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, ctrl_esc_ctrl_table.handler)
ctrl_esc_ctrl_key_tap:start()

-- If any other key is pressed while ctrl is held, we don't want to send escape:
ctrl_esc_other_key_tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(evt)
    ctrl_esc_send_escape = false
    return false
end)
ctrl_esc_other_key_tap:start()
