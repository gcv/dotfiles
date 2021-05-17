--- === Action Spoon ===
---
--- Orchestrates running scheduled tasks. Useful for automatic backup of local
--- data using command-line utilities such as Restic, Kopia, and Borg.
---
--- Download: https://github.com/gcv/action-spoon/releases/download/1.0.0beta1/Action.spoon.zip

local obj = {}
obj.__index = obj

--- Metadata
obj.name = "Action"
obj.version = "1.0.0beta1"
obj.author = "gcv"
obj.homepage = "https://github.com/gcv/action.spoon"
obj.license = "CC0"

--- Internal function used to find code location.
local function scriptPath()
    local str = debug.getinfo(2, "S").source:sub(2)
    return str:match("(.*/)")
end
obj.spoonPath = scriptPath()

--- Objects:
local Utils = dofile(obj.spoonPath .. "/utils.lua")
local Set = dofile(obj.spoonPath .. "/set.lua")

--- Internal state:
obj.confFile = (os.getenv("XDG_CONFIG_HOME") or (os.getenv("HOME") .. "/.config")) .. "/ActionSpoon.lua"
obj.conf = setmetatable({}, {__index=_G}) -- XXX: Allow function calls in conf file! @@
obj.conf.debug = false                    -- XXX: This inherits a debug field! @@
obj.state = {}
obj.sets = {}
obj.active = false
obj.watcher = nil

--- Resources:
obj.menuIconNormal = hs.image.imageFromPath(obj.spoonPath .. "/resources/menu-icon-normal.png")
obj.menuIconError = hs.image.imageFromPath(obj.spoonPath .. "/resources/menu-icon-error.png")
obj.menuIconInactive = hs.image.imageFromPath(obj.spoonPath .. "/resources/menu-icon-inactive.png")
obj.menuIconRunning = hs.image.imageFromPath(obj.spoonPath .. "/resources/menu-icon-running.png")
obj.menuIconInterrupted = hs.image.imageFromPath(obj.spoonPath .. "/resources/menu-icon-interrupted.png")
obj.notifyIconNormal = hs.image.imageFromPath(obj.spoonPath .. "/resources/notify-icon-normal.png")
obj.notifyIconError = hs.image.imageFromPath(obj.spoonPath .. "/resources/notify-icon-error.png")

--- Action:init()
--- Method
--- Initialize Action.
---
--- Parameters:
---  * None
---
--- Returns:
---  * None
function obj:init()
   -- read conf file
   local confFn, err = loadfile(self.confFile, "t", self.conf)
   if confFn then
      confFn()
   else
      print("ActionSpoon:", err)
      obj:notify("error", "Failed to load. Missing configuration file?")
      return
   end
   -- bail out if disabled; omission equivalent to "enabled = true"
   if nil ~= self.conf.enabled and (not self.conf.enabled) then
      return
   end
   -- version check
   self:versionCheck()
   -- configure helper object prototypes
   Utils.app = self
   self.Utils = Utils
   Set.app = self
   -- process conf file: sensible defaults
   if not self.conf.stateFile then
      self.conf.stateFile = (os.getenv("XDG_CONFIG_HOME") or (os.getenv("HOME") .. "/.config")) .. "/ActionSpoon-state.json"
   else
      self.conf.stateFile = hs.fs.pathToAbsolute(self.conf.stateFile)
   end
   local spoonPathExtra = self.spoonPath .. "resources"
   if not self.conf.path then
      self.conf.path = { "/bin", "/usr/bin", "/usr/local/bin", spoonPathExtra }
   else
      hs.fnutils.concat(self.conf.path, { spoonPathExtra }) -- XXX: destructive!
   end
   if self.conf.debug then print("ActionSpoon:", "path: ", hs.inspect(self.conf.path)) end
   if not self.conf.excludedSSIDs then
      self.conf.excludedSSIDs = {}
   end
   if not self.conf.intervals then
      self.conf.intervals = {
         poll = 5 * 60, -- 5 minutes
         commands = {}
      }
   end
   if not self.conf.intervals.poll then
      self.conf.intervals.poll = 5 * 60 -- 5 minutes
   end
   -- process root environment variables
   self.conf.env = Utils.readEnvs(self.conf.environment)
   -- process conf file: for each set, create a new Set object
   for idx, set in ipairs(self.conf.sets) do
      local id = set.id
      local intervals = set.intervals or self.conf.intervals
      intervals.poll = intervals.poll or self.conf.intervals.poll
      intervals.commands = intervals.commands or self.conf.intervals.commands
      local excludedSSIDs = set.excludedSSIDs or self.conf.excludedSSIDs
      local env = Utils.merge(self.conf.env, Utils.readEnvs(set.environment))
      local actions = set.actions
      self.sets[#self.sets+1] = Set.new(id, intervals, env, excludedSSIDs, actions)
   end
   if 0 == #self.sets then
      self:notify("error", "No action sets defined. Check configuration.")
   end
   -- process state file
   self:stateFileRead()
   -- set up menu icon
   self.menu = hs.menubar.new()
   self:updateMenuIcon()
   self.menu:setMenu(self.makeMenuTable)
   -- activate system watcher
   self.watcher = hs.caffeinate.watcher.new(
      function(evt)
         self:systemWatchFn(evt)
      end
   )
   -- go
   self:start()
   return self
end

--- Action:start()
--- Method
--- Start Action.
---
--- Parameters:
---  * None
---
--- Returns:
---  * None
function obj:start()
   obj.active = true
   for idx, set in ipairs(obj.sets) do
      set:start()
   end
   obj.watcher:start()
   obj:updateMenuIcon()
end

--- Action:stop()
--- Method
--- Stop Action.
---
--- Parameters:
---  * None
---
--- Returns:
---  * None
function obj:stop()
   obj.watcher:stop()
   for idx, set in ipairs(obj.sets) do
      set:stop()
   end
   obj.active = false
   obj:updateMenuIcon()
end

function obj:makeMenuTable()
   local res = {}
   res[#res+1] = { title = "-" }
   for idx, set in ipairs(obj.sets) do
      local displayItems = set:display()
      for _, displayItem in ipairs(displayItems) do
         res[#res+1] = displayItem
      end
   end
   res[#res+1] = { title = "-" }
   if obj.conf.debug then
      res[#res+1] = {
         title = "Debug...",
         fn = function()
            print("Configuration: ", hs.inspect(obj.conf))
            print("State: ", hs.inspect(obj.state))
            print("Sets: ", hs.inspect(obj.sets))
         end
      }
   end
   if obj.active then
      res[#res+1] = {
         title = "Disable",
         fn = function()
            obj.stop()
         end
      }
   else
      res[#res+1] = {
         title = "Enable",
         fn = function()
            obj.start()
         end
      }
   end
   return res
end

function obj:systemWatchFn(event)
   if not obj.active then
      return
   end
   if hs.caffeinate.watcher.systemWillSleep == event then
      for idx, set in ipairs(obj.sets) do
         set:stop()
      end
   elseif hs.caffeinate.watcher.systemDidWake == event then
      for idx, set in ipairs(obj.sets) do
         set:start()
      end
   end
end

function obj:updateMenuIcon()
   if not obj.active then
      if obj.menu:icon() ~= obj.menuIconInactive then
         obj.menu:setIcon(obj.menuIconInactive, false)
      end
      return
   end
   local anyErrors = false
   local anyInterrupted = false
   local anyRunning = false
   for idx, set in ipairs(obj.sets) do
      if "error" == set.status then
         anyErrors = true
      elseif "interrupted" == set.status then
         anyInterrupted = true
      elseif "running" == set.status then
         anyRunning = true
      end
   end
   if anyErrors and obj.menu:icon() ~= obj.menuIconError then
      obj.menu:setIcon(obj.menuIconError, false)
   elseif anyInterrupted and obj.menu:icon() ~= obj.menuIconInterrupted then
      obj.menu:setIcon(obj.menuIconInterrupted, false)
   elseif anyRunning and obj.menu:icon() ~= obj.menuIconRunning then
      obj.menu:setIcon(obj.menuIconRunning, true)
   elseif obj.menu:icon() ~= obj.menuIconNormal then
      obj.menu:setIcon(obj.menuIconNormal, true)
   end
end

function obj:notify(kind, text)
   local msg = hs.notify.new(
      nil,
      {
         title = "Action Spoon",
         informativeText = text,
         withdrawAfter = 0,
         setIdImage = ("error" == kind) and obj.notifyIconError or obj.notifyIconNormal
      }
   )
   msg:send()
end

function obj:stateFileRead()
   obj.state = hs.json.read(obj.conf.stateFile)
   -- reconcile the state file with the configuration
   for idxSet, set in ipairs(obj.sets) do
      if obj.state and obj.state[set.id] then
         set.lastActions = obj.state[set.id].lastActions
      end
   end
end

function obj:stateFileWrite()
   local fmt = "%Y-%m-%d (%a) %X" -- equivalent to "%H:%M:%S"
   local state = {}
   for idxSet, set in ipairs(obj.sets) do
      state[set.id] = {}
      state[set.id].lastActions = set.lastActions
      state[set.id].lastActionsAsStrings = Utils.map(
         function(la)
            return os.date(fmt, la)
         end,
         set.lastActions
      )
   end
   local res = hs.json.write(state, obj.conf.stateFile, true, true)
   if not res then
      obj:notify("error", "Failed to write state file")
   end
end

function obj:versionCheck()
   local versionUrl = "https://raw.githubusercontent.com/wiki/gcv/action-spoon/version.txt"
   hs.http.asyncGet(
      versionUrl,
      nil,
      function(status, body, resp)
         if 200 ~= status then
            -- whatever
            return
         end
         local ver = body:gsub("[ \n]*$", "")
         if ver ~= obj.version then
            local n = hs.notify.new(
               function()
                  hs.osascript.applescript("open location \"https://github.com/gcv/action-spoon/releases\"")
               end,
               {
                  title = "Action Spoon",
                  informativeText = "New version available: " .. ver .. ".\nCurrently installed: " .. obj.version .. ".",
                  withdrawAfter = 0,
                  hasActionButton = true,
                  actionButtonTitle = "Visit Page",
                  setIdImage = obj.notifyIconNormal
               }
            )
            n:send()
         end
      end
   )
end

return obj
