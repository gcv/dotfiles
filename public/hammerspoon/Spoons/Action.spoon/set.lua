local obj = { name = "Set" }
obj.__index = obj

function obj.new(id, intervals, env, excludedSSIDs, actions)
   local self = {
      id = id,
      intervals = intervals,
      env = env,
      excludedSSIDs = excludedSSIDs,
      actions = actions,
      lastActions = {},
      status = nil,
      timers = {},
      task = nil,
   }
   setmetatable(self, obj)
   return self
end

function obj:start()
   if self.app.conf.debug then print("ActionSpoon:", "starting timers for set '" .. self.id .. "'") end
   for idxCmd, _ in ipairs(self.intervals.commands) do
      self:startCommand(idxCmd)
   end
end

function obj:stop()
   if "stopped" == self.status then
      -- do nothing
      return
   end
   -- if a task is running, we must interrupt it
   if "running" == self.status and self.task and self.task:isRunning() then
      self.task:interrupt()
   end
   -- stop the timers
   for idxTimer, timer in ipairs(self.timers) do
      if self.app.conf.debug then print("ActionSpoon:", "stopping command[" .. idxTimer .. "] timer for set '" .. self.id .. "'") end
      self.timers[idxTimer]:stop()
   end
   self:updateStatus("stopped")
end

function obj:startCommand(idxCmd)
   self.lastActions[idxCmd] = self.lastActions[idxCmd] or os.time()
   local nextAction = self.lastActions[idxCmd] + self.intervals.commands[idxCmd] - os.time()
   -- if nextAction is in the past (i.e., < 0), then set it to run a minute from now
   if nextAction <= 0 then
      nextAction = 60
   end
   self.timers[idxCmd] = hs.timer.new(
      nextAction,
      function()
         self:goCommand(idxCmd)
      end,
      true -- continueOnError
   )
   self.status = nil
   self.timers[idxCmd]:start()
end

function obj:goCommand(idxCmd)
   if self.app.conf.debug then print("ActionSpoon:", "command[" .. idxCmd .. "] trigger for set '" .. self.id .. "'") end
   -- avoid simultaneous runs! try again in <poll> min
   local isRunning = "running" == self.status
   local currentSSID = hs.wifi.currentNetwork()
   local isExcludedSSID = false
   for idxSSID, rxSSID in ipairs(self.excludedSSIDs) do
      if string.find(currentSSID, rxSSID) then
         isExcludedSSID = true
         break
      end
   end
   if isRunning or isExcludedSSID then
      if self.app.conf.debug and isRunning then print("ActionSpoon:", "command[" .. idxCmd .. "] delayed while another task is (still?) running for set '" .. self.id .. "'") end
      if self.app.conf.debug and isExcludedSSID then print("ActionSpoon:", "command[" .. idxCmd .. "] delayed while on excluded SSID '" .. self.id .. "', " .. currentSSID) end
      self.timers[idxCmd]:setNextTrigger(self.intervals.poll)
      return
   end
   self:helper(1, idxCmd)
end

function obj:helper(idxAction, idxCmd)
   local entry = self.actions[idxAction]
   if not entry then
      -- base case: after last run, so finished successfully
      self.lastActions[idxCmd] = os.time()
      self.timers[idxCmd]:setNextTrigger(self.intervals.commands[idxCmd])
      self:updateStatus("ok")
      self.task = nil
      self.app:stateFileWrite()
   elseif "function" == type(entry.commands[idxCmd]) then
      -- simple: call the command, recurse
      entry.commands[idxCmd]()
      self:updateStatus("running")
      self:helper(idxAction + 1, idxCmd)
   elseif "table" == type(entry.commands[idxCmd]) then
      local splits = hs.fnutils.copy(entry.commands[idxCmd])
      local cmd = self.app.Utils.findExecutable(table.remove(splits, 1))
      self.task = hs.task.new(
         cmd,
         function(code, stdout, stderr)
            if 0 == code then
               if self.app.conf.debug then print("ActionSpoon:", "task successful: " .. self.id .. ", " .. entry.id .. ", " .. idxCmd) end
               -- recurse to the next entry
               self:helper(idxAction + 1, idxCmd)
            else
               -- task failed or interrupted
               if "interrupt" == self.task:terminationReason() then
                  if self.app.conf.debug then print("ActionSpoon:", "task interrupted, code: " .. code .. ", stderr:" .. stderr) end
                  if self.status ~= "stopped" then
                     self:updateStatus("interrupted")
                  end
               else
                  -- always print errors to console, regardless of debug flag
                  print("ActionSpoon:", "task failed: " .. self.id .. " : " .. entry.id .. "; code: " .. code .. ", stderr:" .. stderr)
                  self.app:notify("error", "Task failed: " .. self.id .. ", " .. entry.id)
                  self:updateStatus("error")
               end
               -- do not recurse to the next entry, retry the whole run later
               if "stopped" ~= self.status then
                  self.timers[idxCmd]:setNextTrigger(self.intervals.poll)
               end
            end
         end,
         splits -- rest of the args
      )
      local taskEnv = self.app.Utils.merge(self.task:environment(), self.env)
      local pathToAdd = table.concat(
         self.app.Utils.map(
            function (pe)
               return hs.fs.pathToAbsolute(pe)
            end,
            self.app.conf.path),
         ":")
      if not string.find(taskEnv["PATH"], pathToAdd) then
         taskEnv["PATH"] = ((taskEnv["PATH"] .. ":") or "") .. pathToAdd
      end
      if self.app.conf.debug then print("ActionSpoon:", "task path: " .. hs.inspect(taskEnv["PATH"])) end
      self:updateStatus("running")
      self.task:setEnvironment(taskEnv)
      local directory = hs.fs.pathToAbsolute(entry.directory or self.directory or self.app.conf.directory or self.app.spoonPath)
      if self.app.conf.debug then print("ActionSpoon:", "task working directory:", directory) end
      self.task:setWorkingDirectory(directory)
      self.task:start()
   end
end

function obj:display()
   local res = {}
   local fmt = "%Y-%m-%d (%a) %X" -- equivalent to "%H:%M:%S"
   local setTitle = ""
   -- status
   if "ok" == self.status then
      setTitle = setTitle .. "✓"
   elseif "error" == self.status then
      setTitle = setTitle .. "!"
   elseif "running" == self.status then
      setTitle = setTitle .. "⟳"
   elseif "stopped" == self.status then
      setTitle = setTitle .. "×"
   else
      -- nil or "interrupted"
      setTitle = setTitle .. "•"
   end
   -- path
   setTitle = setTitle .. " " .. self.id .. ": " ..
      -- join visible titles of all actions in the set
      table.concat(self.app.Utils.map(
                      function (b)
                         return b.id
                      end,
                      self.actions),
                   ", ")
   res[#res+1] = {
      title = setTitle,
      disabled = ("running" == self.status or "stopped" == self.status),
      fn = function()
         self:goCommand(1)
      end
   }
   -- additional information
   for idxLastAction, lastAction in ipairs(self.lastActions) do
      res[#res+1] = {
         title = "   - last action[" .. idxLastAction .. "]: " .. os.date(fmt, lastAction),
         disabled = true
      }
   end
   for idxTimer, timer in ipairs(self.timers) do
      res[#res+1] = {
         title = "   - next action[" .. idxTimer.. "]: " .. os.date(fmt, math.floor(os.time() + timer:nextTrigger())),
         disabled = true
      }
   end
   -- done
   return res;
end

function obj:updateStatus(newStatus)
   self.status = newStatus
   self.app:updateMenuIcon()
end

return obj
