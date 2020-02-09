local obj = { name = "Sync" }
obj.__index = obj

function obj.new(displayPath, interval, excludes)
   local self = {
      displayPath = displayPath,
      interval = interval,
      excludes = excludes,
      lastSync = nil,
      status = nil,
      started = nil,
      timer = nil,
      task = nil
   }
   setmetatable(self, obj)
   return self
end

function obj:start()
   if self.app.conf.debug then print("GitSyncSpoon:", "starting sync for ", self.displayPath) end
   self.timer = hs.timer.new(
      self.interval,
      function()
         self:go()
      end,
      true -- continueOnError
   )
   self.started = os.time()
   self.status = nil
   self.timer:start()
end

function obj:pause()
   if self.app.conf.debug then print("GitSyncSpoon:", "pausing sync", self.displayPath) end
   self.timer:stop()
end

function obj:unpause()
   if self.app.conf.debug then print("GitSyncSpoon:", "unpausing sync", self.displayPath) end
   self.timer:start()
   if self.app.conf.debug then
      local nextTrigger = math.floor(self.timer:nextTrigger())
      print("GitSyncSpoon:",
            "current time:", os.date("%X", os.time()),
            "next sync in:", nextTrigger,
            "at time:", os.date("%X", math.floor(os.time() + nextTrigger)))
   end
end

function obj:stop()
   if self.app.conf.debug then print("GitSyncSpoon:", "stopping sync for ", self.displayPath) end
   self:updateStatus("stopped")
   if self.timer then
      self.timer:stop()
      self.timer = nil
      self.started = nil
   end
end

function obj:go()
   -- savedStatus complexity is to allow running a manual sync even when the
   -- service is stopped and go back to the same status it was in before
   local savedStatus = self.status
   if "running" == savedStatus then
      return
   end
   self.lastSync = os.time()
   local realPath = hs.fs.pathToAbsolute(self.displayPath)
   if nil == realPath then
      self:updateStatus("error")
      return
   end
   -- do actual work
   self:updateStatus("running")
   self.task = hs.task.new(
      self.app.gitSyncScript,
      function(code, stdout, stderr)
         if 0 == code then
            if self.app.conf.debug then print("GitSyncSpoon:", "sync successful") end
            if "stopped" == savedStatus then
               self:updateStatus("stopped")
            else
               self:updateStatus("ok")
            end
         else
            if self.app.conf.debug then print("GitSyncSpoon:", "sync failed: " .. stdout .. stderr) end
            self:updateStatus("error")
         end
      end
   )
   local env = self.task:environment()
   env["PATH"] = self.app.conf.gitDirectory .. ":/usr/bin:/bin"
   env["GIT_SYNC_EXCLUDES"] = self.excludes
   self.task:setEnvironment(env)
   self.task:setWorkingDirectory(realPath)
   self.task:start()
end

function obj:display()
   local fmt = "%X" -- equivalent to "%H:%M:%S"
   local resTitle = ""
   -- status
   if "ok" == self.status then
      resTitle = resTitle .. "✓"
   elseif "error" == self.status then
      resTitle = resTitle .. "!"
   elseif "running" == self.status then
      resTitle = resTitle .. "⟳"
   elseif "stopped" == self.status then
      resTitle = resTitle .. "×"
   else
      resTitle = resTitle .. "•"
   end
   -- path
   resTitle = resTitle .. " " .. self.displayPath
   -- last and next sync
   if self.lastSync then
      resTitle = resTitle ..
         " (last: " .. os.date(fmt, self.lastSync) ..
         "; next: " .. os.date(fmt, math.floor(os.time() + self.timer:nextTrigger())) .. ")"
   elseif self.started then
      resTitle = resTitle ..
         " (next: " .. os.date(fmt, math.floor(os.time() + self.timer:nextTrigger())) .. ")"
   end
   -- done
   return {
      title = resTitle,
      disabled = ("running" == self.status),
      fn = function()
         self:go()
      end
   }
end

function obj:updateStatus(newStatus)
   self.status = newStatus
   self.app:updateMenuIcon()
end

return obj
