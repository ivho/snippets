
define hook-quit
    set confirm off
end

# No paginig
set height 0

set history filename ~/.gdb_history
set history save on
set print static-members off
set print vtbl on
set print object on
set print pretty on

set tcp auto-retry on
# High timeout, when running the gdbserver on target in simics 
# it might hang indefinetly when simics is stopped.
set remotetimeout 3600

define functiontracestack
if $arg0
    break $arg0
    commands
        where
        continue
    end
end

