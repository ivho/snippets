#
# Search for a function that emits system log-message (i.e printk)
#
# quick and dirty....
# TODO:
#   - Search for multiple strings.
#   - Test on more SW (u-boot, vxworks...)
#   - Support arm/x86/mips etc
#

buf = ""
found = {}
hits = 0
class Location(object):
# Class to keep track of possible locations for a operating system emit log function.
    def __init__(self, pc):
        self.pc = pc
        self.index = 1
        self.back_log = ""
    def __str__(self):
        return "PC:" + hex(self.pc) + ( " index: %d" % self.index)

def finder(string, mem, bp, memop):
    cpu = simics.SIM_get_mem_op_initiator(memop);
    pc=cpu.pc
    if cpu.gprs[3] > 255:
        return
    global hits
    hits += 1
    r3=chr(cpu.gprs[3])

    if pc in found:
        e=found[pc]
        e.back_log += r3
        if r3 == string[e.index]:
            e.index += 1
            if e.index == len(string):
                SIM_log_info(1, cpu, 1, "FOUND IT @ %s after %d hits" % (hex(pc), hits))
                for a in e.back_log.split("\n"):
                    SIM_log_info(1, cpu, 1, "<backlog>:"+a)
                SIM_delete_breakpoint(bp)
                linux_log(pc)
    else:
        if r3 == string[0]:
            found[pc] = Location(pc)


# This callback installed once we actuall find a an actuall
# address
def callback(dummy, mem, bp, memop):
    global buf
    global flog
    cpu = simics.SIM_get_mem_op_initiator(memop);
    #print r0 from bank 0 as char
    r3=cpu.gprs[3]
    buf+=chr(r3)
    if r3 == ord('\n'):
        flog.write(buf)
        flog.flush()
        SIM_log_info(1, cpu, 1, "printk:%s" % buf[:-1])
        buf=""

def linux_log(emit_log_char_addr):
    #fixme: hardcoded name of $system here
    global flog
    flog = file("/tmp/printk.log", "w")
    bp_id = SIM_breakpoint(
        conf.board.cell_context,
        simics.Sim_Break_Virtual,
        simics.Sim_Access_Execute,
        emit_log_char_addr,
        4,
        Sim_Breakpoint_Private)

    hap_handle = simics.SIM_hap_add_callback_index(
        "Core_Breakpoint_Memop",
        callback,
        None,
        bp_id)

def linux_find_emit(string, base = 0xc000000000000000, size  = 1<<(64-4), prefix = "bl"):
    #fixme: hardcoded name of $system here
    global flog
    print string
    bp_id = SIM_breakpoint(
        conf.board.cell_context,
        simics.Sim_Break_Virtual,
        simics.Sim_Access_Execute,
        base,
        size,
        Sim_Breakpoint_Private)
    SIM_run_command("set-prefix %d bl" % bp_id)
    hap_handle = simics.SIM_hap_add_callback_index(
        "Core_Breakpoint_Memop",
        finder,
        string,
        bp_id)

cli.new_command("find-emit", linux_find_emit,
                [arg(str_t, "string", "?", "<6>Starting Linux")],
                short = "...",
                doc = "...")


