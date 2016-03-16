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
#x=conf.board
#x=conf.t4240qds

def print_found():
    for a in found:
        print "  ", found[a]
class Location(object):
# Class to keep track of possible locations for a operating system emit log function.
    def __init__(self, pc):
        self.pc = pc
        self.index = 1
        self.back_log = ""
    def __str__(self):
        return "PC:" + hex(self.pc) + ( " index: %d" % self.index)

def finder(string, mem, bp, memop):
#    print string
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
            print "next hit <%s> %d" % (hex(pc), e.index)
            if e.index == len(string):
                print "sting:", string
                print e
                SIM_log_info(1, cpu, 1, "FOUND IT @ %s after %d hits" % (hex(pc), hits))
                for a in e.back_log.split("\n"):
                    SIM_log_info(1, cpu, 1, "<backlog>:"+a)
                SIM_delete_breakpoint(bp)
                linux_log(pc)
                for a in found:
                    print found[a]
#                SIM_break_simulation("OKIDOKI");
    else:
        if r3 == string[0]:
            print "Found possible <%c> %s" % (r3, hex(pc))
            found[pc] = Location(pc)


# This callback installed once we actuall find a an actuall
# address
def callback(dummy, mem, bp, memop):
    global buf
    global flog
    cpu = simics.SIM_get_mem_op_initiator(memop);
    #print r0 from bank 0 as char
    r3=cpu.gprs[3]
    if r3 > 255:
        SIM_log_info(1, cpu, 1, "printk:garbage %d" % r3)
        return

    buf+=chr(r3)
    if r3 == ord('\n'):
        flog.write(buf)
        flog.flush()
        cpu_index = -1
        for (i,x) in enumerate(conf.board.pic.core):
            if x == cpu:
                cpu_index = i
                break

        print "cpu%d: cycles:%12d - %s" % (cpu_index, cpu.cycles, buf[:-1])
        SIM_log_info(1, cpu, 1, "printk:%s" % buf[:-1])
        buf=""

def linux_log(emit_log_char_addr):
    #fixme: hardcoded name of $system here
    global flog
    cell = conf.board.cell_context
    flog = file("/tmp/printk.log", "w")
    bp_id = SIM_breakpoint(
        cell,
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


cli.new_command("linux-logXX", linux_log,
                [arg(int_t, "emit_log_char_addr")],
                short = "...",
                doc = "...")

cli.new_command("find-emit", linux_find_emit,
                [arg(str_t, "string", "?", "<6>Starting Linux")],
                short = "...",
                doc = "...")


def watcher(desc, mem, bp, memop):
    cpu = simics.SIM_get_mem_op_initiator(memop)
#    print memop
    SIM_log_info(1, cpu, 1, "<%s> hits" % (desc))

def watch_addr(addr, length, desc):
    (t,a) = addr
    print "addr:", addr
    print "desc:", desc
    print "len:", length
    for (access, access_desc) in [(simics.Sim_Access_Read, "read"),
                                  (simics.Sim_Access_Write, "write")]:

        bp_id = SIM_breakpoint(conf.board.cell_context,
                               simics.Sim_Break_Virtual,
                               access,
                               a, length, Sim_Breakpoint_Private)
        hap_handle = simics.SIM_hap_add_callback_index(
            "Core_Breakpoint_Memop",
            watcher,
            access_desc+":"+desc,
            bp_id)



cli.new_command("watch-sym", watch_addr,
                [arg(addr_t, "addr"),
                 arg(int_t, "len", "?", 4),
                 arg(str_t, "desc", "?", "")],
                short = "...",
                doc = "...")


def tb_info():
    x=len(conf.board.cpu[0])
    tb=[None]*128
    for (i,a) in enumerate(conf.board.cpu):
        for (j, cpu) in enumerate(a):
            index = i*x+j
#            print index, cpu
            tb[index] = cpu.tbl+(cpu.tbu<<32)
            print "%d: % 10d % 10d (%s) (%08x:%08x)" % (index, tb[index], tb[0] - tb[index], cpu.timebase_mode,
                                                  cpu.tbu, cpu.tbl)

cli.new_command("time-base-info", tb_info,
                short = "...",
                doc = "...")

count=[0]*4
def tlb_trace(desc, mem, bp, memop):
    global count
    cpu = simics.SIM_get_mem_op_initiator(memop)
    count[cpu.pir] += 1
#    print "%d:%d:%d"% (cpu.pir,count[cpu.pir],cpu.cycles)

def trace_tlbwe(base = 0xc000000000000000, size  = 1<<(64-4), prefix = "tlbwe"):
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
    SIM_run_command("set-prefix %d tlbwe" % bp_id)
    hap_handle = simics.SIM_hap_add_callback_index(
        "Core_Breakpoint_Memop",
        tlb_trace,
        string,
        bp_id)


cli.new_command("trace-tlb", trace_tlbwe,
                short = "...",
                doc = "...")

def map_trace(desc, mem, bp, memop):
    global count
    cpu = simics.SIM_get_mem_op_initiator(memop)
    cpu.iface
#    p = cpu.iface.processor_info_v2.logical_to_physical(0x0 , simics.Sim_Access_Write)
#    print "tlb:", p.valid
#    print "tlbwe"
#    SIM_run_command("board.cpu[0][0].print-l2tlb")
    count  = 0
    if tlb1_valid(3):
        SIM_break_simulation("valid...");

def tlb1_valid(index):
    return (conf.board.cpu[0][0].tlb1[index][1]&0x80000000) != 0

def break_on_mapped(base = 0x0000000000000000, size  = 1<<(64-4), prefix = "tlbwe"):
    #fixme: hardcoded name of $system here

    bp_id = SIM_breakpoint(
        conf.board.cell_context,
        simics.Sim_Break_Virtual,
        simics.Sim_Access_Execute,
        base,
        size,
        Sim_Breakpoint_Private)
#    SIM_run_command("set-prefix %d tlbwe" % bp_id)
    hap_handle = simics.SIM_hap_add_callback_index(
        "Core_Breakpoint_Memop",
        map_trace,
        None,
        bp_id)
def show_l2tlb():
    cpu = conf.board.cpu[0][0]
    
    for (i,a) in enumerate(cpu.tlb1):
        if a[1]&0x80000000:
            print "%d:" % i, map(hex, a)


cli.new_command("break-on-mapped", break_on_mapped)
cli.new_command("l2tlb", show_l2tlb)

