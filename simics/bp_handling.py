from  simics import *
import conf
import cli

class subcall(object):
    """ class that defines a subroutine """
    def __init__(self, desc, addr, args):
        self.desc = desc
        self.addr = addr
        self.args = args
        self.ret_val = None

#ppc abi conventions when accessing the cpu.gprs[]
RET_REG = 3
ARG_1   = 3
ARG_2   = 4

def setup_awareness():
    SIM_run_command("$system.software.enable-tracker")

global_id = 0

#how do you access the symtable from python? (got it, see below)
def get_addr(symt, var):
    return SIM_run_command("%s.symtab.pos \"%s\"" % (symt, var))

class bp_handler(object):
    def __init__(self, tracker, symbols):
        self.sub_call_in_progess = False
        self.tracker = tracker
        self.symbols = symbols
        self.c_callback_addr=symbols["callback"]

    def handle___libc_nanosleepXX(self, cpu):
        cpu.pc = cpu.lr
        cpu.gprs[3] = 0

    def handle_kalle(self, cpu):
        cpu.pc = self.c_callback_addr
        cpu.gprs[RET_REG] = cpu.gprs[ARG_1]+1

    def func_return(self, func, mem, bpid, memop):
        print "func_return", func
        cpu=SIM_get_mem_op_initiator(memop)
        print "pc:%x lr:%x" % (cpu.pc, cpu.lr)
        sc=self.sub_call_list[self.current_sub_call]
        sc.ret_val = cpu.gprs[RET_REG]
        print "<%s> returned 0x%08x (or possibly 0x%08x" % (sc.desc,
                                                            sc.ret_val,
                                                            cpu.gprs[4])
        self.current_sub_call += 1
        if (self.current_sub_call == len(self.sub_call_list)):
            print "all subcalls done"
            self.finalize(cpu)
            return
        SIM_thread_safe_callback(self.do_sub_calls, cpu)

    def break_on_return(self, ret_addr):
        print "break on return", hex(ret_addr)
        tmp_id=SIM_run_command("%s.tbreak %d" % (self.tracker, ret_addr))
        SIM_hap_add_callback_index(
            "Core_Breakpoint_Memop",
            self.func_return,
            "this is the shit",
            tmp_id)

    def do_sub_calls(self, cpu):
        print "do_sub_calls %d/%d" % (self.current_sub_call, 
                                      len(self.sub_call_list))
        sc=self.sub_call_list[self.current_sub_call]
        print "next sub call(%d) <%s> 0x%x" % (self.current_sub_call,
                                               sc.desc, sc.addr)
        cpu.pc=sc.addr
        cpu.lr = self.org_lr #always return to org_lr
        cpu.gprs[ARG_1] = sc.args[0] #fixme: only 1 arg for now..
        self.break_on_return(self.org_lr)

    def finalize_apa(self, cpu):
        #return the value from malloc...
        cpu.gprs[RET_REG]=self.sub_call_list[1].ret_val
        print "apa returns ", hex(cpu.gprs[RET_REG])
        self.sub_call_in_progress = False

#void *apa(int x, int (*fp)(int))
    def handle_apa(self, cpu):       
        print "****** handle apa"
        print "   arg1:", cpu.gprs[ARG_1], " - should be 1000)"
        print "   arg2:", hex(cpu.gprs[ARG_2]), " - should be c-callback ", hex(self.symbols["callback"])
        self.sub_call_list=[subcall("user callback", cpu.gprs[ARG_2], [99]),
                            subcall("malloc(2000)", self.symbols["malloc"], [2000]),
                            ]
        self.current_sub_call = 0
        self.finalize = self.finalize_apa;
        self.sub_call_in_progress = True

        #don't let the current instr execute...
        cpu.pc = self.sub_call_list[0].addr
        SIM_thread_safe_callback(self.do_sub_calls, cpu)
        
    def callback(self, func, mem, bpid, memop):
        cpu=SIM_get_mem_op_initiator(memop)
        try:
            fn=self.__getattribute__("handle_%s" % func)
        except AttributeError:
#            print "no handler for func=<%s> bpid=%d" % (func, bpid)
            #TODO: optimize by removing breakpoints that we don't handle... can't do this since we are in execution context here
#            print "removing breakpoint %d" % bpid
#            SIM_run_command("delete %d" % bpid)
            return

        print "found handler for func:%s @ 0x%x lr->0x%x" % (func, SIM_get_mem_op_virtual_address(memop), cpu.lr)
        self.org_lr = cpu.lr
        assert(cpu.current_context.name == self.tracker)
        fn(cpu)


def new_api_tracker(name, elf = None):
    global global_id

    print "id:", global_id
    global_id += 1
    if not elf:
        elf = "../../axxia/test_lib/%s" % (name)

    sym_name=SIM_run_command("new-symtable file = %s" % (elf))
#build a python dict for the symbols for future reference
    symt=SIM_get_object(sym_name)
    symbols={}
    for [func, filename, addr, huh] in symt.functions:
        symbols[func]=addr

    track_name=SIM_run_command("$system.software.track test %s" % sym_name)

#fixme: bph global for debug only
    global bph
    bph=bp_handler(track_name, symbols)

#add breakpoints on all! symbols
 #   for func in symbols:
    for func in ["apa", "kalle"]:
        addr=symbols[func]
        bpid=SIM_run_command("%s.break %d" % (track_name, addr))
        SIM_hap_add_callback_index(
            "Core_Breakpoint_Memop",
            bph.callback,
            func,
            bpid)
        print "added bp callback for ", func

 
def start():
    #fixme: do the following for targets that doesn't have the .software
    #       setup alredy in the scripts
    #  simics> load-module os-awareness
    #  simics> create-os-awareness $system + ".software"
    setup_awareness()
    new_api_tracker("test", "../../axxia/test_lib/test")
    SIM_run_command("$system.console0.con.input \"mount /host\n\"", )
    SIM_run_command("$system.console0.con.input \"/host/space/work/simics/axxia/test_lib/test\n\"")

cli.new_command("new_api_tracker", new_api_tracker, args = [cli.arg(cli.str_t, "name"),cli.arg(cli.str_t, "elf"),], )
cli.new_command("start", start, alias="s")

#
# end of real code here....
#

#add a magic breakpoint
if False:
    magic_id=SIM_run_command("%s.break %d" % (track_name, MAGIC_ADDR))
    SIM_hap_add_callback_index(
        "Core_Breakpoint_Memop",
        bph.magic_break,
        None,
        magic_id)

def kalle():
    print "Test:"
    rune = 99L
    addr=0x12345678
    conf.test0.iface.breakpoint.insert_breakpoint(conf.test0, ruhelpne, 99, 0x12345678,0x12345678)
    print "rune:", rune


#LD_TRACE_LOADED_OBJECTS=1 /host/space/work/simics/axxia/test_lib/test

# LD_TRACE_LOADED_OBJECTS=1 /host/space/work/simics/axxia/test_lib/test
#	linux-vdso32.so.1 =>  (0x00100000)
#	libc.so.6 => /lib/libc.so.6 (0x0fe89000)


#print 
#simics.SIM_run_command(ss)   
#sw = conf.mpc8641d_simple.software.tracker.iface.software
#sw.notify_property_change(sw.root_node_id(), None, True, prop_cb, "")
#self.notifiers.add(
#    sw.notify_property_change(sw.root_node_id(), "name", True,
#                              self.name_cb, process_name))



ss="""
$system.software.enable-tracker
$st = (new-symtable name=apa ../../axxia/test_lib/test)
$track = (mpc8641d_simple.software.track test)
$track.symtable apa
$track.break ($st.pos "kalle")
$track.break ($st.pos "printf")
"""
MAGIC_ADDR=0x12345678

# address to do_ramlog_write
def svenne():
    address = 0x00000c9c988
    cpu = conf.board.soc.cpu[0]
    breakpoint = SIM_breakpoint(
        cpu.physical_memory,
        simics.Sim_Break_Physical,
        simics.Sim_Access_Execute,
        address,
        1,
        simics.Sim_Breakpoint_Simulation)

def prop_cb(process_name, tracker, curcpu, node_id,
            key, old_val, new_val, status):
    print "==============="
    print "proc_name", process_name
    print "tracker", tracker
    print "node_id", node_id
    print "curcpu",   curcpu
    print "key", key
    print "old_val", old_val
    print "new_val", new_val
    print "status", status
    print 
