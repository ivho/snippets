#
# quick and dirty....
#
buf=""

def callback(dummy, mem, bp, memop):
    global buf
    cpu = simics.SIM_get_mem_op_initiator(memop);
    #print r0 from bank 0 as char
    buf+=chr(cpu.gprs[0][0])
    if cpu.gprs[0][0] == ord('\n'):
        print buf,
        buf=""

def linux_log(emit_log_char_addr):
    #fixme: hardcoded name of $system here
    bp_id = SIM_breakpoint(
        conf.uatx.cell_context,
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

cli.new_command("linux-log", linux_log,
                [arg(int_t, "emit_log_char_addr")],
                short = "...",
                doc = "...")


