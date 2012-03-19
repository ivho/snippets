
def callback(cbdata,cpu,nr):
    exc_name=cpu.iface.exception.get_name(nr)
    if exc_name == 'DSI':
        print "DSI PC %08x DEAR %08x (p:%08x)" % ( cpu.pc, cpu.dear,
                                                   cpu.iface.processor_info.logical_to_physical(cpu.dear, Sim_Access_Write).address)



simics.SIM_hap_add_callback(
    "Core_Exception",
    callback,
    None)
#        bp_id)
