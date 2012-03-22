#
# quick and dirty....
#
buf=""

def irq_status_callback(a,b,c):
    """Core_Interrupt_Status is called whenever an x86 enabled/disabled irq"""
    SIM_log_info(1, b, 1, "irq status %d %s" % (c, a))


def generic_hap(cb_data, dev, *a):
    SIM_log_info(1, dev, 1, "hap:%s" % cb_data)
    print a

def irq_status():
#    hap_handle = simics.SIM_hap_add_callback(
#        "Core_Interrupt_Status",
#        irq_status_callback,
#        "HEllo")
#    ghaps=["Core_Control_Register_Read", "Core_Control_Register_Write"]
    ghaps=["Core_Mode_Change"]
    for x in ghaps:
        print "installing hap:%s" % x
        hap_handle = simics.SIM_hap_add_callback(x, generic_hap, x)



cli.new_command("irq-status", irq_status,
                [],
                short = "...",
                doc = "...")


import conf
def x():
    FALSE=False
    TRUE=True
    x=conf.ovation.mb.cpu.core[0][0]
    x.debug_len_10b_8_bytes=          FALSE
    x.disable_block_merge=             0
    x.lar_ldt_lm_invalid=              FALSE
    x.load_far_ptr_64=                 FALSE
    x.near_branches_64=                FALSE
    null_clear_base_and_limit=       FALSE
    one_step_per_string_instruction= FALSE
    pause_slow_cycles=               0x0
    port_io_slow_cycles=             0x0
    rdtsc_slow_cycles=               0x0
    seg_push_zero_pad=               TRUE
    skip_canonical_logical_check=    FALSE
    vm_compatible_config=            FALSE

def y():
    FALSE=False
    TRUE=True
    x=conf.ovation.mb.cpu.core[0][0]
#    x.debug_len_10b_8_bytes=          TRUE
#    x.disable_block_merge=             1
#    x.lar_ldt_lm_invalid=              TRUE
#    x.load_far_ptr_64=                 TRUE
#    x.near_branches_64=                TRUE
#    x.null_clear_base_and_limit=       TRUE
#    x.one_step_per_string_instruction= TRUE
    x.pause_slow_cycles=               0x4e20
    x.port_io_slow_cycles=             0x4e20
    x.rdtsc_slow_cycles=               0x4e20
#    x.seg_push_zero_pad=               FALSE
#    x.skip_canonical_logical_check=    TRUE
#    x.vm_compatible_config=            TRUE


import turbo_utils
import sys

def advance():
    SIM_run_command_file("/space/work/simics/emerson/svn/wksp/target/emerson-sch-us15wp/emerson-sch-us15wp-vxworks.simics", False)
    y()
    x=int(file("offset", "r").read())
    print x
    global offset
    offset = x
    SIM_run_command("sba %d ; c" % x)

def re_run():
#    internals()
    advance()
#    SIM_run_command("disable-vmp")
#    conf.ovation.mb.cpu.core[0][0].turbo_threshold = 1
    SIM_load_module("state-assertion")
    SIM_run_command("state-assertion-simple-assert file = ./kalle.gz compression = gz")
 #   SIM_run_command("c")
 #   SIM_run_command('pipe ptime "cat > assert.%d"' % offset)
#    sys.exit(0)

def create():
    advance()
    SIM_load_module("state-assertion")
    SIM_run_command("state-assertion-simple-record kalle.gz compression = gz steps = 1 object = ovation.mb.cpu.core[0][0]")
    SIM_run_command("c")
#    sys.exit(0)




