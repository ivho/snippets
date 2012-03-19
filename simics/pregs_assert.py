#!/usr/bin/python
import os

script="""
run-command-file "/space/work/simics/emerson/svn/wksp/target/emerson-sch-us15wp/emerson-sch-us15wp-vxworks.simics"
%(vmpcmd)s
sba %(stepbreak)d
c
pipe "%(expr)s; pregs; ptime" "cat > %(result)s"
exit
"""

def gen_script(expr, stepbreak, vmp, stall, base):
    if vmp:
        vmpcmd=""
        result="%(base)s_vmp.log" %(locals())
    else:
        vmpcmd="disable-vmp"
        result="%(base)s_novmp.log" % (locals())

    f=file("s1.simics", "w")
    f.write(script % (locals()))
    f.close()
    os.system("./simics %s s1.simics > /dev/null" % stall)
    ff=file(result,"r")
    x=ff.read()
    return x


if __name__ == "__main__":
#    fail = 400000
#    ok = 300000
#    fail = 344531
#    ok = 344335
#    gen_script(344, True)
#    gen_script(new, False)

#    expr = "pregs"
#    fail=344341
#    ok=344338

#    fail=331160
#    ok=331159
#    expr="x ds:0x000f_16bc 4"

#    fail=400000
    fail=5036201
    ok=0
    expr="x ds:0x000f_16bc 4"
    while fail - ok != 1:
        print "fail:%d ok:%d (diff %d)" % (fail, ok, fail-ok)
        new = (fail + ok) / 2
        print "trying %d" % new
#        x=gen_script(expr, new, True, "tmp")
#        y=gen_script(expr, new, False, "tmp")
        x=gen_script(expr, new, True, "",  "tmp")
        y=gen_script(expr, new, True, "-stall", "stalltmp")
        if x == y:
            ok=new
            print "same"
        else:
            fail=new
            print "diff"
    print "fail=%d" % fail
    print "ok=%d" % ok
    print "expr=<%s>" % expr


#    x=gen_script(expr, fail, True, "diff")
#    y=gen_script(expr, fail, False, "diff")
#    x=gen_script(expr, ok, True, "ok")
#    y=gen_script(expr, ok, False, "ok")
