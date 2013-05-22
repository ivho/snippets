def debugFunc(f):
    def dbg(*arg, **kwarg):
        print "calling: %s" % f.__name__
        for x in arg:
            print "arg: ", x
        for k in kwarg:
            print ("kwarg: %s:" % k), kwarg[k]
        f(*arg, **kwarg)
    return dbg

@debugFunc
def hello(x,y,z):
    pass

hello(1,2,3)
hellor(1,2,z=3)
