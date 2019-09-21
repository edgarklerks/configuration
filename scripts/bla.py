class Plop(object):
    def __init__(self,t):
        self.x = t

def lol(t = Plop(1)):
    lol.func_defaults = (t, lol.func_defaults)


print lol()
print lol()

print lol.func_defaults
