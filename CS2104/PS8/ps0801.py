def task1(x):
    reg = x[0]
    yield
    reg = reg+1  # that would be observed in realistic hardware
    yield
    x[0] = reg
    yield
def task2(x):
    reg = x[0]
    yield
    reg = reg+1  # that would be observed in realistic hardware
    yield
    x[0] = reg
    yield
def scheduler():
    x=[0]
    tasks = [task1(x),task2(x)]
    for i in range(6):
tasks[i/3].next()
print "x=",x[0] # will print “1” due to interfering increment