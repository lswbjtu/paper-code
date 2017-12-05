import dis

myglobal = True


def add(a):
    b = 1
    a += b
    return a


class world:
    def __init__(self):
        pass

    def sayHello(self):
        print('hello,world')



w = world()
w.sayHello()
print('Hello,zhuoling')
print(add(9))