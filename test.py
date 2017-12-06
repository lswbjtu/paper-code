from BatchJob import BatchJob

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

pAllDataPath='/Users/zhuoling/Documents/ML/ICBC/BatchJob/201712/BatchJobAll_dev.xlsx'
pDataPath = r'/Users/lsw/Desktop/Batch/'

bj = BatchJob(pAllDataPath)
#bj.GenCoreFeatures()
#bj.AtomPrediction('CBB320A9','2000-01-01','2017-07-15','2017-08-07')
#print(bj.dfJobNameFeature[0:5])
#print(bj.LastEvent(jobname_mbj,'2017-07-15'))
bj.JobName=bj.JOBNAME_MBJ
bj.GenJobNameFeatures()
l_train,l_test=bj.SplitTrainTestWithWeight(bj.dfJobNameFeature,'2017-07-01','2017-07-15','2017-07-16')
print(len(l_train),len(l_test))
print(l_train)
print(l_test)