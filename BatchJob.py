import datetime

import numpy as np
import pandas as pd

pd.options.mode.chained_assignment = None  # default='warn'

# %matplotlib inline

jobname_mbj="MASTERBATCH"

class BatchJob:
    def __init__(self,pJobListPath,pJobRunDataPath,pCalendarPath,pTransRatePath,pEventPath):
        self.JobListPath=pJobListPath
        self.JobRunDataPath=pJobRunDataPath
        self.CalendarPath=pCalendarPath
        self.TransRatePath=pTransRatePath
        self.EventPath=pEventPath

        self.dfCoreFeatures=pd.DataFrame()
#        self.dfCoreFeatures = self.GenCoreFeatures()

        self.JobName=jobname_mbj
        self.TargetDate=datetime.date.today()
        self.dfEvent=pd.read_excel(pEventPath)


    def GenCoreFeatures(self):
        dfTransRate= pd.read_excel(pTransRatePath)
        dfTransRate['DATE']=dfTransRate['DATE'].apply(lambda x: x+ datetime.timedelta(days=1))
        dfTransRate = dfTransRate.fillna(value=0)

        def set_batch_type(x):
            if x == 'd01':
                return '1'
            else:
                if (x == 'd02') or (x == 'd06') or (x == 'd12') or (x == 'd21') or (x == 'd26'):
                    return '2'
            return '0'

        dfCalendar=pd.read_excel(pCalendarPath)
        dfCalendar['batch_type'] = dfCalendar['Day'].apply(lambda x: set_batch_type(x))



        dfEvent=pd.read_excel(pEventPath)

        self.dfCoreFeatures=dfTransRate.join(dfCalendar,on=["DATE"],how='left')
            #.join(dfEvent,on=['DATE'],how='left')




       # dfJobRunData=pd.read_csv(pJobRunDataPath)

    def AtomPrediction(self,JobName,BeginDate,SplitDate,EndDate):
        print('hello,world')

    # set weight for specific days, 8 times for last month, 4 times for last 2nd month and same month of last year




    def last_event(self,jobname,split_date):

        def match_scope(jobname, scope):
            if scope == 'ALL':
                return True
            elif (scope == jobname):
                return True
            else:#for more sophisticated usage, use 正则表达式
                return False

        #find the latest event before splitdate, jobname is in the scope of 'SCOPE'
        df=self.dfEvent[(self.dfEvent['DATE']<=split_date)].sort_values(by=['DATE'],axis=0,ascending=False)
        for i in range(len(df)):
            if match_scope(jobname,df[i]['SCOPE']):
                return df.irow[i]
        return pd.DataFrame()  #np.nan  #or null df





    def split_train_test_with_weight(self,widetable, begin_date, split_date, end_date):

        widetable_frq = widetable.copy()
        l_date = pd.to_datetime(split_date)
        l_1month_ago = (l_date + relativedelta(months=-1)).strftime("%Y-%m-%d")
        l_2month_ago = (l_date + relativedelta(months=-2)).strftime("%Y-%m-%d")
        l_3month_ago = (l_date + relativedelta(months=-3)).strftime("%Y-%m-%d")
        l_11month_ago = (l_date + relativedelta(months=-11)).strftime("%Y-%m-%d")
        l_12month_ago = (l_date + relativedelta(months=-12)).strftime("%Y-%m-%d")
        l_1month_later = (l_date + relativedelta(months=+1)).strftime("%Y-%m-%d")

        l_delta=widetable['RUNTIME'].mean/1000 #use l_delta to make duplicated runtime a little different so avoid over-fit to specific value

        for i in range(16):
            df = widetable[(widetable['DATE'] >= l_1month_ago) & (widetable['DATE'] < split_date)]
            df.loc['RUNTIME'] = df['RUNTIME'].apply(lambda x: x + np.random.random()*l_delta)
            widetable_frq = widetable_frq.append(df, ignore_index=True)
        for i in range(4):
            df = widetable[(widetable['DATE'] >= l_2month_ago) & (widetable['DATE'] < l_1month_ago)]
            df.loc['RUNTIME'] = df['RUNTIME'].apply(lambda x: x + np.random.random()*l_delta)
            widetable_frq = widetable_frq.append(df, ignore_index=True)
        for i in range(8):
            df = widetable[(widetable['DATE'] >= l_3month_ago) & (widetable['DATE'] < l_2month_ago)]
            df.loc['RUNTIME'] = df['RUNTIME'].apply(lambda x: x + np.random.random()*l_delta)
            widetable_frq = widetable_frq.append(df, ignore_index=True)
        for i in range(4):
            df = widetable[(widetable['DATE'] >= l_12month_ago) & (widetable['DATE'] < l_11month_ago)]
            df.loc['RUNTIME'] = df['RUNTIME'].apply(lambda x: x + np.random.random()*l_delta)
            widetable_frq = widetable_frq.append(df, ignore_index=True)



        l_wt_train = widetable_frq[
            (widetable_frq['DATE'] < split_date) & (widetable_frq['DATE'] >= '2016-01-01')]
        l_wt_test = widetable_frq[
            (widetable_frq['DATE'] < l_1month_later) & (widetable_frq['DATE'] >= split_date)]
        return l_wt_train, l_wt_test


pDataPath=r'/Users/zhuoling/Documents/ML/ICBC/BatchJob/201712/'
pJobListPath=pDataPath+r'job_list.csv'
pJobRunDataPath=pDataPath+r'job_run_data.csv'
pCalendarPath=pDataPath+r'calendar.xlsx'
pTransRatePath=pDataPath+r'trans_rate.xlsx'
pEventPath=pDataPath+r'event.xlsx'

bj = BatchJob(pJobListPath,pJobRunDataPath,pCalendarPath,pTransRatePath,pEventPath)
bj.GenCoreFeatures()
bj.AtomPrediction(jobname_mbj,'2000-01-01','2017-07-15','2017-08-07')