import pandas as pd
import numpy as np

import datetime as dt
from dateutil.relativedelta import relativedelta

pd.options.mode.chained_assignment = None  # default='warn'


pd.options.mode.chained_assignment = None  # default='warn'

# %matplotlib inline


#import xgboost

from sklearn.linear_model import LinearRegression,LassoCV,RidgeCV
from sklearn.decomposition import SparsePCA,PCA
from sklearn.svm import SVR
from sklearn.preprocessing import MinMaxScaler

import matplotlib.pyplot as plt
#%matplotlib inline

import datetime



class BatchJob:
    def __init__(self,pAllDataPath):
        self.JOBNAME_MBJ = "MASTERBATCH"
        self.AllDataPath=pAllDataPath

#        self.dfCoreFeatures=pd.DataFrame()
        self.dfEvent = pd.read_excel(self.AllDataPath, sheetname='event')
        self.dfJobListData = pd.read_excel(self.AllDataPath, sheetname='job_list', usecols=['JOBNAME','M1BEGINDATE','M1LASTDATE'])

        self.JobName = self.JOBNAME_MBJ
        self.TargetDate=datetime.date.today()

        self.GenCoreFeatures()
        self.GenJobNameFeatures()




    def GenCoreFeatures(self):
        dfTransRate = pd.read_excel(self.AllDataPath,sheetname='trans_rate')

        dfTransRate['DATE'] = dfTransRate['DATE'].apply(lambda x: x+ datetime.timedelta(days=1))
        dfTransRate = dfTransRate.fillna(value=0)
        dfTransRate['All'] = dfTransRate.iloc[:,1:].sum(axis=1)

        def set_batch_type(x):
            if x == 'd01':
                return '1'
            else:
                if (x == 'd02') or (x == 'd06') or (x == 'd12') or (x == 'd21') or (x == 'd26'):
                    return '2'
            return '0'

        dfCalendar = pd.read_excel(self.AllDataPath,sheetname='calendar')
        dfCalendar['batch_type'] = dfCalendar['DAY'].apply(lambda x: set_batch_type(x))


        self.dfCoreFeatures = dfTransRate.merge(dfCalendar,on=["DATE"],how='left')
           # .merge(dfEvent,on=['DATE'],how='left')
        #return dfCoreFeatures

    def GenJobNameFeatures(self):
        dfJobRunData = pd.read_excel(self.AllDataPath,sheetname='job_run_data',usecols=['JOBNAME','START_DATE','NA1'])##NA1 is RUNTIME
        dfJobRunData.rename(columns={'START_DATE': 'DATE', 'NA1': 'RUNTIME'}, inplace=True)

        dfJobNameRunData = dfJobRunData[dfJobRunData['JOBNAME'] == self.JobName]
        dfJobNameRunData['IsRun'] = np.ones(len(dfJobNameRunData), dtype=int)

        dfMasterBatch = pd.read_excel(self.AllDataPath,sheetname='master_batch')
        dfMasterBatch.rename(columns={'START_DATE': 'DATE'}, inplace=True)
        dfMasterBatch['IsRun'] = np.ones(len(dfMasterBatch), dtype=int)

        dfJobCriticalPath = pd.read_excel(self.AllDataPath,sheetname='job_criticalpath',usecols=['DATE','JOBNAME'])
        JobNameCriticalPath = dfJobCriticalPath[dfJobCriticalPath['JOBNAME'] == self.JobName]
        JobNameCriticalPath['IsCriticalPath'] = np.ones(len(JobNameCriticalPath), dtype=int)

        if self.JobName=='MASTERBATCH':
            self.dfJobNameFeature = self.dfCoreFeatures.merge(dfMasterBatch,on='DATE',how='left')\
                .merge(JobNameCriticalPath[['DATE','IsCriticalPath']],on='DATE',how='left')

        else:
            self.dfJobNameFeature = self.dfCoreFeatures.merge(dfJobNameRunData['DATE', 'RUNTIME', 'IsRun'], on='DATE',how='left')\
                .merge(JobNameCriticalPath[['DATE', 'IsCriticalPath']], on='DATE', how='left')

        self.dfJobNameFeature = self.dfJobNameFeature.fillna(value=0)


    def LastEvent(self,jobname,split_date):

        def match_scope(jobname, scope):
            if scope == 'ALL':
                return True
            elif (scope == jobname):
                return True
            else:#for more sophisticated usage, use 正则表达式
                return False

        #find the latest event before splitdate, jobname is in the scope of 'SCOPE'
        df=self.dfEvent[(self.dfEvent['DATE']<=split_date)&(self.dfEvent['IMPACT']>0)].sort_values(by=['DATE'],axis=0,ascending=False)
        for i in range(len(df)):
            if match_scope(jobname,df.at[i,'SCOPE']):
                df=df.iloc[i:i+1]
                df=df.reset_index(drop=True)
                return df
        return pd.DataFrame()  #np.nan  #or null df





    def SplitTrainTestWithWeight(self,widetable, begin_date, split_date, end_date):

        widetable_frq = widetable.copy()
        l_date = pd.to_datetime(split_date)
        l_1month_ago = (l_date + relativedelta(months=-1)).strftime("%Y-%m-%d")
        l_2month_ago = (l_date + relativedelta(months=-2)).strftime("%Y-%m-%d")
        l_3month_ago = (l_date + relativedelta(months=-3)).strftime("%Y-%m-%d")
        l_11month_ago = (l_date + relativedelta(months=-11)).strftime("%Y-%m-%d")
        l_12month_ago = (l_date + relativedelta(months=-12)).strftime("%Y-%m-%d")
        l_1month_later = (l_date + relativedelta(months=+1)).strftime("%Y-%m-%d")

        l_delta=widetable['RUNTIME'].mean()/100 #use l_delta to make duplicated runtime a little different so avoid over-fit to specific value

        for i in range(16):
            df = widetable[(widetable['DATE'] >= l_1month_ago) & (widetable['DATE'] <= split_date)]
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

        l_times=0
        l_event=self.LastEvent(self.JobName,split_date)
        l_job=self.dfJobListData[self.dfJobListData['JOBNAME']==self.JobName]
        l_begindate=l_job.at[0,'M1BEGINDATE']
        l_begindate=max(l_begindate,pd.to_datetime(begin_date))
        if len(l_event)>0:
            l_event_date=l_event.at[0,'DATE']
            l_event_impact=pd.to_numeric(l_event.at[0,'IMPACT'])
            if l_event_impact==100:
                l_begindate=max(l_begindate,l_event_date)
            else: # between 0 and 100
                l_times=l_event_impact/(100-l_event_impact)
                l_before_len=len(widetable_frq[(widetable_frq['DATE'] >= l_begindate) & (widetable_frq['DATE'] < l_event_date)])
                l_after_len=len(widetable_frq[(widetable_frq['DATE'] >= l_event_date) & (widetable_frq['DATE'] <= split_date)])
                l_needadd=int(l_before_len*l_times-l_after_len)
                l_times=int(np.ceil(l_before_len*l_times/l_after_len)-1)

        widetable_frq=widetable_frq[(widetable_frq['DATE'] >= l_begindate) & (widetable_frq['DATE'] <=end_date )]

        if l_times>0:
            df = widetable_frq[(widetable_frq['DATE'] >= l_event_date) & (widetable_frq['DATE'] <= split_date)]
            widetable_add=pd.DataFrame()
            for i in range(l_times):
                df.loc['RUNTIME'] = df['RUNTIME'].apply(lambda x: x + np.random.random() * l_delta)
                widetable_add = widetable_add.append(df, ignore_index=True)
            widetable_frq.append(widetable_add.sample(l_needadd))


        l_wt_train = widetable_frq[
            (widetable_frq['DATE'] <= split_date) & (widetable_frq['DATE'] >= l_begindate)]
        l_wt_test = widetable_frq[
            (widetable_frq['DATE'] <= end_date) & (widetable_frq['DATE'] > split_date)]
        return l_wt_train, l_wt_test



    def AtomPrediction(self,JobName,BeginDate,SplitDate,EndDate):
        print(self.dfJobNameFeature.head(5))
        #TrainDate = np.range(SplitDate-BeginDate).days


