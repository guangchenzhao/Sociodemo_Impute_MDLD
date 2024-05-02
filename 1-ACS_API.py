# -*- coding: utf-8 -*-
"""
"""

import os
import urllib
import zipfile
import pandas as pd
from io import StringIO

os.chdir('')

# Global Variables
state_web = pd.read_csv('InputFiles/web_state.csv')
state_web['state_code'] = state_web['state_code'].astype(str).str.zfill(2)

def ImportACSData(year='2021',state='Maryland',level='Block Group'):
    
    web_state = state_web.loc[state_web['state']==state,'web_state'].iloc[0]
    
    # Import Column Name
    print('Preparing Summary File:')
    #s_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/data/%s_5yr_Summary_FileTemplates.zip" % (year,year)
    s_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/sequence-based-SF/data/%s_5yr_Summary_FileTemplates.zip" % (year,year) #2021
    s_fh, s_ = urllib.request.urlretrieve(s_url)
    s_zip = zipfile.ZipFile(s_fh, 'r')
    
    # Import GEOID-LOGRECNO relationsip
    print('Preparing GEOID-Logical Record Number Relationship File:')
    #g_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/documentation/geography/5yr_year_geo/%s.xlsx" % (year,web_state)
    g_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/sequence-based-SF/documentation/geography/5yr_year_geo/%s.xlsx" % (year,web_state)#2021
    g_fh, g_ = urllib.request.urlretrieve(g_url)    
    g_file = pd.read_excel(g_fh,dtype=object)
    g_file = g_file.set_index('Logical Record Number')[['Geography ID', 'Geography Name']]
    
    # Import File
    if (level == 'Block Group') or (level == 'Census Tract'):
        print('Preparing Census Tract and Block Group File:')
        #c_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/data/5_year_by_state/%s_Tracts_Block_Groups_Only.zip" % (year,state)
        c_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/sequence-based-SF/data/5_year_by_state/%s_Tracts_Block_Groups_Only.zip" % (year,state)#2021
        c_fh, c_ = urllib.request.urlretrieve(c_url)
        c_zip = zipfile.ZipFile(c_fh, 'r')
        path = os.getcwd()+'\ACS_%s_%s\%s_%s' % (year,level,year,state)
    else:
        print('Preparing County File:')
        #c_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/data/5_year_by_state/%s_All_Geographies_Not_Tracts_Block_Groups.zip" % (year,state)
        c_url = "https://www2.census.gov/programs-surveys/acs/summary_file/%s/sequence-based-SF/data/5_year_by_state/%s_All_Geographies_Not_Tracts_Block_Groups.zip" % (
                year,state)#2021
        c_fh, c_ = urllib.request.urlretrieve(c_url)
        c_zip = zipfile.ZipFile(c_fh, 'r')
        path = os.getcwd()+'\ACS_%s_%s\%s_%s_County' % (year,level,year,state)
        
    try:
        os.makedirs(path,mode = 0o666)
    except OSError:
        print ("Creation of the directory %s failed" % path)
    else:
        print ("Successfully Created the Directory %s " % path)
    
    print('Start Formatting the Data:')
    for i in range(147):
        s_f = pd.read_excel(s_zip.open('seq%s.xlsx' % str(i+1)))
        table_cols = s_f.columns.tolist()
        name_cols = s_f.iloc[0].tolist()
        name = name_cols[6].split('%')[0]
        try:
            data = pd.read_csv(StringIO(str(c_zip.open('e%s5%s0%s000.txt'% (year,web_state,str(i+1).zfill(3))).read(),'utf-8'))
                               ,names=table_cols,dtype=object)
            data = pd.merge(data,g_file,left_on='LOGRECNO',right_index=True)
            data.to_csv(path+'\%s_%s.csv' % (str(i+1),name),index=False)
        except:
            print('Duplicate column names in %s.' % name)
    print('Finished!')
    
    return

for i in range(0,51):
    if __name__ == "__main__":
        year = '2021'
        state = state_web.iloc[i,0]
        print(state)
        # County / Block Group / Census Tract
        level = 'Block Group'
        ImportACSData(year,state,level)
        
