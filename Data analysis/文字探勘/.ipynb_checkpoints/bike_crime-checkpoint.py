#!/usr/bin/env python
# coding: utf-8

# In[ ]:


def GetCrimeData(filename):
    data =[]
    with open(filename, encoding='big5') as f:
        f.readline()
        for line in f:
            data.append(line.split(','))
    return data

def GetYear(record):
    return(int(record[2][:3]))

def GetMonth(record):
    return(int(record[2][3:5]))

def GetTime(record):
    start = int(record[3][:2])
    end = int(record[3][3:])
    return(start, end)
    
def GetYearCount(data, year):
    count = 0
    for record in data:
        if GetYear(record) == year:
            count+=1
    return(count)

def GetMonthCount(data, month):
    count = 0 
    for record in data:
        if GetMonth(record)==month:
            count+=1
    return count

def GetTimeCount(data, time):
    count=0
    for record in data:
        start, end = GetTime(record)
        if start<= time < end:
            count+=1
    return count

def GetPlace(record):
    return record[4][3:6]

def StatisticGetPlace(data):
    result = []
    for record in data:
        crimePlace = GetPlace(record)
        
        found = False
        
        for case in result:
            if crimePlace == case[0]:     
                case[1]+=1
                found = True
        
        if not found:
            result.append([crimePlace,1])
            
    return result
    
