# api.ipstack.com，可以把ip address轉成地址
# 現在不是隨便存取了，要先去取得 access_key
# 使用上很簡單，就是把你要查的ip放進api中，並
# 加上access_key，用urlopen()打開，並編釋成
# utf-8。再用json.loads()讀成json檔。
# 因為json檔是python的字典，故可以用 .get(key)
# 來取得值。或是直接使用key也可以取得值。


def getCountry(ipAddress):
    import json
    from urllib.request import urlopen
    response = urlopen('http://api.ipstack.com/'+ipAddress+'?access_key=YOURCODE').read().decode('utf-8')
    responseJson = json.loads(response)
    return responseJson
    # return responseJson.get('country_code'), responseJson.get('city'), responseJson.get('zip')

# print(getCountry('140.123.41.235').get('country_code'), getCountry('140.123.41.235').get('city'), getCountry('140.123.41.235').get('zip'))



# wikipedia API

'''
Wiki API說明：
1. 下面的url是中文wikipedia的api：使用者可以填入的訊息是在
   srsearch=incategory:之後。我們用字串取代的方式來允許使
   用者輸入；在這裏，已註明輸出的格式是json。
2. 用 requests.get()來取得網頁，並用 .json()取得json格式
3. nest其是是一個長度為2的字典，關鍵字：query的值是另一個字典
   裏面存有許多資訊，包括title(查詢的資料細節)及pageid (該資
   料所在的wiki網頁。
4. https:/zh.wikipedia.org/?curid= 後加 pageid可以得到
   該資料的wiki頁面。
'''


def getWikiInfo(term):
    '''使用者提供一個關鍵字，使用wikipedia查詢
       回傳一個表列，表列中是有序對，儲存查詢結果
       細節及其相對之wiki網頁
    '''
    import requests

    url = "https://zh.wikipedia.org/w/api.php?action=query&list=search&format=json&srsearch=incategory:%s&srlimit=500&srprop=size" %term
    res = requests.get(url)
    nest = res.json()
    result = []
    # 之所有知道下面要用query及search兩個關鍵字
    # 是去看nest的結果得知的。用len()去看看nest
    # 中的結果長度，可以知道每層有幾個成員...
    for search in nest['query']['search']:
       # yield search.get('title'), search.get('pageid')
       result.append((search.get('title'), search.get('pageid')))
    return result

## 簡單的facebook API


def fb_demo():
    '''示範取得按讚的社團'''
    import facebook 

    token = "YOURTOKEN"
    graph = facebook.GraphAPI(access_token=token, version='3.1')
    mylikes = graph.get_connections(id='me', connection_name='likes')
    likes = mylikes['data']
    for like in likes:
        print(like['name'])

def fb_demo2():
    '''列出臉書的朋友數'''
    import facebook

    token = "YOURTOKEN"
    graph = facebook.GraphAPI(access_token=token, version='3.1')
    friends = graph.get_connections(id='me', connection_name="friends")
    print('朋友總數：', friends['summary']['total_count'])

