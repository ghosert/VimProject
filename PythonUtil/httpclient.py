import httplib, urllib, socket, types

class HttpClient:
    "This is a self-made http client to handle 301, 302 redirect and cookie automatically, and wrap the httplib to simplify programming."
    "For now this class is not thread safe, please instance this class for every thread."
    def __init__(self, host, ishttps = False, debuglevel = 0, timeout = socket._GLOBAL_DEFAULT_TIMEOUT):
        if ishttps:
            self.conn = httplib.HTTPSConnection(host, timeout = timeout) # https here is using the default port 443
        else:
            self.conn = httplib.HTTPConnection(host, timeout = timeout) # https here is using the default port 80
        self.conn.set_debuglevel(debuglevel)
        self.cookie = None

    def request(self, method, url, body=None, headers={}):
        "This method will force to read data from response and return the form of (response, data), cookie will be managed automatically."
        if self.cookie != None:
            headers['Cookie'] = self.cookie
        self.conn.request(method, url, body, headers)
        response = self.conn.getresponse()
        # return None if no cookie.
        if response.getheader('Set-Cookie'):
            self.cookie = response.getheader('Set-Cookie')
        data = response.read() # Note that you must have read the whole response before you can send a new request to the server, so force to read here.
        return (response, data)

    def __check_redirect(self, response, data):
        if response.status in (httplib.MOVED_PERMANENTLY, httplib.FOUND):
            location = response.getheader('location')
            response, data = self.request('GET', location)
            response, data = self.__check_redirect(response, data)
        return (response, data)

    def __convert_params(self, params):
        if params:
            if type(params) == types.DictType:
                body = urllib.urlencode(params)
            else:
                body = params
        else:
            body = None
        return body

    def post_request(self, url, params = None, headers = {"Content-type": "application/x-www-form-urlencoded"}, enable_redirect = True):
        "params here could be a dictionary type of encoded string type."
        body = self.__convert_params(params)
        response, data = self.request('POST', url, body, headers) # 'POST'/'GET' here should be upper case.
        if enable_redirect == False:
            return (response, data)
        return self.__check_redirect(response, data)

    def get_request(self, url, params = None, headers = {}, enable_redirect = True):
        "params here could be a dictionary type of encoded string type."
        body = self.__convert_params(params)
        response, data = self.request('GET', url, body, headers) # 'POST'/'GET' here should be upper case.
        if enable_redirect == False:
            return (response, data)
        return self.__check_redirect(response, data)

    def close(self):
        self.conn.close()

if __name__ == '__main__':
    httpclient = HttpClient('developer.emetrix.com', True, 1)
    params = {'UserName': 'ghost_e', 'Password': '13916939847273450', 'FormAction': 'True'}
    response, data = httpclient.post_request('/index.asp', params)
    print response.status, response.reason
    response, data = httpclient.get_request('https://developer.emetrix.com/storefront/send_DL.asp?ProductID=11784033')
    print response.status, response.reason
    params = {'Format': 'PRINTLINK', 'FormAction': 'Send Software'}
    response, data = httpclient.post_request('/storefront/send_DL.asp?PID=11784033&UN=ghost_e&FFN=Official_Recorder_Professional.exe&PN=Windows%20Audio%20Recorder%20Professional&FFS=5119714', params)
    print response.status, response.reason
    
    from HTMLParser import HTMLParser
    class MyHTMLParser(HTMLParser):
        def handle_starttag(self, tag, attrs):
            if tag == 'a':
                for name, value in attrs:
                    if name == 'href' and 'Official_Recorder_Professional.exe' in value:
                        print value

        def handle_endtag(self, tag):
            pass

        def handle_data(self, data):
            pass

    parser = MyHTMLParser()
    parser.feed(data)

    httpclient.close()



