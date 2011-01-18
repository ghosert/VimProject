import xmlrpclib

from pylons import request
from pylons.controllers import XMLRPCController

states = ['Delaware', 'Pennsylvania', 'New Jersey', 'Georgia',
          'Connecticut', 'Massachusetts', 'Maryland', 'South Carolina',
          'New Hampshire', 'Virginia', 'New York', 'North Carolina',
          'Rhode Island', 'Vermont', 'Kentucky', 'Tennessee', 'Ohio',
          'Louisiana', 'Indiana', 'Mississippi', 'Illinois', 'Alabama',
          'Maine', 'Missouri', 'Arkansas', 'Michigan', 'Florida', 'Texas',
          'Iowa', 'Wisconsin', 'California', 'Minnesota', 'Oregon',
          'Kansas', 'West Virginia', 'Nevada', 'Nebraska', 'Colorado',
          'North Dakota', 'South Dakota', 'Montana', 'Washington', 'Idaho',
          'Wyoming', 'Utah', 'Oklahoma', 'New Mexico', 'Arizona', 'Alaska',
          'Hawaii']

class RpctestController(XMLRPCController):

    # For testing this service:
    # from pprint import pprint
    # import xmlrpclib
    # srvr = xmlrpclib.Server("http://example.com/rpctest/")
    # pprint(srvr.system.listMethods())
    # ['system.listMethods', 'system.methodHelp', 'system.methodSignature', 'test.battingOrder']
    # print srvr.system.methodHelp('test.battingOrder')
    # This docstring becomes the content of the returned value for system.methodHelp called with the parameter "test.battingOrder"). The method signature will be appended below ...
    # Method signature: [['string', 'int']]
    # pprint(srvr.system.methodSignature('test.battingOrder'))
    # [['string', 'int']]
    # pprint(srvr.test.battingOrder(12))
    # 'North Carolina'

    def userstatus(self):
        return 'basic string'
    userstatus.signature = [['string']]

    def userinfo(self, username, age=None):
        user = LookUpUser(username)
        result = {'username': user.name}
        if age and age > 10:
            result['age'] = age
        return result
    # Return an array of arrays.
    # First item of each array is return value, the remainder passing parameters.
    # Here are two arrays in a single array, because passed in parameter 'age' above may be None, so we should have two definition here.
    # 'struct' below is XMLRPC datatype means 'dict' in Python.
    userinfo.signature = [['struct', 'string'],
                          ['struct', 'string', 'int']]
    # XMLRPC date type with its corresponding Python data type:
    # string - str
    # array - list
    # boolean - bool
    # int - int
    # double - float
    # struct - dict
    # dateTime.iso8601 - xmlrpclib.DateTime
    # base64 - xmlrpclib.Binary

    def test_battingOrder(self, posn):
        """This docstring becomes the content of the
        returned value for system.methodHelp called with
        the parameter "test.battingOrder"). The method
        signature will be appended below ...
        """
        # XML-RPC checks agreement for arity and parameter datatype, so
        # by the time we get called, we know we have an int.
        if posn > 0 and posn < 51:
            return states[posn-1]
        else:
            # Technically, the param value is correct: it is an int.
            # Raising an error is inappropriate, so instead we
            # return a facetious message as a string.
            return 'Out of cheese error.'
    test_battingOrder.signature = [['string', 'int']]

    def xmlrpc_fault(code, message):
        """Convenience method to return a Pylons response XMLRPC Fault"""
        return
