# jiawzhang:
# In a production system, user and group data will most often come from a database, but here we use “dummy” data to represent user and groups sources

USERS = {'editor':'editor',
          'viewer':'viewer'}
GROUPS = {'editor':['group:editors']}

def groupfinder(userid, request):
    if userid in USERS:
        return GROUPS.get(userid, [])

