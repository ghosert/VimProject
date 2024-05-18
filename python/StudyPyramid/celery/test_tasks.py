import random

from tasks import add, mul, pdf

x = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
y = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

print "x={0}, {1}".format(x, y)

result = mul.delay(x, y)
print result.id # message id
print "mul.delay(x, y)={0}".format(result.get())
result = mul.apply_async((x, y), countdown=5)  # Same as add.delay(2, 2)
print result.id # message id
print "mul.delay(x, y)={0}".format(result.get())

result = add.apply_async((2, 2))
print result.get()
print 'result.successful(): {0}'.format(result.successful())
print result.state # SUCCESS

# Enhance above, create/send mesage to queue name 'lopri'
# add.apply_async((2, 2), queue='lopri', countdown=5)

# get propagated error if any
try:
    result = add.delay(x)
    result.get() # propagate=True by default
except Exception as e:
    print '========='
    print e
# disable propagated error if any
try:
    result = add.delay(x)
    result.get(propagate=False)
except Exception as e:
    print '---------'
    print e
print 'result.failed(): {0}'.format(result.failed())
print result.state # FAILURE

# Test on pdf
try:
    async_result = pdf.delay('This is the content of this pdf')
    async_result.get(propagate=False, timeout=30)
    if not async_result.failed():
        print 'get pdf successfully.'
        with open('/home/jiawzhang/Downloads/test-pdf.pdf', 'w') as file:
            file.write(async_result.result)
    else:
        print 'get pdf failed.'
        raise async_result.result
except Exception as e:
    print '================ catch htmlto pdf exception ================'
    print e.message


# Canvas/subtask
# group/map/chain/starmap/chord/chunks check out this:
# http://docs.celeryproject.org/en/latest/getting-started/next-steps.html#next-steps

