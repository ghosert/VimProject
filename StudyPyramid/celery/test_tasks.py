import random

from tasks import add, mul

x = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
y = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

print "x={0}, {1}".format(x, y)

result = add.delay(x, y)
print result.id # message id

result = mul.delay(x, y)
print result.id # message id
print "mul.delay(x, y)={0}".format(result.get())

# Same as above
result = add.apply_async((2, 2))
print result.get()
print result.successful()
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
print result.failed()
print result.state # FAILURE

# Canvas/subtask
# group/map/chain/starmap/chord/chunks check out this:
# http://docs.celeryproject.org/en/latest/getting-started/next-steps.html#next-steps

