import random

from tasks import add

x = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
y = random.choice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

add.delay(x, y)
