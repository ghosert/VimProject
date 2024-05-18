# coding: utf-8

"""
Start me:
cd ../
$ celery worker -A proj -l info
"""

from __future__ import absolute_import

from proj.celery import celery


@celery.task
def add(x, y):
    return x + y


@celery.task
def mul(x, y):
    return x * y


@celery.task
def xsum(numbers):
    return sum(numbers)
