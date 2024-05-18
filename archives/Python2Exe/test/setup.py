from distutils.core import setup
import py2exe
import os, shutil

# remove dist, build folder
shutil.rmtree('dist', ignore_errors = True)
shutil.rmtree('build', ignore_errors = True)

setup(console=['test.py'])

# copy the files under vclib to dist, without these files, it may be wrong in some target machine.
os.chdir('vclib')
for root, dirs, files in os.walk('.'):
    for file in files:
        shutil.copyfile(root + os.path.sep + file, '../dist' + os.path.sep + file)
os.chdir('..')

