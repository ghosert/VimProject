from distutils.core import setup
import py2exe
import zipfile, os, shutil

# remove dist, build folder
shutil.rmtree('dist', ignore_errors = True)
shutil.rmtree('build', ignore_errors = True)

# start setup which will create dist and build folder.
setup(console=['wintwill.py'])


# add wintwill.py to library, zip library
zip = zipfile.ZipFile('library.zip', 'w', zipfile.ZIP_DEFLATED)
zip.write('wintwill.py')
os.chdir('library')
for root, dirs, files in os.walk('.'):
    for file in files:
       zip.write(root + os.path.sep + file)
os.chdir('..')
zip.close()

# remove dist/library.zip, copy new library.zip to dist
os.remove('dist/library.zip')
shutil.move('library.zip', 'dist')

# copy the files under vclib to dist
os.chdir('vclib')
for root, dirs, files in os.walk('.'):
    for file in files:
        shutil.copyfile(root + os.path.sep + file, '../dist' + os.path.sep + file)
os.chdir('..')

# remove wintwill/dist and copy new dist to wintwill/dist
shutil.rmtree('wintwill/dist', ignore_errors = True)
shutil.copytree('dist', 'wintwill/dist')

# remove dist, build folder
shutil.rmtree('dist', ignore_errors = True)
shutil.rmtree('build', ignore_errors = True)

