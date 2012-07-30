import os

from setuptools import setup, find_packages

here = os.path.abspath(os.path.dirname(__file__))
README = open(os.path.join(here, 'README.txt')).read()
CHANGES = open(os.path.join(here, 'CHANGES.txt')).read()

requires = [
    'pyramid',
    'SQLAlchemy',
    'transaction',
    'pyramid_tm',
    'pyramid_debugtoolbar',
    'zope.sqlalchemy',
    'waitress',
    # jiawzhang: add a new docutils package for MyProject app.
    # After adding this new dependency, enter virtual env, re-run 'python setup.py develop' in MyProject path to download and install the dependency.
    'docutils',
    ]

setup(name='MyProject',
      version='0.0',
      description='MyProject',
      long_description=README + '\n\n' +  CHANGES,
      classifiers=[
        "Programming Language :: Python",
        "Framework :: Pylons",
        "Topic :: Internet :: WWW/HTTP",
        "Topic :: Internet :: WWW/HTTP :: WSGI :: Application",
        ],
      author='',
      author_email='',
      url='',
      keywords='web wsgi bfg pylons pyramid',
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      test_suite='myproject',
      install_requires=requires,
      entry_points="""\
      [paste.app_factory]
      main = myproject:main
      [console_scripts]
      initialize_MyProject_db = myproject.scripts.initializedb:main
      """,
      )

