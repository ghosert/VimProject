shootout
========

Shootout is a demo app for the Pyramid web framework.  The concepts
demonstrated in the code include:

- Url dispatch mechanism.

- Built-in authentication and authorization mechanism.

- Usage of built-in sessioning machinery.

- Integration with pyramid_simpleform for form handling.

- SQLAlchemy based models and transaction management via pyramid_tm.

Library Requirements
--------------------

shootout requires a SQLite3 bindings.

On a Debian system, these imply: build-essentials, libsqlite3-dev.

Installing and Running
----------------------

Python 2.6 or 2.7 is required.

- virtualenv --no-site-packages env

- cd env

- . bin/activate

- git clone git@github.com:Pylons/shootout.git

- cd shootout

- python setup.py develop

- initialize_shootout_db

- pserve development.ini

