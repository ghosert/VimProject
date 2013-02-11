import os
import sys
import transaction

# jiawzhang: Code in this file is executed whenever we run the "../bin/initialize_MyProject_db development.ini" command.
# In this file, you can add new ORM here like Page class below and populate the data like "DBSession.add(model)" below.
# After changing this file please make sure reinitializing the database with the command above.
# Running "cd ../../" "$ python setup develop" will also excute this script, since this one is specified in ../../setup.py

from sqlalchemy import engine_from_config

from pyramid.paster import (
    get_appsettings,
    setup_logging,
    )

from ..models import (
    DBSession,
    # jiawzhang: add new model.
    Page,
    Base,
    )

def usage(argv):
    cmd = os.path.basename(argv[0])
    print('usage: %s <config_uri>\n'
          '(example: "%s development.ini")' % (cmd, cmd)) 
    sys.exit(1)

def main(argv=sys.argv):
    if len(argv) != 2:
        usage(argv)
    config_uri = argv[1]
    setup_logging(config_uri)
    settings = get_appsettings(config_uri)
    engine = engine_from_config(settings, 'sqlalchemy.')
    DBSession.configure(bind=engine)
    Base.metadata.create_all(engine)
    with transaction.manager:
        # jiawzhang: add new model.
        model = Page('FrontPage', 'This is the front page')
        DBSession.add(model)
