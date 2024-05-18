from sqlalchemy import (
    Column,
    Integer,
    Text,
    )

from sqlalchemy.ext.declarative import declarative_base

from sqlalchemy.orm import (
    scoped_session,
    sessionmaker,
    )

from zope.sqlalchemy import ZopeTransactionExtension

# jiawzhang: We specify that we'd like to use the "ZopeTransactionExtension".
# jiawzhang: This extension is an extension which allows us to use a transaction manager instead of controlling commits and aborts to database operations by hand.
# jiawzhang: This works only when you start application in web context: it commits at the end of a request or aborts if there's an exception.
# jiawzhang: This does not work in unit test or script context, that's why in the file ./tests.py ./scripts/initializedb.py you have to introduce "transaction.manager"
DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension()))

# jiawzhang: We create a declarative Base object to use as a base class for our model.
Base = declarative_base()

# jiawzhang: Page class, It has an __init__ that takes a three arguments (id, name, and data).
# jiawzhang: The Page class also has a __tablename__ attribute. This informs SQLAlchemy which table to use to store the data representing instances of this class.
class Page(Base):
    """ The SQLAlchemy declarative model class for a Page object. """
    __tablename__ = 'pages'
    id = Column(Integer, primary_key=True)
    name = Column(Text, unique=True)
    data = Column(Text)

    def __init__(self, name, data):
        self.name = name
        self.data = data

# This is dedicated for the url dispatch based app.

from pyramid.security import (
    Allow,
    Everyone,
    )
class RootFactory(object):
    __acl__ = [ (Allow, Everyone, 'view'),
                (Allow, 'group:editors', 'edit') ]
    def __init__(self, request):
        pass

