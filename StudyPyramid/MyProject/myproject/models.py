import transaction

from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import Text

from sqlalchemy.exc import IntegrityError
from sqlalchemy.ext.declarative import declarative_base

from sqlalchemy.orm import scoped_session
from sqlalchemy.orm import sessionmaker

from zope.sqlalchemy import ZopeTransactionExtension

# jiawzhang: We specify that we'd like to use the "ZopeTransactionExtension".
# jiawzhang: This extension is an extension which allows us to use a transaction manager instead of controlling commits and aborts to database operations by hand.
DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension()))

# jiawzhang: We create a declarative Base object to use as a base class for our model.
Base = declarative_base()

# jiawzhang: Page class, It has an __init__ that takes a three arguments (id, name, and data).
# jiawzhang: The Page class also has a __tablename__ attribute. This informs SQLAlchemy which table to use to store the data representing instances of this class.
class Page(Base):
    __tablename__ = 'pages'
    id = Column(Integer, primary_key=True)
    name = Column(Text, unique=True)
    data = Column(Text)

    def __init__(self, name, data):
        self.name = name
        self.data = data

# jiawzhang: A function named populate which adds a single model instance into our SQL storage and commits a transaction.
def populate():
    session = DBSession()
    page = Page('FrontPage', 'initial data')
    session.add(page)
    transaction.commit()
    
# jiawzhang: Binds db engine to our SQLAlchemy DBSession object. It also calls the populate function, to do initial database population.
def initialize_sql(engine):
    DBSession.configure(bind=engine)
    Base.metadata.bind = engine
    Base.metadata.create_all(engine)
    try:
        populate()
    except IntegrityError:
        print 'already created'
        pass
