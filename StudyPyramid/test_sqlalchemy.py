from sqlalchemy import create_engine
engine = create_engine('sqlite:///:memory:', echo=True)
from sqlalchemy.ext.declarative import declarative_base

from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey

Base = declarative_base()
class User(Base):
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    fullname = Column(String)
    password = Column(String)

    def __init__(self, name, fullname, password):
        self.name = name
        self.fullname = fullname
        self.password = password

    def __repr__(self):
       return "<User('%s','%s', '%s')>" % (self.name, self.fullname, self.password)

# users_table = User.__table__
metadata = Base.metadata
metadata.create_all(engine)

from sqlalchemy.orm import sessionmaker
from sqlalchemy.orm import relationship, backref
Session = sessionmaker(bind=engine)

session = Session()

ed_user = User('ed', 'Ed Jones', 'edspassword')
session.add(ed_user)

session.add_all([User('wendy', 'Wendy Williams', 'foobar'), User('mary', 'Mary Contrary', 'xxg527'), User('fred', 'Fred Flinstone', 'blah')])

print session.query(User).all()

from sqlalchemy import Text

# association table
post_keywords = Table('post_keywords', metadata,
    Column('post_id', Integer, ForeignKey('posts.id')),
    Column('keyword_id', Integer, ForeignKey('keywords.id'))
)

class BlogPost(Base):
    __tablename__ = 'posts'

    id = Column(Integer, primary_key=True)
    user_id = Column(Integer, ForeignKey('users.id'))
    headline = Column(String(255), nullable=False)
    body = Column(Text)

    # many to many BlogPost<->Keyword
    keywords = relationship('Keyword', secondary=post_keywords, backref='posts')

    def __init__(self, headline, body, author):
        self.author = author
        self.headline = headline
        self.body = body

    def __repr__(self):
        return "BlogPost(%r, %r, %r)" % (self.headline, self.body, self.author)

class Keyword(Base):
    __tablename__ = 'keywords'

    id = Column(Integer, primary_key=True)
    keyword = Column(String(50), nullable=False, unique=True)

    def __init__(self, keyword):
        self.keyword = keyword

# "dynamic" loading relationship to User
BlogPost.author = relationship(User, backref=backref('posts', lazy='dynamic'))

metadata.create_all(engine) 

wendy = session.query(User).filter_by(name='wendy').one() 
post = BlogPost("Wendy's Blog Post", "This is a test", wendy)
session.add(post)
post.keywords.append(Keyword('wendy'))
post.keywords.append(Keyword('firstpost'))
session.query(BlogPost).filter(BlogPost.keywords.any(keyword='firstpost')).all()
blog = session.query(BlogPost).filter(BlogPost.author==wendy).filter(BlogPost.keywords.any(keyword='firstpost')).all() 
print blog[0].keywords[0].posts

wendy = session.query(User).filter_by(name='wendy').one() 
print wendy.posts[0].headline





