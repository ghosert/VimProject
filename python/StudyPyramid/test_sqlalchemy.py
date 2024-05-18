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

# "dynamic" loading relationship to User, and notice we don't define posts/relationship() in User class.
BlogPost.author = relationship(User, backref=backref('posts', lazy='dynamic'))
# BlogPost.author = relationship(User, backref=backref('posts'))

metadata.create_all(engine) 

wendy = session.query(User).filter_by(name='wendy').one() 
post = BlogPost("Wendy's Blog Post", "This is a test", wendy)
session.add(post)
post.keywords.append(Keyword('wendy'))
post.keywords.append(Keyword('firstpost'))
session.commit()

print '============================Two SQLs produced without contains_eager============================================='
from sqlalchemy.orm import contains_eager
blogpost = session.query(BlogPost).filter(BlogPost.keywords.any(keyword='firstpost')).all()
print blogpost[0].keywords[1].keyword
session.commit()
print '============================One SQL produced wit contains_eager, it means contains_eager works for both JOIN and EXISTS sql=================================================='
blogpost = session.query(BlogPost).filter(BlogPost.keywords.any(keyword='firstpost')).options(contains_eager(BlogPost.keywords)).all()
print blogpost[0].keywords[1].keyword
print '================================================================================================================='

blog = session.query(BlogPost).filter(BlogPost.author==wendy).filter(BlogPost.keywords.any(keyword='firstpost')).all() 
print blog[0].keywords[0].posts
print '================================================================================================================='

# Even if we fail to define posts/relationship() in User class, we can still use it like below:
wendy = session.query(User).filter_by(name='wendy').one() 
# print wendy.posts[0].headline
print '================================================================================================================='
# The line below won't produce any sql because of lazy='dynamic' above, but 'print wendy.posts' will produce sql, means: unless you do need data, it will produce sql, otherwise, not.
wendy.posts
print '================================================================================================================='
# When we access User.posts, we'd like to be able to filter results further so as not to load the entire collection. For this we use a setting accepted by relationship() called lazy='dynamic', which configures an alternate loader strategy on the attribute.
# That means, if you set lazy='dynamic', wendy.posts will not trigger any sql to avoid loading a large entire collection, you are supposed to filter results further like below to fetch the partial collection conditionally. Ideally in this case if there are too many posts for the single user, you don't want to load them all but filter them conditionally, you can set lazy='dynamic' like this.
# And once you set lazy='dynamic', you can't use something like "join + options(contains_eager(User.posts))" in query to load posts all, instead, you will get an error, because they are paradoxical.
wendy.posts.filter(BlogPost.keywords.any(keyword='firstpost')).all()


