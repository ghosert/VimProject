import cryptacular.bcrypt

from sqlalchemy import (
    Table,
    Column,
    ForeignKey,
    )

from sqlalchemy.orm import (
    scoped_session,
    sessionmaker,
    relation,
    backref,
    column_property,
    synonym,
    joinedload,
    )

from sqlalchemy.types import (
    Integer,
    Unicode,
    UnicodeText,
    )

from sqlalchemy.sql import func
from sqlalchemy.ext.declarative import declarative_base

from zope.sqlalchemy import ZopeTransactionExtension

from pyramid.security import (
    Everyone,
    Authenticated,
    Allow,
    )

DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension()))
Base = declarative_base()

crypt = cryptacular.bcrypt.BCRYPTPasswordManager()

def hash_password(password):
    return unicode(crypt.encode(password))


class User(Base):
    """
    Application's user model.
    """
    __tablename__ = 'users'
    user_id = Column(Integer, primary_key=True)
    username = Column(Unicode(20), unique=True)
    name = Column(Unicode(50))
    email = Column(Unicode(50))
    hits = Column(Integer, default=0)
    misses = Column(Integer, default=0)
    delivered_hits = Column(Integer, default=0)
    delivered_misses = Column(Integer, default=0)

    _password = Column('password', Unicode(60))

    def _get_password(self):
        return self._password

    def _set_password(self, password):
        self._password = hash_password(password)

    password = property(_get_password, _set_password)
    password = synonym('_password', descriptor=password)

    def __init__(self, username, password, name, email):
        self.username = username
        self.name = name
        self.email = email
        self.password = password

    @classmethod
    def get_by_username(cls, username):
        return DBSession.query(cls).filter(cls.username == username).first()

    @classmethod
    def check_password(cls, username, password):
        user = cls.get_by_username(username)
        if not user:
            return False
        return crypt.check(user.password, password)


ideas_tags = Table('ideas_tags', Base.metadata,
    Column('idea_id', Integer, ForeignKey('ideas.idea_id')),
    Column('tag_id', Integer, ForeignKey('tags.tag_id'))
)


class Tag(Base):
    """
    Idea's tag model.
    """
    __tablename__ = 'tags'
    tag_id = Column(Integer, primary_key=True)
    name = Column(Unicode(50), unique=True, index=True)

    def __init__(self, name):
        self.name = name

    @staticmethod
    def extract_tags(tags_string):
        tags = tags_string.replace(';', ' ').replace(',', ' ')
        tags = [tag.lower() for tag in tags.split()]
        tags = set(tags)

        return tags

    @classmethod
    def get_by_name(cls, tag_name):
        tag = DBSession.query(cls).filter(cls.name == tag_name)
        return tag.first()

    @classmethod
    def create_tags(cls, tags_string):
        tags_list = cls.extract_tags(tags_string)
        tags = []

        for tag_name in tags_list:
            tag = cls.get_by_name(tag_name)
            if not tag:
                tag = Tag(name=tag_name)
                DBSession.add(tag)
            tags.append(tag)

        return tags

    @classmethod
    def tag_counts(cls):
        query = DBSession.query(Tag.name, func.count('*'))
        return query.join('ideas').group_by(Tag.name)

voted_users = Table('ideas_votes', Base.metadata,
    Column('idea_id', Integer, ForeignKey('ideas.idea_id')),
    Column('user_id', Integer, ForeignKey('users.user_id'))
)


class Idea(Base):
    __tablename__ = 'ideas'
    idea_id = Column(Integer, primary_key=True)
    target_id = Column(Integer, ForeignKey('ideas.idea_id'))
    comments = relation('Idea', cascade="delete",
        backref=backref('target', remote_side=idea_id))
    author_id = Column(Integer, ForeignKey('users.user_id'))
    author = relation(User, cascade="delete", backref='ideas')
    title = Column(UnicodeText)
    text = Column(UnicodeText)
    hits = Column(Integer, default=0)
    misses = Column(Integer, default=0)
    tags = relation(Tag, secondary=ideas_tags, backref='ideas')
    voted_users = relation(User, secondary=voted_users, lazy='dynamic',
        backref='voted_ideas')
    hit_percentage = func.coalesce(hits / (hits + misses) * 100, 0)

    hit_percentage = column_property(hit_percentage.label('hit_percentage'))

    total_votes = column_property((hits + misses).label('total_votes'))

    vote_differential = column_property(
        (hits - misses).label('vote_differential')
    )

    @classmethod
    def get_query(cls, with_joinedload=True):
        query = DBSession.query(cls)
        if with_joinedload:
            query = query.options(joinedload('tags'), joinedload('author'))
        return query

    @classmethod
    def get_by_id(cls, idea_id, with_joinedload=True):
        query = cls.get_query(with_joinedload)
        return query.filter(cls.idea_id == idea_id).first()

    @classmethod
    def get_by_tagname(cls, tag_name, with_joinedload=True):
        query = cls.get_query(with_joinedload)
        return query.filter(Idea.tags.any(name=tag_name))

    @classmethod
    def ideas_bunch(cls, order_by, how_many=10, with_joinedload=True):
        query = cls.get_query(with_joinedload).join('author')
        query = query.filter(cls.target == None).order_by(order_by)
        return query.limit(how_many).all()

    def user_voted(self, username):
        return bool(self.voted_users.filter_by(username=username).first())

    def vote(self, user, positive):
        if positive:
            self.hits += 1
            self.author.hits += 1
            user.delivered_hits += 1
        else:
            self.misses += 1
            self.author.misses += 1
            user.delivered_misses += 1

        self.voted_users.append(user)


class RootFactory(object):
    __acl__ = [
        (Allow, Everyone, 'view'),
        (Allow, Authenticated, 'post')
    ]

    def __init__(self, request):
        pass  # pragma: no cover

