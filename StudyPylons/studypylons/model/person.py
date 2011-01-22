"""Person model"""
from sqlalchemy import Column
from sqlalchemy.types import Integer, String

from studypylons.model.meta import Base

class Person(Base):
    __tablename__ = "person"

    id = Column(Integer, primary_key=True)
    name = Column(String(100))
    email = Column(String(100))

    def __init__(self, name='', email=''):
        self.name = name
        self.email = email

    def __repr__(self):
        return "<Person('%s')" % self.name

