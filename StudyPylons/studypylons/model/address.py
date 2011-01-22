"""Address model"""
from sqlalchemy import Column, ForeignKey
from sqlalchemy.types import Integer, String
from sqlalchemy.orm import relation, backref

from studypylons.model.meta import Base

class Address(Base):
    "This is a one-to-many relationship on person.addresses"
    __tablename__ = "address"

    id = Column(Integer, primary_key=True)
    address = Column(String(100))
    city = Column(String(100))
    state = Column(String(2))
    person_id = Column(Integer, ForeignKey('person.id'))

    person = relation('Person', backref=backref('addresses', order_by=id))

    def __repr__(self):
        return "<Address('%s')" % self.address

