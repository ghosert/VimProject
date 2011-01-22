"""The application's model objects"""
from studypylons.model.meta import Session, Base

# Add model/person.py, model/address.py first, then import its class Person, Address here.
from studypylons.model.person import Person
from studypylons.model.address import Address

def init_model(engine):
    """Call me before using any of the tables or classes in the model"""
    Session.configure(bind=engine)
