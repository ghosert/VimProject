# -*- coding: utf-8 -*-
import unittest

from pyramid import testing


def init_db():
    from shootout.models import (
        DBSession,
        Base,
        )
    from sqlalchemy import create_engine
    engine = create_engine('sqlite://')
    DBSession.configure(bind=engine)
    Base.metadata.bind = engine
    Base.metadata.create_all(engine)
    session = DBSession()
    return session


class ModelsTestCase(unittest.TestCase):
    def setUp(self):
        self.session = init_db()

    def tearDown(self):
        import transaction
        from shootout.models import DBSession
        transaction.abort()
        DBSession.remove()
        testing.tearDown()

    def _addUser(self, username=u'username'):
        from shootout.models import User
        user = User(username=username, password=u'password', name=u'name',
                    email=u'email')
        self.session.add(user)
        self.session.flush()
        return user

    def _addIdea(self, target=None, user=None, title=u'title'):
        from shootout.models import Idea
        if not user:
            user = self._addUser()
        idea = Idea(target=target, author=user, title=title,
                    text=u'text')
        self.session.add(idea)
        self.session.flush()
        return idea


class TestUser(ModelsTestCase):
    def test_add_user(self):
        from shootout.models import User
        user = User(u'username', u'password', u'name', u'email')
        self.session.add(user)
        self.session.flush()
        user = self.session.query(User).filter(User.username == u'username')
        user = user.first()
        self.assertEqual(user.username, u'username')
        self.assertEqual(user.name, u'name')
        self.assertEqual(user.email, u'email')
        self.assertEqual(user.hits, 0)
        self.assertEqual(user.misses, 0)
        self.assertEqual(user.delivered_hits, 0)
        self.assertEqual(user.delivered_misses, 0)

    def test_doesnt_exitst(self):
        from shootout.models import User
        from sqlalchemy.orm.exc import NoResultFound
        query = self.session.query(User).filter(User.username == u'nobody')
        self.assertRaises(NoResultFound, query.one)

    def test_arleady_exist(self):
        from sqlalchemy.exc import IntegrityError
        self._addUser()
        self.assertRaises(IntegrityError, self._addUser)

    def test_password_hashing(self):
        import cryptacular.bcrypt
        crypt = cryptacular.bcrypt.BCRYPTPasswordManager()
        user = self._addUser()
        self.assertTrue(crypt.check(user.password, u'password'))

    def test_password_checking(self):
        from shootout.models import User
        self._addUser()
        self.assertTrue(User.check_password(u'username', u'password'))
        self.assertFalse(User.check_password(u'username', u'wrong'))
        self.assertFalse(User.check_password(u'nobody', u'password'))

    def test_getting_by_username(self):
        from shootout.models import User
        user = self._addUser()
        self.assertEqual(user, User.get_by_username(u'username'))


class TestTag(ModelsTestCase):
    def test_extracting_tags(self):
        from shootout.models import Tag
        tags_string = u'foo, bar; baz xxx,, yyy, zzz'
        expected_tags = set([
            u'foo', u'bar', u'baz', u'xxx', u'yyy', u'zzz'
        ])
        extracted_tags = Tag.extract_tags(tags_string)
        self.assertEqual(extracted_tags, expected_tags)

    def test_creating_tags(self):
        from shootout.models import Tag
        tags = Tag.create_tags(u'foo bar baz')
        tags_names = set([u'foo', u'bar', u'baz'])
        self.assertEqual(tags[0].name, tags_names.pop())
        self.assertEqual(tags[1].name, tags_names.pop())
        self.assertEqual(tags[2].name, tags_names.pop())

    def test_tags_counts(self):
        from shootout.models import Tag

        user = self._addUser()

        idea1 = self._addIdea(user=user)
        idea1.tags = Tag.create_tags(u'foo bar baz')
        self.session.add(idea1)
        idea2 = self._addIdea(user=user)
        idea2.tags = Tag.create_tags(u'baz zzz aaa')
        self.session.add(idea2)
        idea2 = self._addIdea(user=user)
        idea2.tags = Tag.create_tags(u'foo baz')
        self.session.add(idea2)
        self.session.flush()

        tags_counts = Tag.tag_counts()
        expected_counts = [
            ('aaa', 1),
            ('bar', 1),
            ('baz', 3),
            ('foo', 2),
            ('zzz', 1),
        ]
        self.assertEqual(list(tags_counts), expected_counts)


class TestIdea(ModelsTestCase):

    def _getIdea(self, idea_id):
        from shootout.models import Idea
        query = self.session.query(Idea).filter(Idea.idea_id == idea_id)
        return query.first()

    def test_add_idea(self):
        from shootout.models import Idea
        user = self._addUser()
        idea = Idea(
            author=user,
            title=u'Foo',
            text=u'Lorem ipsum dolor sit amet',
        )
        self.session.flush()

        idea = self.session.query(Idea).filter(Idea.title == u'Foo')
        idea = idea.first()

        self.assertEqual(idea.comments, [])
        self.assertEqual(idea.author.user_id, user.user_id)
        self.assertEqual(idea.author.username, u'username')
        self.assertEqual(idea.title, u'Foo')
        self.assertEqual(idea.text, u'Lorem ipsum dolor sit amet')
        self.assertEqual(idea.hits, 0)
        self.assertEqual(idea.misses, 0)
        self.assertEqual(idea.tags, [])
        self.assertEqual(idea.voted_users.all(), [])
        self.assertEqual(idea.hit_percentage, 0)
        self.assertEqual(idea.total_votes, 0)
        self.assertEqual(idea.vote_differential, 0)

    def test_doesnt_exist(self):
        from shootout.models import Idea
        from sqlalchemy.orm.exc import NoResultFound
        query = self.session.query(Idea).filter(Idea.title == u'Bar')
        self.assertRaises(NoResultFound, query.one)

    def test_add_comments(self):
        user = self._addUser()
        idea = self._addIdea(user=user)
        comment1 = self._addIdea(user=user, target=idea)
        comment2 = self._addIdea(user=user, target=idea)

        self.assertEqual(idea.comments, [comment1, comment2])

    # @unittest.skip("no idea how to force floats instead of ints here")
    # def test_hit_percentage(self):
    #     idea = self._addIdea()
    #     idea.hits = 3
    #     idea.misses = 7
    #     self.session.flush()
    #     idea = self._getIdea(idea.idea_id)
    #     self.assertEqual(idea.hit_percentage, 30)
    #     idea.hits = 13
    #     self.session.flush()
    #     idea = self._getIdea(idea.idea_id)
    #     self.assertEqual(idea.hit_percentage, 65)

    def test_total_votes(self):
        idea = self._addIdea()
        idea.hits = 5
        idea.misses = 12
        self.session.flush()
        idea = self._getIdea(idea.idea_id)
        self.assertEqual(idea.total_votes, 17)

    def test_vote_differential(self):
        idea = self._addIdea()
        idea.hits = 3
        idea.misses = 8
        self.session.flush()
        idea = self._getIdea(idea.idea_id)
        self.assertEqual(idea.vote_differential, -5)

    def test_get_by_id(self):
        from shootout.models import Idea
        idea = self._addIdea()
        queried_idea = Idea.get_by_id(idea.idea_id)
        self.assertEqual(idea, queried_idea)

    def test_ideas_bunch(self):
        from shootout.models import Idea
        user = self._addUser()
        idea1 = self._addIdea(user=user)
        idea2 = self._addIdea(user=user, title=u'title3')
        idea3 = self._addIdea(user=user, title=u'title4')
        idea4 = self._addIdea(user=user, title=u'title2')

        self.assertEqual(Idea.ideas_bunch(Idea.idea_id),
                         [idea1, idea2, idea3, idea4])
        self.assertEqual(Idea.ideas_bunch(Idea.idea_id, 2), [idea1, idea2])
        self.assertEqual(Idea.ideas_bunch(Idea.title),
                         [idea1, idea4, idea2, idea3])

    def test_user_voted(self):
        idea = self._addIdea()
        voting_user = self._addUser(u'voter')
        idea.voted_users.append(voting_user)
        self.session.flush()
        self.assertTrue(idea.user_voted(u'voter'))
        self.assertFalse(idea.user_voted(u'xxx'))

