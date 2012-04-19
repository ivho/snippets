#!/usr/bin/python2.4
#
# Copyright 2010 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


"""Discovery document tests

Unit tests for objects created from discovery documents.
"""

__author__ = 'jcgregorio@google.com (Joe Gregorio)'

import base64
import httplib2
import time
import unittest
import urlparse

try:
    from urlparse import parse_qs
except ImportError:
    from cgi import parse_qs

import dev_appserver
dev_appserver.fix_sys_path()
import webapp2

from apiclient.http import HttpMockSequence
from google.appengine.api import apiproxy_stub
from google.appengine.api import apiproxy_stub_map
from google.appengine.api import app_identity
from google.appengine.api import users
from google.appengine.api.memcache import memcache_stub
from google.appengine.ext import testbed
from google.appengine.runtime import apiproxy_errors
from oauth2client.anyjson import simplejson
from oauth2client.appengine import AppAssertionCredentials
from oauth2client.appengine import OAuth2Decorator
from oauth2client.appengine import OAuth2Handler
from oauth2client.client import AccessTokenRefreshError
from oauth2client.client import FlowExchangeError
from webtest import TestApp

class UserMock(object):
  """Mock the app engine user service"""

  def user_id(self):
    return 'foo_user'


class Http2Mock(object):
  """Mock httplib2.Http"""
  status = 200
  content = {
      'access_token': 'foo_access_token',
      'refresh_token': 'foo_refresh_token',
      'expires_in': 3600,
    }

  def request(self, token_uri, method, body, headers, *args, **kwargs):
    self.body = body
    self.headers = headers
    return (self, simplejson.dumps(self.content))


class TestAppAssertionCredentials(unittest.TestCase):
  account_name = "service_account_name@appspot.com"
  signature = "signature"


  class AppIdentityStubImpl(apiproxy_stub.APIProxyStub):

    def __init__(self):
      super(TestAppAssertionCredentials.AppIdentityStubImpl, self).__init__(
          'app_identity_service')

    def _Dynamic_GetAccessToken(self, request, response):
      response.set_access_token('a_token_123')
      response.set_expiration_time(time.time() + 1800)


  class ErroringAppIdentityStubImpl(apiproxy_stub.APIProxyStub):

    def __init__(self):
      super(TestAppAssertionCredentials.ErroringAppIdentityStubImpl, self).__init__(
          'app_identity_service')

    def _Dynamic_GetAccessToken(self, request, response):
      raise app_identity.BackendDeadlineExceeded()

  def test_raise_correct_type_of_exception(self):
    app_identity_stub = self.ErroringAppIdentityStubImpl()
    apiproxy_stub_map.apiproxy = apiproxy_stub_map.APIProxyStubMap()
    apiproxy_stub_map.apiproxy.RegisterStub("app_identity_service",
                                            app_identity_stub)
    apiproxy_stub_map.apiproxy.RegisterStub(
      'memcache', memcache_stub.MemcacheServiceStub())

    scope = "http://www.googleapis.com/scope"
    try:
      credentials = AppAssertionCredentials(scope)
      http = httplib2.Http()
      credentials.refresh(http)
      self.fail('Should have raised an AccessTokenRefreshError')
    except AccessTokenRefreshError:
      pass

  def test_get_access_token_on_refresh(self):
    app_identity_stub = self.AppIdentityStubImpl()
    apiproxy_stub_map.apiproxy = apiproxy_stub_map.APIProxyStubMap()
    apiproxy_stub_map.apiproxy.RegisterStub("app_identity_service",
                                            app_identity_stub)
    apiproxy_stub_map.apiproxy.RegisterStub(
      'memcache', memcache_stub.MemcacheServiceStub())

    scope = "http://www.googleapis.com/scope"
    credentials = AppAssertionCredentials(scope)
    http = httplib2.Http()
    credentials.refresh(http)
    self.assertEqual('a_token_123', credentials.access_token)


class DecoratorTests(unittest.TestCase):

  def setUp(self):
    self.testbed = testbed.Testbed()
    self.testbed.activate()
    self.testbed.init_datastore_v3_stub()
    self.testbed.init_memcache_stub()
    self.testbed.init_user_stub()

    decorator = OAuth2Decorator(client_id='foo_client_id',
                                client_secret='foo_client_secret',
                                scope=['foo_scope', 'bar_scope'],
                                user_agent='foo')
    self.decorator = decorator

    class TestRequiredHandler(webapp2.RequestHandler):

      @decorator.oauth_required
      def get(self):
        pass

    class TestAwareHandler(webapp2.RequestHandler):

      @decorator.oauth_aware
      def get(self, *args, **kwargs):
        self.response.out.write('Hello World!')
        assert(kwargs['year'] == '2012')
        assert(kwargs['month'] == '01')


    application = webapp2.WSGIApplication([
        ('/oauth2callback', OAuth2Handler),
        ('/foo_path', TestRequiredHandler),
        webapp2.Route(r'/bar_path/<year:\d{4}>/<month:\d{2}>',
          handler=TestAwareHandler, name='bar')],
      debug=True)
    self.app = TestApp(application)
    users.get_current_user = UserMock
    self.httplib2_orig = httplib2.Http
    httplib2.Http = Http2Mock

  def tearDown(self):
    self.testbed.deactivate()
    httplib2.Http = self.httplib2_orig

  def test_required(self):
    # An initial request to an oauth_required decorated path should be a
    # redirect to start the OAuth dance.
    response = self.app.get('/foo_path')
    self.assertTrue(response.status.startswith('302'))
    q = parse_qs(response.headers['Location'].split('?', 1)[1])
    self.assertEqual('http://localhost/oauth2callback', q['redirect_uri'][0])
    self.assertEqual('foo_client_id', q['client_id'][0])
    self.assertEqual('foo_scope bar_scope', q['scope'][0])
    self.assertEqual('http://localhost/foo_path', q['state'][0])
    self.assertEqual('code', q['response_type'][0])
    self.assertEqual(False, self.decorator.has_credentials())

    # Now simulate the callback to /oauth2callback.
    response = self.app.get('/oauth2callback', {
        'code': 'foo_access_code',
        'state': 'foo_path',
        })
    self.assertEqual('http://localhost/foo_path', response.headers['Location'])
    self.assertEqual(None, self.decorator.credentials)

    # Now requesting the decorated path should work.
    response = self.app.get('/foo_path')
    self.assertEqual('200 OK', response.status)
    self.assertEqual(True, self.decorator.has_credentials())
    self.assertEqual('foo_refresh_token',
                     self.decorator.credentials.refresh_token)
    self.assertEqual('foo_access_token',
                     self.decorator.credentials.access_token)

    # Invalidate the stored Credentials.
    self.decorator.credentials.invalid = True
    self.decorator.credentials.store.put(self.decorator.credentials)

    # Invalid Credentials should start the OAuth dance again.
    response = self.app.get('/foo_path')
    self.assertTrue(response.status.startswith('302'))
    q = parse_qs(response.headers['Location'].split('?', 1)[1])
    self.assertEqual('http://localhost/oauth2callback', q['redirect_uri'][0])

  def test_storage_delete(self):
    # An initial request to an oauth_required decorated path should be a
    # redirect to start the OAuth dance.
    response = self.app.get('/foo_path')
    self.assertTrue(response.status.startswith('302'))

    # Now simulate the callback to /oauth2callback.
    response = self.app.get('/oauth2callback', {
        'code': 'foo_access_code',
        'state': 'foo_path',
        })
    self.assertEqual('http://localhost/foo_path', response.headers['Location'])
    self.assertEqual(None, self.decorator.credentials)

    # Now requesting the decorated path should work.
    response = self.app.get('/foo_path')

    # Invalidate the stored Credentials.
    self.decorator.credentials.store.delete()

    # Invalid Credentials should start the OAuth dance again.
    response = self.app.get('/foo_path')
    self.assertTrue(response.status.startswith('302'))

  def test_aware(self):
    # An initial request to an oauth_aware decorated path should not redirect.
    response = self.app.get('/bar_path/2012/01')
    self.assertEqual('Hello World!', response.body)
    self.assertEqual('200 OK', response.status)
    self.assertEqual(False, self.decorator.has_credentials())
    url = self.decorator.authorize_url()
    q = parse_qs(url.split('?', 1)[1])
    self.assertEqual('http://localhost/oauth2callback', q['redirect_uri'][0])
    self.assertEqual('foo_client_id', q['client_id'][0])
    self.assertEqual('foo_scope bar_scope', q['scope'][0])
    self.assertEqual('http://localhost/bar_path/2012/01', q['state'][0])
    self.assertEqual('code', q['response_type'][0])

    # Now simulate the callback to /oauth2callback.
    url = self.decorator.authorize_url()
    response = self.app.get('/oauth2callback', {
        'code': 'foo_access_code',
        'state': 'bar_path',
        })
    self.assertEqual('http://localhost/bar_path', response.headers['Location'])
    self.assertEqual(False, self.decorator.has_credentials())

    # Now requesting the decorated path will have credentials.
    response = self.app.get('/bar_path/2012/01')
    self.assertEqual('200 OK', response.status)
    self.assertEqual('Hello World!', response.body)
    self.assertEqual(True, self.decorator.has_credentials())
    self.assertEqual('foo_refresh_token',
                     self.decorator.credentials.refresh_token)
    self.assertEqual('foo_access_token',
                     self.decorator.credentials.access_token)


  def test_kwargs_are_passed_to_underlying_flow(self):
    decorator = OAuth2Decorator(client_id='foo_client_id',
        client_secret='foo_client_secret',
        user_agent='foo_user_agent',
        scope=['foo_scope', 'bar_scope'],
        access_type='offline',
        approval_prompt='force')
    self.assertEqual('offline', decorator.flow.params['access_type'])
    self.assertEqual('force', decorator.flow.params['approval_prompt'])
    self.assertEqual('foo_user_agent', decorator.flow.user_agent)
    self.assertEqual(None, decorator.flow.params.get('user_agent', None))


if __name__ == '__main__':
  unittest.main()
