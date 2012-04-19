#!/usr/bin/python
#
# Copyright 2011 Google Inc. All Rights Reserved.
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

"""This example gets a specific account for the logged in user.

This includes the full tree of sub-accounts.

Tags: accounts.get
"""

__author__ = 'sergio.gomes@google.com (Sergio Gomes)'

import sys
import gflags
from oauth2client.client import AccessTokenRefreshError
import sample_utils

# Declare command-line flags, and set them as required.
gflags.DEFINE_string('account_id', None,
                     'The ID of the account to use as the root of the tree',
                     short_name='a')
gflags.MarkFlagAsRequired('account_id')


def main(argv):
  # Process flags and read their values.
  sample_utils.process_flags(argv)
  account_id = gflags.FLAGS.account_id

  # Authenticate and construct service.
  service = sample_utils.initialize_service()

  try:
    # Retrieve account.
    request = service.accounts().get(accountId=account_id, tree=True)
    account = request.execute()

    if account:
      display_tree(account)

  except AccessTokenRefreshError:
    print ('The credentials have been revoked or expired, please re-run the '
           'application to re-authorize')


def display_tree(account, level=0):
  print (' ' * level * 2 +
         'Account with ID "%s" and name "%s" was found. ' %
             (account['id'], account['name']))

  if 'subAccounts' in account:
    for sub_account in account['subAccounts']:
      display_tree(sub_account, level + 1)

if __name__ == '__main__':
  main(sys.argv)
