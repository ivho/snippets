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

"""This example gets all ad units corresponding to a specified custom channel.

To get custom channels, run get_all_custom_channels.py.

Tags: accounts.customchannels.adunits.list
"""

__author__ = 'sergio.gomes@google.com (Sergio Gomes)'

import sys
import gflags
from oauth2client.client import AccessTokenRefreshError
import sample_utils

MAX_PAGE_SIZE = 50

# Declare command-line flags, and set them as required.
gflags.DEFINE_string('account_id', None,
    'The ID of the account with the specified custom channel',
    short_name='a')
gflags.MarkFlagAsRequired('account_id')

gflags.DEFINE_string('ad_client_id', None,
    'The ID of the ad client with the specified custom channel',
    short_name='c')
gflags.MarkFlagAsRequired('ad_client_id')

gflags.DEFINE_string('custom_channel_id', None,
    'The ID of the custom channel for which to get ad units',
    short_name='x')
gflags.MarkFlagAsRequired('custom_channel_id')


def main(argv):
  # Process flags and read their values.
  sample_utils.process_flags(argv)
  account_id = gflags.FLAGS.account_id
  ad_client_id = gflags.FLAGS.ad_client_id
  custom_channel_id = gflags.FLAGS.custom_channel_id

  # Authenticate and construct service.
  service = sample_utils.initialize_service()

  try:
    # Retrieve ad unit list in pages and display data as we receive it.
    request = service.accounts().customchannels().adunits().list(
        accountId=account_id, adClientId=ad_client_id,
        customChannelId=custom_channel_id, maxResults=MAX_PAGE_SIZE)

    while request is not None:
      result = request.execute()
      ad_units = result['items']
      for ad_unit in ad_units:
        print ('Ad unit with code "%s", name "%s" and status "%s" was found. ' %
               (ad_unit['code'], ad_unit['name'], ad_unit['status']))

      request = service.adunits().list_next(request, result)

  except AccessTokenRefreshError:
    print ('The credentials have been revoked or expired, please re-run the '
           'application to re-authorize')

if __name__ == '__main__':
  main(sys.argv)
