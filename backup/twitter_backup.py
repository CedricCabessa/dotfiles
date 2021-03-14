#!/usr/bin/env python3

import os
import twitter
import yaml

api = twitter.Api(
    consumer_key=os.getenv("TWITTER_CONSUMER_KEY"),
    consumer_secret=os.getenv("TWITTER_CONSUMER_SECRET"),
    access_token_key=os.getenv("TWITTER_ACCESS_TOKEN"),
    access_token_secret=os.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
)

backup = {"list": {}, "following": []}
for lst in api.GetLists():
    backup["list"][lst.slug] = []
    for user in sorted(api.GetListMembers(lst.id), key=lambda u: u.screen_name):
        backup["list"][lst.slug].append("@%s" % user.screen_name)

friends = sorted(api.GetFriends(), key=lambda u: u.screen_name)
for user in friends:
    backup["following"].append("@%s %s" % (user.screen_name, user.name))

print(yaml.dump(backup, default_flow_style=False))
