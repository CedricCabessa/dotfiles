#!/bin/bash

notmuch new

notmuch tag +gitlab tag:new from:system@online.net
notmuch tag +jira tag:new from:jira@online.net
notmuch tag +sentry tag:new from:sentry@sentry.pv.ocshq.com
notmuch tag +sentry tag:new from:sentry@internal.scaleway.com
notmuch tag +sentry tag:new subject:sentry from:monitoring-team@scaleway.com
notmuch tag -new tag:new
