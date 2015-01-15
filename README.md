Egara is an event driven groupware archival system for e-discovery and data
loss prevention. The name means "storehouse" in Sumerian.

It is designed to be used with the Kolab Groupware server and is written
(primarily) in Erlang.

== Functional Overview

Egara is a parallelized pipeline that accepts events, normalizes them by
standardizing and finalizing the data passed in, and then stores the resulting
data in a centralized key/value store for later processing and retrieval.

This implies a three-stage system:

    0. Listen for incoming events and normalize data if needed
    1. Gather any additional data implied by the event and add it to the data
    2. Store the final dataset

Incoming events are accepted by "incoming handlers" which subsequently process
those into a normalized set of Erlang terms. For example, the cyrus imap
handler listens for notifications on a local dgram socket, parses the included
json into Erlang terms and passes the results on to the egara notifications
receiver process. At this point the events are required to be in a normalized
form. The receiver process then inserts the event into a queue for further
processing.

Worker processes claim items in the queue, identify any missing data (such
as user information), add the missing data and finally store the results.
A worker may also decide to defer processing of the event or even drop the
event as uninteresting.

Unless dropped by the worker, the event is removed from the incoming queue.
This is also handled by the worker process, allowing for a linear pipeline
of processing per event.

This system can be extended at the following points:

    * additional sources of events can be added as incoming handlers
    * workers can have new processing logic added

Access to certain resources, along with the workers themselves, are handled
by processes kept in stateful pools. Currently these pools include:

    * IMAP backchannel to cyrus imap
    * LDAP connections
    * Riak connections
    * Workers

Once the data is stored in the key/value store it can be queried via a web
service or direct access to the Riak cluster. Additional processing may
occur at this point, however event data stored is considered immutable:
once written they may never be changed. The only alteration to events that
is allowed for is deletion.

This paves the way for collecting events from multiple sources to be used
for eDiscovery, data loss prevention and usage analytics.

Currently, Egara focuses on capturing the following event data:

    * new messages in the Kolab IMAP store (email, calendaring, etc.)
    * deletions and modifications of message in the Kolab IMAP store
    * folder creation, deletion and modifications
    * user sessions

== Building

On the first build run `make deps-up` then apply the patches in the deps_diffs
directory. (Mostly this adjusts the dependencies in these applications to be
in line with each other. Eventually this will be either automated or fixed
in upstream repositories; probably the latter but for now this is convenient
as dependencies are still shifting at this stage of development.)

Note that the riak diff requires using `git apply` rather than the usual `patch`
command line utility. Also, you may need to manually go into deps/lager and perform
a make there before the top-level make will work. Ah, dependency hell. ;)

Next, simply run `make`. This should get you all the dependencies required and build
the application which can be run from in the base directory with `make run`.

After the first build, `make egara` is faster and should be all that is needed
unless the dependencies need updating.

== Usage

... not yet :)
