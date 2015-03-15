= Egara Data Storage

The structure of keys and values as stored by egara is documented
below. It is divided into one section for each type of data is manages,
which is then further subdivided by bucket.

= Riak Configuration

Egara relies on a set of bucket types being available at runtime. As Riak
currently does not provide an API for creating bucket types, the script in
deployment/riak.sh must be run once on any one of the systems that is
hosting a Riak node prior to using Egara. Egara may be running when the script
is run, but will not function properly until it completed.

= IMAP Data

There are four buckets in use:

    * users: data about individual users at a given point in time
    * imap_folders: data about individual folders at a given point in time
    * imap_events: record of individual changes to messages, folders, etc
    * imap_message_timeline: the timeline of renames for a given message

== "users"

The users bucket contains a full replication of the data stored in LDAP at
the time of retrieval for a given user. This data is stored both in a
timestamped set of records as well as a special "current" record. The latter
facilitates quick look ups.

The record formats are:

    Bucket Type     Bucket          Key Format
    -----------     ------          ----------
    egara-lww       users           <LDAP User Id>::<Timestamp>::<IMAP Login>
    egara-unique    users-current   <IMAP Login>

The data stored is a JSON representation of the user's LDAP record:

    EXAMPLE NEEDED

== "imap-folders"

Data stored in this bucket are JSON representations of the folder name and relevant
metadata at the time of retrieval, creating a timeline for the given folder.
A copy of the most recent data is also stored.

The keys used are:

    Bucket Type     Bucket                  Key Format
    -----------     ------                  ----------
    egara-lww       imap-folders            <Folder UID>::<Timestamp>::<Folder Path>
    egara-unique    imap-folders-current    <Folder Path>

Example of the data stored:

    EXAMPLE NEEDED

== "imap-events"

The data in this bucket are JSON representations of individual events that
occure in the IMAP data store. These include:

    FlagsClear
    FlagsSet
    MessageAppend
    MessageCopy
    MessageExpire
    MessageExpunge
    MessageMove
    MessageNew
    MessageRead
    MessageTrash
    AclChange
    MailboxCreate
    MailboxDelete
    MailboxRename
    MailboxSubscribe
    MailboxUnSubscribe
    Login
    Logout
    QuotaChange
    QuotaExceeded
    QuotaWithin

The key is a tuple representing the message UID and folder with timestamp:

    [TYPE]::<Folder UID>::<Message UID>::<Timestamp>

TYPE represents the category of event, including:

    message
    mailbox
    session
    quota

The timestamp comes from the event when it exists or is generated at time of
storage when it doesn't.

Event records are stored as JSON which may look like this:

    {
        "clientIP": "::1",
        "clientPort": 46461,
        "event": "Login",
        "pid": 7629,current-
        "serverDomain": "::1",
        "serverPort": 143,
        "service": "imap",
        "timestamp": "2015-02-23T14:47:36.097+01:00",
        "uri": "imap://kolab.example.org",
        "user": "john.doe@example.org",
        "vnd.cmu.sessionId": "kolab.example.org-7629-1424699256-1-3981462903180119079"
    }

The exact contents vary from event to event.

(TODO: document the results from various events, perhaps as individual specimen files
       with reference to that location here)

== "imap-message-timeline"

In order to work-around the problem of not having globally unique message IDs
over arbitrary time frames, Egara stores a side-channel data stream that documents
moves of messages. With this timeline in hand, a given record from imap_events can
be mapped to the correct object at a given point in time.

The format of the key used is:

    <Message UID>::<Folder>::<Timestamp>

The data consists of JSON in the following format:

    { "message": <UID>, "folder": "<PATH>" }