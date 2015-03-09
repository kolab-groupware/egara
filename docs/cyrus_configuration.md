Cyrus IMAP must be configured correctly to send notification events
to egara. This involves two files: cyrus.conf and imapd.conf.

= /etc/cyrus.conf

The following line must be placed in the cyrus.conf file in the SERVICES section:

    notify      cmd="notifyd" listen="/var/lib/imap/socket/notify" proto="udp" prefork=1

This is the default in Kolab Enterprise 14.

= /etc/imapd.conf

In /etc/imapd.conf add:

    event_content_inclusion_mode: standard
    event_content_size: 0
    event_extra_params: bodyStructure clientAddress diskUsed flagNames messageSize \
            messages modseq service timestamp uidnext vnd.cmu.midset \
            vnd.cmu.unseenMessages vnd.cmu.envelope vnd.cmu.sessionId
    event_groups: message quota flags access mailbox subscription
    event_notifier: log

If Egara is running on the machine, this will route all relevant notifications to
Egara. Otherwise it will cause notifications to appear in the relevant syslog.
