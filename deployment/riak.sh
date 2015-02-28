#!/bin/sh

path_to_riak_node="~/src/riak-2.0.5/dev/dev1"
riak_admin=${path_to_riak_node}/bin/riak-admin

${riak_admin} bucket-type create egara-lww '{ "props": { "last_write_wins": true } }'
${riak_admin} bucket-type activate egara-lww
${riak_admin} bucket-type create egara-unique '{ "props": { "last_write_wins": true, "allow_mult": false  } }'
${riak_admin} bucket-type activate egara-unique

