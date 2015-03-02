#!/bin/sh

function complain_no_riakadmin {
    echo "$1 not found at supplied path"
    exit 1
}

if [ "$#" -ne 1 ]; then
    echo "Incorrect number of paramaters given."
    echo "Exactly one param is required: the path to the root dir of a riak node,"
    echo "e.g. /var/riak/node1"
    exit 1
fi

path_to_riak_node=$1
riak_admin=${path_to_riak_node}/bin/riak-admin

command -v $riak_admin >/dev/null 2>&1 || complain_no_riakadmin $riak_admin;

${riak_admin} bucket-type create egara-lww '{ "props": { "last_write_wins": true } }'
${riak_admin} bucket-type activate egara-lww
${riak_admin} bucket-type create egara-unique '{ "props": { "last_write_wins": true, "allow_mult": false  } }'
${riak_admin} bucket-type activate egara-unique

