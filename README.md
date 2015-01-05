Egara is an event driven groupware archival system for e-discovery and datai
loss prevention.

It is designed to be used with the Kolab Groupware server and is written
(primarily) in Erlang.

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
