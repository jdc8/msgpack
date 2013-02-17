[section "Unpacker"]

[list_begin definitions]

[call [cmd {msgpack unpack}] [arg data]]

Unpack the specified data. A list with unpacked type and value information pairs
is returned. Type information can be one of the following:

[list_begin options]
[opt_def nil]
[opt_def boolean]
[opt_def integer]
[opt_def double]
[opt_def raw]
[opt_def array]
[opt_def map]
[list_end]

Values can be nested type/value list.

[list_end]
