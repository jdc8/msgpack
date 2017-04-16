[section "Utilities"]

[list_begin definitions]

[call [cmd {msgpack array2list}]]

Convert a MessagePack array as retuned by the [cmd unpack] command or method
into a Tcl list.

[call [cmd {msgpack map2array}]]

Convert a MessagePack map as retuned by the [cmd unpack] command or method into
a Tcl array.

[call [cmd {msgpack map2dict}]]

Convert a MessagePack map as retuned by the [cmd unpack] command or method into
a Tcl dict.

[call [cmd {msgpack pack}] [arg list]]

Pack the specified [arg list] of type and value pairs. The packed value is returned.

[call [cmd {msgpack unpack}] [arg string]]3

Unpack the specified data. A list with unpacked type (see [sectref "Unpacker class"]) and value
pairs is returned.

[list_end]
