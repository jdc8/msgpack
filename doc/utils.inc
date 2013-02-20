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

[call [cmd {msgpack pack}] [arg args]]

Pack the specified [arg value]. The packed value is returned. More information
on how to specify values to be packed can be found in section
[sectref "Pack options"].

[call [cmd {msgpack unpack}] [arg string]]

Unpack the specified data. A list with unpacked type (see [sectref "Unpacker class"]) and value
pairs is returned.

[list_end]
