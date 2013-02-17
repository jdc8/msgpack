[section "Utilities"]

[list_begin definitions]

[call [cmd {msgpack map2dict}]]

Convert a MessagePack map as retuned by the [cmd unpack] command into a Tcl dict.

[call [cmd {msgpack map2array}]]

Convert a MessagePack map as retuned by the [cmd unpack] command into a Tcl array.

[call [cmd {msgpack array2list}]]

Convert a MessagePack array as retuned by the [cmd unpack] command into a Tcl list.

[list_end]
