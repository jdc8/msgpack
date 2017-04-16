[section "Packer class"]

[list_begin definitions]

[call {msgpack::packer} [method new]]

[uri http://www.tcl.tk/man/tcl8.6/TclCmd/class.htm [cmd oo::class]]
implementing the MessagePack packing.

[call [arg packerObject] [method data]]

Return the packed data.

[call [arg packerObject] [method destroy]]

Destroy the packer object.

[call [arg packerObject] [method pack] [arg type] [opt [arg value]]]

Pack the specified [arg value] as [arg type] and store it internally. More
information on how to specify values to be packed can be found in section
[sectref "Pack options"].

[call [arg packerObject] [method pack_list] [arg list]]

Pack the specified [arg list] of (nested) key/value pair and store it
internally.

[call [arg packerObject] [method reset]]

Reset the packer.

[list_end]
