[section "Packer class"]

[list_begin definitions]

[call {msgpack::packer} [method new]]

[uri http://www.tcl.tk/man/tcl8.6/TclCmd/class.htm [cmd oo::class]]
implementing the MessagePack packing.

[call [arg packerObject] [method data]]

Return the packed data.

[call [arg packerObject] [method destroy]]

Destroy the packer object.

[call [arg packerObject] [method pack] [arg args]]

Pack the specified [arg value] and store it internally. More information on how
to specify values to be packed can be found in section [sectref "Pack options"].
To get the packed data, use the [method data] method.

[call [arg packerObject] [method reset]]

Reset the packer.

[list_end]
