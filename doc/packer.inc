[section "Packer"]

[subsection {Packer PACKAGE COMMANDS}]

[list_begin definitions]

[call [cmd {msgpack packer}] [opt [arg packerName]]]

This command creates a new MessagePack packer object and associated Tcl object
command whose name is [arg packerName] if specified or auto generated if not
specified. The object command will be created under the current namespace if the
[arg packerName] is not fully qualified, and in the specified namespace
otherwise. The object command name is returned by this command. The object
command methods are explained in section [sectref {Packer OBJECT METHODS}].

[list_end]

[subsection {Packer OBJECT METHODS}]

[list_begin definitions]

[call [arg {packerName}] [method data]]

Return the packed data.

[call [arg {packerName}] [method destroy]]

Destroy the packer and delete the associated Tcl object command.

[call [arg {packerName}] [method pack] [method array] [arg size]]

Add array size to packed data. Must be followed by [cmd size] calls to method
[method pack] to add the array elements to the packed data.

[call [arg {packerName}] [method pack] [method boolean] [arg data]]

Add a boolean to the packed data. Is equivalent calling methods
[method {pack true}] or [method {pack false}].

[call [arg {packerName}] [method pack] [method dict] [arg keyType] [arg valueType] [arg data]]

Add a dict to the packed data. This is equivalent to calling method
[method {pack map}] with the dict size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the dict.

[call [arg {packerName}] [method pack] [method double] [arg data]]

Add a double to the packed data.

[call [arg {packerName}] [method pack] [method false]]

Add a boolean with value [cmd false] to the packed data.

[call [arg {packerName}] [method pack] [method fix_int8] [arg data]]

Add an 8 bit integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_int16] [arg data]]

Add a 16 bit integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_int32] [arg data]]

Add a 32 bit integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_int64] [arg data]]

Add a 64 bit integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_uint8] [arg data]]

Add an 8 bit unsigned integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_uint16] [arg data]]

Add a 16 bit unsigned integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_uint32] [arg data]]

Add a 32 bit unsigned integer to the packed data.

[call [arg {packerName}] [method pack] [method fix_uint64] [arg data]]

Add a 64 bit unsigned integer to the packed data.

[call [arg {packerName}] [method pack] [method float] [arg data]]

Add a float to the packed data.

[call [arg {packerName}] [method pack] [method int] [arg data]]

Add an integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method int8] [arg data]]

Add an 8 bit integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method int16] [arg data]]

Add a 16 bit integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method int32] [arg data]]

Add a 32 bit integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method int64] [arg data]]

Add a 64 bit integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method list] [arg elemenType] [arg data]]

Add a Tcl list to the packed data. This is equivalent to calling method
[method {pack array}] with the list length as argument followed by calls to method
[method {pack elementType}] for each list element.

[call [arg {packerName}] [method pack] [method long] [arg data]]

Add a long integer to the packed data.

[call [arg {packerName}] [method pack] [method long_long] [arg data]]

Add a long long integer to the packed data.

[call [arg {packerName}] [method pack] [method map] [arg size]]

Add the map size to the packed data. Must be followed by [cmd size] pairs of
calls to method [method pack] to add the keys and values to the packed data.

[call [arg {packerName}] [method pack] [method nil]]

Add a nil to the packed data.

[call [arg {packerName}] [method pack] [method raw] [arg size]]

Add the size of a raw body to the packed data. Must precede a call to
method [method {pack raw_body}].

[call [arg {packerName}] [method pack] [method raw_body] [arg data]]

Add raw data to the packed data. Must be preceded by a call to method [method {pack raw}].

[call [arg {packerName}] [method pack] [method short] [arg data]]

Add a short integer to the packed data.

[call [arg {packerName}] [method pack] [method string] [arg data]]

Add a string to the packed data. Is equivalent to calling methods [method {pack raw}]
and [method {pack raw_body}].

[call [arg {packerName}] [method pack] [method tclarray] [arg keyType] [arg valueType] [arg arrayName]]

Add a Tcl array to the packed data. This is equivalent to calling method
[method {pack map}] with the array size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the array.

[call [arg {packerName}] [method pack] [method true]]

Add a boolean with value [cmd true] to the packed data.

[call [arg {packerName}] [method pack] [method uint8] [arg data]]

Add an 8 bit unsigned integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method uint16] [arg data]]

Add a 16 bit unsigned integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method uint32] [arg data]]

Add a 32 bit unsigned integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method uint64] [arg data]]

Add a 64 bit unsigned integer to the packed data, let the packer choose the best packing.

[call [arg {packerName}] [method pack] [method unsigned_int] [arg data]]

Add an unsigned integer to the packed data.

[call [arg {packerName}] [method pack] [method unsigned_long] [arg data]]

Add a unsigned long integer to the packed data.

[call [arg {packerName}] [method pack] [method unsigned_long_long] [arg data]]

Add an unsigned long long integer to the packed data.

[call [arg {packerName}] [method pack] [method unsigned_short] [arg data]]

Add an unsigned short integer to the packed data.

[call [arg {packerName}] [method reset]]

Reset the packer.

[list_end]
