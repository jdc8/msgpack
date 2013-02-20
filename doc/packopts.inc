
[section "Pack options"]

The arguments for the [method pack] command or method are always one or more type
specifiers and if needed a value. The list below shows the supported types:

[list_begin options]

[opt_def [method array] [arg size]]

Add array size to packed data. Must be followed by [cmd size] calls to method
[method pack] to add the array elements to the packed data.

[opt_def [method boolean] [arg data]]

Add a boolean to the packed data. Is equivalent calling methods
[method {pack true}] or [method {pack false}].

[opt_def [method dict] "[arg keyType] [arg valueType] [arg dictionaryValue]"]

Add a dict to the packed data. This is equivalent to calling method
[method {pack map}] with the dict size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the dict.

[opt_def [method double] [arg data]]

Add a double to the packed data.

[opt_def [method false]]

Add a boolean with value [cmd false] to the packed data.

[opt_def [method fix_int8] [arg data]]

Add an 8 bit integer to the packed data.

[opt_def [method fix_int16] [arg data]]

Add a 16 bit integer to the packed data.

[opt_def [method fix_int32] [arg data]]

Add a 32 bit integer to the packed data.

[opt_def [method fix_int64] [arg data]]

Add a 64 bit integer to the packed data.

[opt_def [method fix_uint8] [arg data]]

Add an 8 bit unsigned integer to the packed data.

[opt_def [method fix_uint16] [arg data]]

Add a 16 bit unsigned integer to the packed data.

[opt_def [method fix_uint32] [arg data]]

Add a 32 bit unsigned integer to the packed data.

[opt_def [method fix_uint64] [arg data]]

Add a 64 bit unsigned integer to the packed data.

[opt_def [method float] [arg data]]

Add a float to the packed data.

[opt_def [method int] [arg data]]

Add an integer to the packed data, let the packer choose the best packing.

[opt_def [method int8] [arg data]]

Add an 8 bit integer to the packed data, let the packer choose the best packing.

[opt_def [method int16] [arg data]]

Add a 16 bit integer to the packed data, let the packer choose the best packing.

[opt_def [method int32] [arg data]]

Add a 32 bit integer to the packed data, let the packer choose the best packing.

[opt_def [method int64] [arg data]]

Add a 64 bit integer to the packed data, let the packer choose the best packing.

[opt_def [method list] "[arg elemenType] [arg list]"]

Add a Tcl list to the packed data. This is equivalent to calling method
[method {pack array}] with the list length as argument followed by calls to method
[method {pack elementType}] for each list element.

[opt_def [method long] [arg data]]

Add a long integer to the packed data.

[opt_def [method long_long] [arg data]]

Add a long long integer to the packed data.

[opt_def [method map] [arg size]]

Add the map size to the packed data. Must be followed by [cmd size] pairs of
calls to method [method pack] to add the keys and values to the packed data.

[opt_def [method nil]]

Add a nil to the packed data.

[opt_def [method raw] [arg size]]

Add the size of a raw body to the packed data. Must precede a call to
method [method {pack raw_body}].

[opt_def [method raw_body] [arg data]]

Add raw data to the packed data. Must be preceded by a call to method [method {pack raw}].

[opt_def [method short] [arg data]]

Add a short integer to the packed data.

[opt_def [method string] [arg string]]

Add a string to the packed data. Is equivalent to calling methods [method {pack raw}]
and [method {pack raw_body}].

[opt_def [method tcl_array] "[arg keyType] [arg valueType] [arg arrayName]"]

Add a Tcl array to the packed data. This is equivalent to calling method
[method {pack map}] with the array size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the array.

[opt_def [method true]]

Add a boolean with value [cmd true] to the packed data.

[opt_def [method uint8] [arg data]]

Add an 8 bit unsigned integer to the packed data, let the packer choose the best packing.

[opt_def [method uint16] [arg data]]

Add a 16 bit unsigned integer to the packed data, let the packer choose the best packing.

[opt_def [method uint32] [arg data]]

Add a 32 bit unsigned integer to the packed data, let the packer choose the best packing.

[opt_def [method uint64] [arg data]]

Add a 64 bit unsigned integer to the packed data, let the packer choose the best packing.

[opt_def [method unsigned_int] [arg data]]

Add an unsigned integer to the packed data.

[opt_def [method unsigned_long] [arg data]]

Add a unsigned long integer to the packed data.

[opt_def [method unsigned_long_long] [arg data]]

Add an unsigned long long integer to the packed data.

[opt_def [method unsigned_short] [arg data]]

Add an unsigned short integer to the packed data.

[list_end]
