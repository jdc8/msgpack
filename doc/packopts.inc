
[section "Pack options"]

The arguments for the [method pack] command or method are always one or more type
specifiers and if needed a value. The list below shows the supported types:

[list_begin options]

[opt_def [method array] [arg size]]

Add array size to packed data. Must be followed by [cmd size] calls to method
[method pack] to add the array elements to the packed data.

[opt_def [method bin] [arg bytes]]

Add a byte array (a binary string) to the packed data.

[opt_def [method boolean] [arg data]]

Add a boolean to the packed data. Is equivalent calling methods
[method {pack true}] or [method {pack false}].

[opt_def [method dict] "[arg keyType] [arg valueType] [arg dictionaryValue]"]

Add a dict to the packed data. This is equivalent to calling method
[method {pack map}] with the dict size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the dict.

[opt_def [method ext] "[arg type] [arg bytes]"]

Add a byte array of a chosen extension type to the packed data.

[opt_def [method false]]

Add a boolean with value [cmd false] to the packed data.

[opt_def [method fix_ext1] "[arg type] [arg byte]"]

Add 1 byte of a chosen extension type to the packed data.

[opt_def [method fix_ext2] "[arg type] [arg bytes]"]

Add 2 bytes of a chosen extension type to the packed data.

[opt_def [method fix_ext4] "[arg type] [arg bytes]"]

Add 4 bytes of a chosen extension type to the packed data.

[opt_def [method fix_ext8] "[arg type] [arg bytes]"]

Add 8 bytes of a chosen extension type to the packed data.

[opt_def [method fix_ext16] "[arg type] [arg bytes]"]

Add 16 bytes of a chosen extension type to the packed data.

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

[opt_def [method float32] [arg data]]

Add a 32-bit float to the packed data.

[opt_def [method float64] [arg data]]

Add a 64-bit (double precision) float to the packed data.

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

[opt_def [method microseconds] [arg micros]]

Add a microsecond timestamp to the packed data as a [method timestamp96].

[opt_def [method milliseconds] [arg millis]]

Add a millisecond timestamp to the packed data as a [method timestamp96].

[opt_def [method nil]]

Add a nil to the packed data.

[opt_def [method short] [arg data]]

Add a short integer to the packed data.

[opt_def [method str] [arg string]]

Add a string to the packed data.

[opt_def [method tcl_array] "[arg keyType] [arg valueType] [arg arrayName]"]

Add a Tcl array to the packed data. This is equivalent to calling method
[method {pack map}] with the array size as argument, followed by calling
method [method {pack keyType}] and method [method {pack valueType}]
for each key/value pair in the array.

[opt_def [method timestamp32] [arg seconds]]

Add a 32-bit unsigned timestamp to the packed data.

[opt_def [method timestamp64] "[arg seconds] [arg nanoseconds]"]

Add a 64-bit timestamp (34 bits for seconds, 30 bits for nanoseconds,
both unsigned) to the packed data.  Nanoseconds must not exceed 999999999.

[opt_def [method timestamp96] "[arg seconds] [arg nanoseconds]"]

Add a 96-bit timestamp (64 bits for seconds, signed, and 32 bits for
nanoseconds, unsigned) to the packed data.  Nanoseconds must not exceed
999999999.

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
