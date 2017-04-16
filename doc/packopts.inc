
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

[opt_def [method double] [arg data]]

Add a 64 bit double to the packed data.

[opt_def [method false]]

Add a boolean with value [cmd false] to the packed data.

[opt_def [method fix_numpos] [arg data]]

Add a snall positive integer to the packed data.

[opt_def [method fix_numneg] [arg data]]

Add a small negative integer to the packed data.

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

Add a 32 bit float to the packed data.

[opt_def [method integer] [arg data]]

Add an integer to the packed data, let the packer choose the best packing.

[opt_def [method unsigned] [arg data]]

Add an unsigned integer to the packed data, let the packer choose the best packing.

[opt_def [method map] [arg size]]

Add the map size to the packed data. Must be followed by [cmd size] pairs of
calls to method [method pack] to add the keys and values to the packed data.

[opt_def [method nil]]

Add a nil to the packed data.

[opt_def [method bin] [arg size]]

Add the size of a bin body to the packed data. Must precede a call to
method [method {pack bin_body}].

[opt_def [method bin_body] [arg data]]

Add bin data to the packed data. Must be preceded by a call to method [method {pack bin}].

[opt_def [method short] [arg data]]

Add a short integer to the packed data.

[opt_def [method string] [arg string]]

Add a string to the packed data. Is equivalent to calling methods [method {pack string}]
and [method {pack string_body}].

[opt_def [method true]]

Add a boolean with value [cmd true] to the packed data.

[list_end]
