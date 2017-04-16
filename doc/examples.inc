[section "Examples"]

Creating a [class msgpack::packer] object and packing some data:

[example {package require msgpack

set p [msgpack::packer new]

$p pack integer 123456789
$p pack string "A MessagePack example"
set packed_data [$p data]
$p destroy}]

Now unpack the packed data using a [class msgpack::packer] object:

[example {package require msgpack

set u [msgpack::unpacker new]
$u unpack_string $packed_data

$u destroy}]

After unpacking, the following list of type/value pairs is returned by the [method unpack_string] method:

[example {{integer 123456789} {string {A MessagePack example}}}]

The same example using the [cmd pack] utility function for packing the data:

[example {set packed_data ""
append packed_data [msgpack pack int 0xFFFFFFFF]
append packed_data [msgpack pack string "A Utility example"]}]

An using the [cmd unpack] utility function to unpack the data:

[example {puts [msgpack unpack $packed_data]}]

After unpacking, the following list of type/value pairs is returned by the [cmd unpack] utility function:

[example {{integer 4294967295} {string {A Utility example}}}]
