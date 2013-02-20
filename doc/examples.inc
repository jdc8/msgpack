[section "Examples"]

Creating a [class msgpack::packer] object and packing some data:

[example {package require msgpack

set p [msgpack::packer new]

$p pack int 123456789
$p pack string "A MessagePack example"
$p pack dict int string {1 one 2 two}
set packed_data [$p data]
$p destroy}]

Now unpack the packed data using a [class msgpack::packer] object:

[example {package require msgpack

set u [msgpack::unpacker new]
$u unpack_string $packed_data

$u destroy}]

After unpacking, the following list of type/value pairs is returned by the [method unpack_string] method:

[example {{integer 123456789} {raw {A MessagePack example}} {map {{integer 1} {raw one} {integer 2} {raw two}}}}]

The same example using the [cmd pack] utility function for packing the data:

[example {set packed_data ""
append packed_data [msgpack pack int 0xFFFFFFFF]
append packed_data [msgpack pack string "A Utility example"]
append packed_data [msgpack pack dict int string {3 three 4 four}]}]

An using the [cmd unpack] utility function to unpack the data:

[example {puts [msgpack unpack $packed_data]}]

After unpacking, the following list of type/value pairs is returned by the [cmd unpack] utility function:

[example {{integer 4294967295} {raw {A Utility example}} {map {{integer 3} {raw three} {integer 4} {raw four}}}}]
