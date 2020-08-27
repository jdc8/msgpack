[section "Examples"]

Creating a [class msgpack::packer] object and packing some data:

[example {package require msgpack

set p [msgpack::packer new]

$p pack int 123456789
$p pack str "A MessagePack example"
$p pack dict int str {1 one 2 two}
set packed_data [$p data]
$p destroy}]

Now unpack the packed data using a [class msgpack::packer] object:

[example {package require msgpack

set u [msgpack::unpacker new]
$u unpack_string $packed_data

$u destroy}]

After unpacking, the following list of type/value pairs is returned by the [method unpack_string] method:

[example {{integer 123456789} {str {A MessagePack example}} {map {{integer 1} {str one} {integer 2} {str two}}}}]

The same example using the [cmd pack] utility function for packing the data:

[example {set packed_data ""
append packed_data [msgpack pack int 0xFFFFFFFF]
append packed_data [msgpack pack bin "A Utility example"]
append packed_data [msgpack pack dict int str {3 three 4 four}]}]

An using the [cmd unpack] utility function to unpack the data:

[example {puts [msgpack unpack $packed_data]}]

After unpacking, the following list of type/value pairs is returned by the [cmd unpack] utility function:

[example {{integer 4294967295} {bin {A Utility example}} {map {{integer 3} {str three} {integer 4} {str four}}}}]

With [cmd set_ext_unpacker] you can register a handler to unpack custom extension types.

[example {set up [msgpack::unpacker new]

proc xor {n type data} {
    set res {}
    foreach b [split $data {}] {
        set code [scan $b %c]
        append res [format %c [expr { $code ^ $n }]]
    }

    return [list encrypted $res]
}

$up set_ext_unpacker 100 {xor 5}
# Prints "{encrypted Hello!}".
puts [$up unpack_string [msgpack pack ext 100 M`iij$]]
$up destroy}]
