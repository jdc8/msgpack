# This examples send MessagePack packed messages to the server byte per byte

lappend auto_path ..
package require msgpack

# Option to test streaming unpacker
set byte_per_byte 0

# Open the connection and configure the socket
set s [socket localhost 12345]
chan configure $s -blocking 0 -buffering none -translation binary

# Create a packer
set p [msgpack::packer new]

while {1} {
    # Pack some random data: an int, a string (MessagePack str), a list (MessagePack array) and a dict (MessagePack map)
    $p pack int [expr {int(100000*rand())}]
    set l [string repeat [expr {int(10*rand())}] [expr {int(10*rand())}]]
    $p pack str $l
    set l {}
    set n [expr {2*int(10*rand())}]
    for {set i 0} {$i < $n} {incr i} {
    lappend l [expr {int(65536*rand())}]
    }
    $p pack list int $l
    $p pack dict int int $l
    # Print for debugging
    puts "Send: [msgpack unpack [$p data]]"
    # Send to server
    if {$byte_per_byte} {
    # Send byte after byte to test streaming unpacker
    set d [$p data]
    for {set i 0} {$i < [string length $d]} {incr i} {
        puts -nonewline $s [string index $d $i]
        after $byte_per_byte "set continue 1"
        vwait continue
    }
    } else {
    puts -nonewline $s [$p data]
    }
    # Reset the packers
    $p reset
    # Wait a little to be able to check results
    after 1000
}
