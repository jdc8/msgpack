# This examples let's clients connect to it on port 12345 and decodes the
# MessagePack packed messages sent to it.

lappend auto_path ..
package require msgpack

# Start the server
set s [socket -server accept 12345]

# Called when a client connects
proc accept {s a p} {
    # Create an unpacker
    set o [msgpack::unpacker new]
    # unpack the client stream, call callback when a MessagePack object is read
    $o unpack_stream $s [list callback $o]
}

# Called when the streaming unpacker has read a MessagePack object
proc callback {o cmd data} {
    # Print result
    puts $o/$cmd/$data
    # Close socket and clean up the unpacker if client disconnected
    if {$cmd eq "eof"} {
    close $data
    $o destroy
    }
}

# Enter event loop
vwait forever
