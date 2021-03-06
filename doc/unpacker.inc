[section "Unpacker class"]

[list_begin definitions]

[call {msgpack::unpacker} [method new]]

[uri http://www.tcl.tk/man/tcl8.6/TclCmd/class.htm [cmd oo::class]]
implementing the MessagePack unpacking.

[call [arg unpackerObject] [method destroy]]

Destroy the unpacker object.

[call [arg unpackerObject] [method set_ext_unpacker] [opt [arg type]] [opt [arg script]]]

Set the handler for an extension type.  When the unpacker encounters extension
type [arg type], it will call [arg script] with the type and the data as its
arguments.  Omit [arg script] or [arg script] and [arg type] to get handlers.
Set the handler for a type to an empty string to disable it.

[call [arg unpackerObject] [method unpack_stream] [arg istream] [arg callback]]

Unpack data read from the [arg istream] argument. The [arg callback] command is
called when a MessagePack object is unpacked. Before calling the [arg callback]
command, the word [term data] and the unpacked MessagePack object is [term lappend]-ed
to the command. When the stream is closed ([term eof] detected), the
[arg callback] command is called with the word [term eof] and the stream handle
[term lappend]-ed.

[para]

The [arg istream] is configure like this:

[list_begin itemized]
[item] Non blocking
[item] Unbuffered
[item] Translation [term binary]
[item] Encoding [term binary]
[list_end]

[para]

Opening and closing the [arg istream] is the responsability of the script
calling the [method unpack_stream] method.

[call [arg unpackerObject] [method unpack_string] [arg istring] [opt [arg callback]]]

Unpack the specified data. If no [arg callback] command is specified, a list
with unpacked type (see below) and value pairs is returned. If a [arg callback]
command is specified, this command is called when a MessagePack object is
unpacked. Before calling the [arg callback] command, the word [term data] and
the unpacked MessagePack object is [term lappend]-ed to the command.

[list_end]

Type information found in the unpacked MessagePack objects can be one of the
following:

[list_begin options]
[opt_def array]
[opt_def bin]
[opt_def boolean]
[opt_def ext]
[opt_def float32]
[opt_def float64]
[opt_def integer]
[opt_def map]
[opt_def nil]
[opt_def str]
[opt_def timestamp]
[list_end]

Values can be nested type/value list.
