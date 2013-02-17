package ifneeded msgpack 1.0.0 [list source [file join $dir msgpack.tcl]]
package ifneeded tclmsgpack 1.0.0 "set ::tclmsgpack 1 ; source \"[file join $dir msgpack.tcl]\""
