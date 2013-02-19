package require Tcl 8.6
package provide msgpack 1.0.0

namespace eval msgpack {

    variable packerId 0

    proc packer {} {
	variable packerId
	variable data
	set id $packerId
	incr packerId
	set data($id) ""
	set cmd msgpack::msgpacker$id
	proc $cmd {cmd args} [format {
	    variable data
	    switch -exact -- $cmd {
		pack {
		    append data(%1$s) [pack {*}$args]
		}
		data {
		    return $data(%1$s)
		}
		reset {
		    set data(%1$s) ""
		}
		destroy {
		    unset data(%1$s)
		    rename %2$s {}
		}
	    }
	} $id $cmd]
	return $cmd
    }

    proc unpacker {stream callback} {
	variable packerId
	variable data
	variable datastream
	variable datacallback
	set id $packerId
	incr packerId
	set coro msgpack::msgunpacker$id
	set data($id) ""
	set datastream($id) $stream
	set datacallback($id) $callback
	coroutine $coro msgpack::unpack_coro $id 0 1
	chan configure $stream -blocking 0 -buffering none -translation binary -encoding binary
	chan event $stream readable $coro
    }

    proc pack {type {value 0} {value1 ""} {value2 ""}} {
	switch -exact -- $type {
	    short { return [pack int16 $value] }
	    int { return [pack int32 $value] }
	    long { return [pack int32 $value] }
	    long_long { return [pack int64 $value] }
	    unsigned_short { return [pack uint16 $value] }
	    unsigned_int { return [pack uint32 $value] }
	    unsigned_long { return [pack uint32 $value] }
	    unsigned_long_long { return [pack uint64 $value] }
	    fixnumpos { return [binary format c [expr {$value & 0x7F}]] }
	    fixnumneg { return [binary format c [expr {($value & 0x1F) | 0xE0}]] }
	    int8 {
		if {$value < -32} {
		    return [pack fix_int8 $value]
		} else {
		    if {$value < 0} {
			return [pack fixnumneg $value]
		    } else {
			if {$value < 128} {
			    return [pack fixnumpos $value]
			} else {
			    return [pack fix_int8 $value]
			}
		    }
		}
	    }
	    int16 {
		if {$value < -128} {
		    return [pack fix_int16 $value]
		} elseif {$value < 128} {
		    return [pack int8 $value]
		} elseif {$value < 256} {
		    return [pack fix_uint8 $value]
		} else {
		    return [pack fix_uint16 $value]
		}
	    }
	    int32 {
		if {$value < -32768} {
		    return [pack fix_int32 $value]
		} elseif {$value < 65536} {
		    return [pack int16 $value]
		} else {
		    return [pack fix_uint32 $value]
		}
	    }
	    int64 {
		if {$value < -2147483648} {
		    return [pack fix_int64 $value]
		} elseif {$value < 4294967296} {
		    return [pack int32 $value]
		} else {
		    return [pack fix_uint64 $value]
		}
	    }
	    uint8 {
		set value [expr {$value & 0xFF}]
		if {$value < 128} {
		    return [pack fixnumpos $value]
		} else {
		    return [pack fix_uint8 $value]
		}
	    }
	    uint16 {
		set value [expr {$value & 0xFFFF}]
		if {$value < 256} {
		    return [pack uint8 $value]
		} else {
		    return [pack fix_uint16 $value]
		}
	    }
	    uint32 {
		set value [expr {$value & 0xFFFFFFFF}]
		if {$value < 65536} {
		    return [pack int16 $value]
		} else {
		    return [pack fix_uint32 $value]
		}
	    }
	    uint64 {
		set value [expr {$value & 0xFFFFFFFFFFFFFFFF}]
		if {$value < 4294967296} {
		    return [pack int32 $value]
		} else {
		    return [pack fix_uint64 $value]
		}
	    }
	    fix_int8 { return [binary format cc 0xD0 [expr {$value & 0xFF}]] }
	    fix_int16 { return [binary format cS 0xD1 [expr {$value & 0xFFFF}]] }
	    fix_int32 { return [binary format cI 0xD2 [expr {$value & 0xFFFFFFFF}]] }
	    fix_int64 { return [binary format cW 0xD3 [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }
	    fix_uint8 { return [binary format cc 0xCC [expr {$value & 0xFF}]] }
	    fix_uint16 { return [binary format cS 0xCD [expr {$value & 0xFFFF}]] }
	    fix_uint32 { return [binary format cI 0xCE [expr {$value & 0xFFFFFFFF}]] }
	    fix_uint64 { return [binary format cW 0xCF [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }
	    float { return [binary format cR 0xCA $value] }
	    double { return [binary format cQ 0xCB $value] }
	    nil { return [binary format c 0xC0] }
	    true { return [binary format c 0xC3] }
	    false { return [binary format c 0xC2] }
	    array {
		if {$value < 16} {
		    return [binary format c [expr {0x90 | $value}]]
		} elseif {$value < 65536} {
		    return [binary format cS 0xDC $value]
		} else {
		    return [binary format cI 0xDD $value]
		}
	    }
	    list {
		set r [pack array [llength $value1]]
		foreach e $value1 {
		    append r [pack $value $e]
		}
		return $r
	    }
	    map {
		if {$value < 16} {
		    return [binary format c [expr {0x80 | $value}]]
		} elseif {$value < 65536} {
		    return [binary format cS 0xDE $value]
		} else {
		    return [binary format cI 0xDF $value]
		}
	    }
	    tcl_array {
		upvar $value2 a
		set r [pack map [array size a]]
		foreach k [lsort -dictionary [array names a]] {
		    append r [pack $value $k]
		    append r [pack $value1 $a($k)]
		}
		return $r
	    }
	    dict {
		set r [pack map [dict size $value2]]
		dict for {k v} $value2 {
		    append r [pack $value $k]
		    append r [pack $value1 $v]
		}
		return $r
	    }
	    raw {
		if {$value < 32} {
		    return [binary format c [expr {0xA0 | $value}]]
		} elseif {$value < 65536} {
		    return [binary format cS 0xDA $value]
		} else {
		    return [binary format cI 0xDB $value]
		}
	    }
	    raw_body {
		return [binary format a* $value]
	    }
	    string {
		set n [string length $value]
		if {$n < 32} {
		    return [binary format ca* [expr {0xA0 | $n}] $value]
		} elseif {$n < 65536} {
		    return [binary format cSa* 0xDA $n $value]
		} else {
		    return [binary format cIa* 0xDB $n $value]
		}
	    }
	}
    }

    proc unpack {s {callback {}}} {
	variable packerId
	variable data
	variable datacallback
	set id $packerId
	incr packerId
	set data($id) $s
	set datacallback($id) $callback
	set l [unpack_coro $id 0 0]
	unset data($id)
	unset datacallback($id)
	if {[llength $callback] == 0} {
	    return $l
	}
    }

    proc need_coro {id n} {
	variable data
	variable datastream
	variable datacallback
	while {1} {
	    if {[eof $datastream($id)]} {
		{*}$datacallback($id) eof $datastream($id)
		unset data($id)
		unset datastream($id)
		unset datacallback($id)
		return -code return {}
	    }
	    append data($id) [read $datastream($id)]
	    if {[string length $data($id)] >= $n} break
	    yield
	}
    }

    proc need_string {id n} {
	variable data
	if {[string length $data($id)] < $n} {
	    error "input string not long enough, need $n bytes, only [string length $data($id)] left"
	}
    }

    proc unpack_coro {id nested coro} {
	if {$coro} {
	    if {!$nested} {
		# Yield creation
		yield
	    }
	    set need_proc need_coro
	} else {
	    set need_proc need_string
	}
	variable data
	variable datacallback
	set l {}
	while {1} {
	    $need_proc $id 1
	    binary scan $data($id) c c
	    set tc [expr {$c & 0xFF}]
	    set data($id) [string range $data($id) 1 end]
	    if {$tc < 0x80} {
		# Positive FixNum
		lappend l [list integer [expr {$c & 0x7F}]]
	    } elseif {($tc & 0xE0) >= 0xE0} {
		# Negative FixNum
		binary scan [binary format c [expr {($c & 0x1F) | 0xE0}]] c c
		lappend l [list integer $c]
	    } elseif {$tc >= 0x80 && $tc <= 0x8F} {
		# FixMap
		set n [expr {$tc & 0xF}]
		set a {}
		for {set i 0} {$i < $n} {incr i} {
		    lappend a {*}[unpack_coro $id 1 $coro]
		    lappend a {*}[unpack_coro $id 1 $coro]
		}
		lappend l [list map $a]
	    } elseif {$tc >= 0x90 && $tc <= 0x9F} {
		# FixArray
		set n [expr {$tc & 0xF}]
		set a {}
		for {set i 0} {$i < $n} {incr i} {
		    lappend a {*}[unpack_coro $id 1 $coro]
		}
		lappend l [list array $a]
	    } elseif {$tc >= 0xA0 && $tc <= 0xBF} {
		# FixRaw
		set n [expr {$tc & 0xF}]
		$need_proc $id $n
		binary scan $data($id) a$n c
		lappend l [list raw $c]
		set data($id) [string range $data($id) $n end]
	    } else {
		if {$tc == 0xC0} {
		    # nil
		    lappend l nil
		} elseif {$tc == 0xC2} {
		    # false
		    lappend l [list boolean 0]
		} elseif {$tc == 0xC3} {
		    # true
		    lappend l [list boolean 1]
		} elseif {$tc == 0xCA} {
		    # float
		    $need_proc $id 4
		    binary scan $data($id) R c
		    set data($id) [string range $data($id) 4 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCB} {
		    # double
		    $need_proc $id 8
		    binary scan $data($id) Q c
		    set data($id) [string range $data($id) 8 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCC} {
		    # uint8
		    $need_proc $id 1
		    binary scan $data($id) c c
		    set data($id) [string range $data($id) 1 end]
		    lappend l [list integer [expr {$c & 0xFF}]]
		} elseif {$tc == 0xCD} {
		    # uint16
		    $need_proc $id 2
		    binary scan $data($id) S c
		    set data($id) [string range $data($id) 2 end]
		    lappend l [list integer [expr {$c & 0xFFFF}]]
		} elseif {$tc == 0xCE} {
		    # uint32
		    $need_proc $id 4
		    binary scan $data($id) I c
		    set data($id) [string range $data($id) 4 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFF}]]
		} elseif {$tc == 0xCF} {
		    # uint64
		    $need_proc $id 8
		    binary scan $data($id) W c
		    set data($id) [string range $data($id) 8 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFFFFFFFFFF}]]
		} elseif {$tc == 0xD0} {
		    # int8
		    $need_proc $id 1
		    binary scan $data($id) c c
		    set data($id) [string range $data($id) 1 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD1} {
		    # int16
		    $need_proc $id 2
		    binary scan $data($id) S c
		    set data($id) [string range $data($id) 2 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD2} {
		    # int32
		    $need_proc $id 4
		    binary scan $data($id) I c
		    set data($id) [string range $data($id) 4 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD3} {
		    # int64
		    $need_proc $id 8
		    binary scan $data($id) W c
		    set data($id) [string range $data($id) 8 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xDA} {
		    # raw 16
		    $need_proc $id 2
		    binary scan $data($id) S n
		    set n [expr {$n & 0xFFFF}]
		    set data($id) [string range $data($id) 2 end]
		    $need_proc $id $n
		    binary scan $data($id) a$n c
		    lappend l [list raw $c]
		    set data($id) [string range $data($id) $n end]
		} elseif {$tc == 0xDB} {
		    # raw 32
		    $need_proc $id 4
		    binary scan $data($id) I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data($id) [string range $data($id) 4 end]
		    $need_proc $id $n
		    binary scan $data($id) a$n c
		    lappend l [list raw $c]
		    set data($id) [string range $data($id) $n end]
		} elseif {$tc == 0xDC} {
		    # array 16
		    $need_proc $id 2
		    binary scan $data($id) S n
		    set n [expr {$n & 0xFFFF}]
		    set data($id) [string range $data($id) 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack_coro $id 1 $coro]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDD} {
		    # array 32
		    $need_proc $id 4
		    binary scan $data($id) I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data($id) [string range $data($id) 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack_coro $id 1 $coro]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDE} {
		    # map 16
		    $need_proc $id 2
		    binary scan $data($id) S n
		    set n [expr {$n & 0xFFFF}]
		    set data($id) [string range $data($id) 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack_coro $id 1 $coro]
			lappend a {*}[unpack_coro $id 1 $coro]
		    }
		    lappend l [list map $a]
		} elseif {$tc == 0xDF} {
		    # map 32
		    $need_proc $id 4
		    binary scan $data($id) I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data($id) [string range $data($id) 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack_coro $id 1 $coro]
			lappend a {*}[unpack_coro $id 1 $coro]
		    }
		    lappend l [list map $a]
		}
	    }
	    if {$nested} {
		return $l
	    } else {
		if {[llength $datacallback($id)]} {
		    foreach i $l {
			{*}$datacallback($id) data $i
		    }
		    set l {}
		}
		if {!$coro} {
		    if {[string length $data($id)] == 0} {
			return $l
		    }
		}
	    }
	}
    }

    proc map2dict {m} {
	set d [dict create]
	foreach {ki vi} $m {
	    lassign $ki kt kv
	    lassign $vi vt vv
	    dict set d $kv $vv
	}
	return $d
    }

    proc map2array {m anm} {
	upvar $anm a
	foreach {ki vi} $m {
	    lassign $ki kt kv
	    lassign $vi vt vv
	    set a($kv) $vv
	}
    }

    proc array2list {a} {
	set l [list]
	foreach i $a {
	    lassign $i t v
	    lappend l $v
	}
	return $l
    }

    namespace export *
    namespace ensemble create
}
