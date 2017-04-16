package require Tcl 8.6
package provide msgpack 2.1.0

namespace eval msgpack {}

oo::class create msgpack::packer {

    variable data

    constructor {} {
	set data ""
    }

    destructor {}

    method data {} { return $data }

    method reset {} { set data "" }

    method pack_list {l} {
	foreach i $l {
	    lassign $i t v
	    switch -exact -- $t {
		array {
		    my pack array [llength $v]
		    my pack_list $v
		}
		map {
		    my pack map [expr {[llength $v] / 2}]
		    my pack_list $v
		}
		bin - raw {
		    my pack bin [string length $v]
		    my pack bin_body $v
		}
		default {
		    my pack $t $v
		}
	    }
	}
    }

    method pack {type {value ""}} {
	switch -exact -- $type {
	    nil { append data [binary format c 0xC0] }
	    true { append data [binary format c 0xC3] }
	    false { append data [binary format c 0xC2] }

	    fix_numpos { append data [binary format c [expr {$value & 0x7F}]] }
	    fix_numneg { append data [binary format c [expr {($value & 0x1F) | 0xE0}]] }

	    fix_uint8 { append data [binary format cc 0xCC [expr {$value & 0xFF}]] }
	    fix_uint16 { append data [binary format cS 0xCD [expr {$value & 0xFFFF}]] }
	    fix_uint32 { append data [binary format cI 0xCE [expr {$value & 0xFFFFFFFF}]] }
	    fix_uint64 { append data [binary format cW 0xCF [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }

	    fix_int8 { append data [binary format cc 0xD0 [expr {$value & 0xFF}]] }
	    fix_int16 { append data [binary format cS 0xD1 [expr {$value & 0xFFFF}]] }
	    fix_int32 { append data [binary format cI 0xD2 [expr {$value & 0xFFFFFFFF}]] }
	    fix_int64 { append data [binary format cW 0xD3 [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }

	    float32 - float { append data [binary format cR 0xCA $value] }
	    float64 - double { append data [binary format cQ 0xCB $value] }

	    string {
		set n [string length $value]
		if {$n < 32} {
		    append data [binary format ca* [expr {0xA0 | $n}] $value]
		} elseif {$n < 256} {
		    append data [binary format cca* 0xD9 $n $value]
		} elseif {$n < 65536} {
		    append data [binary format cSa* 0xDA $n $value]
		} else {
		    append data [binary format cIa* 0xDB $n $value]
		}
	    }

	    bin - raw {
		# value contains the binary data size, pack binary data with type bin_body
		if {$value < 256} {
		    append data [binary format cc 0xC4 $value]
		} elseif {$value < 65536} {
		    append data [binary format cS 0xC5 $value]
		} else {
		    append data [binary format cI 0xC6 $value]
		}
	    }
	    bin_body - raw_body {
		append data [binary format a* $value]
	    }

	    array {
		# value contains the array size, pack array elements separately
		if {$value < 16} {
		    append data [binary format c [expr {0x90 | $value}]]
		} elseif {$value < 65536} {
		    append data [binary format cS 0xDC $value]
		} else {
		    append data [binary format cI 0xDD $value]
		}
	    }

	    map {
		# value contains the map size, pack map keys/values pairs separately
		if {$value < 16} {
		    append data [binary format c [expr {0x80 | $value}]]
		} elseif {$value < 65536} {
		    append data [binary format cS 0xDE $value]
		} else {
		    append data [binary format cI 0xDF $value]
		}
	    }

	    boolean {
		# convenience function
		if {$value} {
		    append data [my pack true]
		} else {
		    append data [my pack false]
		}
	    }

	    int - integer {
		# convenience function
		if {$value < -0x80000000} {
		    append data [my pack fix_int64 $value]
		} elseif {$value < -0x8000} {
		    append data [my pack fix_int32 $value]
		} elseif {$value < -0x80} {
		    append data [my pack fix_int16 $value]
		} elseif {$value < -0x20} {
		    append data [my pack fix_int8 $value]
		} elseif {$value < 0} {
		    append data [my pack fix_numneg $value]
		} elseif {$value < 128} {
		    append data [my pack fix_numpos $value]
		} elseif {$value < 0x80} {
		    # never used, fully covered by fix_numpos
		    append data [my pack fix_int8 $value]
		} elseif {$value < 0x8000} {
		    append data [my pack fix_int16 $value]
		} elseif {$value < 0x80000000} {
		    append data [my pack fix_int32 $value]
		} else {
		    append data [my pack fix_int64 $value]
		}
	    }

	    unsigned {
		# convenience function
		if {$value < 256} {
		    append data [my pack fix_uint8 $value]
		} elseif {$value < 65536} {
		    append data [my pack fix_uint16 $value]
		} elseif {$value < 4294967296} {
		    append data [my pack fix_uint32 $value]
		} else {
		    append data [my pack fix_uint64 $value]
		}
	    }

	    default {
		error "Unknown msgpack type: $type"
	    }
	}
	return
    }
}

oo::class create msgpack::unpacker {

    variable data stream callback coro

    constructor {} {
    }

    destructor {
	if {[info exists coro]} {
	    rename $coro {}
	}
    }

    method unpack_stream {istream icallback} {
	set coro ::msgpack::ups[clock milliseconds]
	set data ""
	set stream $istream
	set callback $icallback
	coroutine $coro [self] unpack_coro 0 1
	chan configure $stream -blocking 0 -buffering none -translation binary -encoding binary
	chan event $stream readable $coro
    }

    method unpack_string {idata {icallback {}}} {
	set data $idata
	set callback $icallback
	set l [my unpack_coro 0 0]
	if {[llength $callback] == 0} {
	    return $l
	}
    }

    method NeedCoro {n} {
	while {1} {
	    # Catch the [eof] and [read], socket may be close already.
	    catch {
		if {[eof $stream]} {
		    {*}$callback eof $stream
		    return -code return {}
		}
		append data [read $stream]
	    }
	    if {[string length $data] >= $n} break
	    yield
	}
    }

    method NeedString {n} {
	if {[string length $data] < $n} {
	    error "input string not long enough, need $n byte(s), only [string length $data] left"
	}
    }

    method unpack_coro {nested coro} {
	if {$coro} {
	    if {!$nested} {
		# Yield creation
		yield
	    }
	    set need_proc NeedCoro
	} else {
	    set need_proc NeedString
	}
	set l {}
	while {1} {
	    my $need_proc 1
	    binary scan $data c c
	    set tc [expr {$c & 0xFF}]
	    set data [string range $data 1 end]
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
		    lappend a {*}[my unpack_coro 1 $coro]
		    lappend a {*}[my unpack_coro 1 $coro]
		}
		lappend l [list map $a]
	    } elseif {$tc >= 0x90 && $tc <= 0x9F} {
		# FixArray
		set n [expr {$tc & 0xF}]
		set a {}
		for {set i 0} {$i < $n} {incr i} {
		    lappend a {*}[my unpack_coro 1 $coro]
		}
		lappend l [list array $a]
	    } elseif {$tc >= 0xA0 && $tc <= 0xBF} {
		# FixString
		set n [expr {$tc & 0x1F}]
		my $need_proc $n
		binary scan $data a$n c
		lappend l [list string $c]
		set data [string range $data $n end]
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
		    my $need_proc 4
		    binary scan $data R c
		    set data [string range $data 4 end]
		    lappend l [list float $c]
		} elseif {$tc == 0xCB} {
		    # double
		    my $need_proc 8
		    binary scan $data Q c
		    set data [string range $data 8 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCC} {
		    # uint8
		    my $need_proc 1
		    binary scan $data c c
		    set data [string range $data 1 end]
		    lappend l [list unsigned [expr {$c & 0xFF}]]
		} elseif {$tc == 0xCD} {
		    # uint16
		    my $need_proc 2
		    binary scan $data S c
		    set data [string range $data 2 end]
		    lappend l [list unsigned [expr {$c & 0xFFFF}]]
		} elseif {$tc == 0xCE} {
		    # uint32
		    my $need_proc 4
		    binary scan $data I c
		    set data [string range $data 4 end]
		    lappend l [list unsigned [expr {$c & 0xFFFFFFFF}]]
		} elseif {$tc == 0xCF} {
		    # uint64
		    my $need_proc 8
		    binary scan $data W c
		    set data [string range $data 8 end]
		    lappend l [list unsigned [expr {$c & 0xFFFFFFFFFFFFFFFF}]]
		} elseif {$tc == 0xD0} {
		    # int8
		    my $need_proc 1
		    binary scan $data c c
		    set data [string range $data 1 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD1} {
		    # int16
		    my $need_proc 2
		    binary scan $data S c
		    set data [string range $data 2 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD2} {
		    # int32
		    my $need_proc 4
		    binary scan $data I c
		    set data [string range $data 4 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD3} {
		    # int64
		    my $need_proc 8
		    binary scan $data W c
		    set data [string range $data 8 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD9} {
		    # string 8
		    my $need_proc 1
		    binary scan $data c n
		    set n [expr {$n & 0xFF}]
		    set data [string range $data 1 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list string $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xDA} {
		    # string 16
		    my $need_proc 2
		    binary scan $data S n
		    set n [expr {$n & 0xFFFF}]
		    set data [string range $data 2 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list string $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xDB} {
		    # string 32
		    my $need_proc 4
		    binary scan $data I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data [string range $data 4 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list string $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xC4} {
		    # bin 8
		    my $need_proc 1
		    binary scan $data c n
		    set n [expr {$n & 0xFF}]
		    set data [string range $data 1 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list bin $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xC5} {
		    # bin 16
		    my $need_proc 2
		    binary scan $data S n
		    set n [expr {$n & 0xFFFF}]
		    set data [string range $data 2 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list bin $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xC6} {
		    # bin 32
		    my $need_proc 4
		    binary scan $data I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data [string range $data 4 end]
		    my $need_proc $n
		    binary scan $data a$n c
		    lappend l [list bin $c]
		    set data [string range $data $n end]
		} elseif {$tc == 0xDC} {
		    # array 16
		    my $need_proc 2
		    binary scan $data S n
		    set n [expr {$n & 0xFFFF}]
		    set data [string range $data 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[my unpack_coro 1 $coro]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDD} {
		    # array 32
		    my $need_proc 4
		    binary scan $data I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data [string range $data 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[my unpack_coro 1 $coro]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDE} {
		    # map 16
		    my $need_proc 2
		    binary scan $data S n
		    set n [expr {$n & 0xFFFF}]
		    set data [string range $data 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[my unpack_coro 1 $coro]
			lappend a {*}[my unpack_coro 1 $coro]
		    }
		    lappend l [list map $a]
		} elseif {$tc == 0xDF} {
		    # map 32
		    my $need_proc 4
		    binary scan $data I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set data [string range $data 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[my unpack_coro 1 $coro]
			lappend a {*}[my unpack_coro 1 $coro]
		    }
		    lappend l [list map $a]
		}
	    }
	    if {$nested} {
		return $l
	    } else {
		if {[llength $callback]} {
		    foreach i $l {
			{*}$callback data $i
		    }
		    set l {}
		}
		if {!$coro} {
		    if {[string length $data] == 0} {
			return $l
		    }
		}
	    }
	}
    }
}

namespace eval msgpack {

    proc pack {l} {
	set o [msgpack::packer new]
	$o pack_list $l
	set d [$o data]
	$o destroy
	return $d
    }

    proc unpack {s} {
	set o [msgpack::unpacker new]
	set l [$o unpack_string $s]
	$o destroy
	return $l
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
