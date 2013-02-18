if {[info exists ::tclmsgpack] && $::tclmsgpack} {
    package provide tclmsgpack 1.0.0
    set ::tclmsgpacknamespace ::tclmsgpack
} else {
    package provide msgpack 1.0.0
    set ::tclmsgpacknamespace ::msgpack
}

namespace eval $::tclmsgpacknamespace {

    variable packerId 0
    variable namespacename $::tclmsgpacknamespace

    proc packer {} {
	variable packerId
	variable data
	variable namespacename
	set id $packerId
	incr packerId
	set data($id) ""
	set cmd ${namespacename}::msgpacker$id
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
	variable namespacename
	variable datastream
	variable datacallback
	set id $packerId
	incr packerId
	set data($id) ""
	set datastream($id) $stream
	set datacallback($id) $callback
	set cmd ${namespacename}::msgunpacker$id
	proc $cmd {cmd args} [format {
	    variable data
	    variable datastream
	    variable datacallback
	    switch -exact -- $cmd {
		readable {
		    if {[eof $datastream(%1$s)]} {
			# Maybe call callback here to signal stream is close
			{*}$datacallback(%1$s) eof $datastream(%1$s)
			unset data(%1$s)
			unset datastream(%1$s)
			unset datacallback(%1$s)
			rename %2$s {}
		    } else {
			append data(%1$s) [read $datastream(%1$s)]
			lassign [%3$s::unpack_streaming $data(%1$s)] l data(%1$s)
			foreach i $l {
			    {*}$datacallback(%1$s) data $i
			}
		    }
		}
		destroy {
		    unset data(%1$s)
		    unset datastream(%1$s)
		    unset datacallback(%1$s)
		    rename %2$s {}
		}
	    }
	} $id $cmd $namespacename]
	chan configure $stream -blocking 0 -buffering none -translation binary -encoding binary
	chan event $stream readable [list $cmd readable]
	return $cmd
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

    proc unpack_streaming {s {nested 0}} {
	set orig_s $s
	set l {}
	while {[string length $s]} {
	    binary scan $s c c
	    set tc [expr {$c & 0xFF}]
	    set s [string range $s 1 end]
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
		    lassign [unpack_streaming $s 1] al s
		    if {[llength $al] == 0} { return [list {} $orig_s] }
		    lappend a {*}$al
		    lassign [unpack_streaming $s 1] al s
		    if {[llength $al] == 0} { return [list {} $orig_s] }
		    lappend a {*}$al
		}
		lappend l [list map $a]
	    } elseif {$tc >= 0x90 && $tc <= 0x9F} {
		# FixArray
		set n [expr {$tc & 0xF}]
		set a {}
		for {set i 0} {$i < $n} {incr i} {
		    lassign [unpack_streaming $s 1] al s
		    if {[llength $al] == 0} { return [list {} $orig_s] }
		    lappend a {*}$al
		}
		lappend l [list array $a]
	    } elseif {$tc >= 0xA0 && $tc <= 0xBF} {
		# FixRaw
		set n [expr {$tc & 0xF}]
		if {[string length $s] < $n} { return [list {} $orig_s] }
		binary scan $s a$n c
		lappend l [list raw $c]
		set s [string range $s $n end]
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
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s R c
		    set s [string range $s 4 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCB} {
		    # double
		    if {[string length $s] < 8} { return [list {} $orig_s] }
		    binary scan $s Q c
		    set s [string range $s 8 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCC} {
		    # uint8
		    if {[string length $s] < 1} { return [list {} $orig_s] }
		    binary scan $s c c
		    set s [string range $s 1 end]
		    lappend l [list integer [expr {$c & 0xFF}]]
		} elseif {$tc == 0xCD} {
		    # uint16
		    if {[string length $s] < 2} { return [list {} $orig_s] }
		    binary scan $s S c
		    set s [string range $s 2 end]
		    lappend l [list integer [expr {$c & 0xFFFF}]]
		} elseif {$tc == 0xCE} {
		    # uint32
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s I c
		    set s [string range $s 4 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFF}]]
		} elseif {$tc == 0xCF} {
		    # uint64
		    if {[string length $s] < 8} { return [list {} $orig_s] }
		    binary scan $s W c
		    set s [string range $s 8 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFFFFFFFFFF}]]
		} elseif {$tc == 0xD0} {
		    # int8
		    if {[string length $s] < 1} { return [list {} $orig_s] }
		    binary scan $s c c
		    set s [string range $s 1 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD1} {
		    # int16
		    if {[string length $s] < 2} { return [list {} $orig_s] }
		    binary scan $s S c
		    set s [string range $s 2 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD2} {
		    # int32
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s I c
		    set s [string range $s 4 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD3} {
		    # int64
		    if {[string length $s] < 8} { return [list {} $orig_s] }
		    binary scan $s W c
		    set s [string range $s 8 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xDA} {
		    # raw 16
		    if {[string length $s] < 2} { return [list {} $orig_s] }
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    if {[string length $s] < $n} { return [list {} $orig_s] }
		    binary scan $s a$n c
		    lappend l [list raw $c]
		    set s [string range $s $n end]
		} elseif {$tc == 0xDB} {
		    # raw 32
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    if {[string length $s] < $n} { return [list {} $orig_s] }
		    binary scan $s a$n c
		    lappend l [list raw $c]
		    set s [string range $s $n end]
		} elseif {$tc == 0xDC} {
		    # array 16
		    if {[string length $s] < 2} { return [list {} $orig_s] }
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDD} {
		    # array 32
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDE} {
		    # map 16
		    if {[string length $s] < 2} { return [list {} $orig_s] }
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
		    }
		    lappend l [list map $a]
		} elseif {$tc == 0xDF} {
		    # map 32
		    if {[string length $s] < 4} { return [list {} $orig_s] }
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
			lassign [unpack_streaming $s 1] al s
			if {[llength $al] == 0} { return [list {} $orig_s] }
			lappend a {*}$al
		    }
		    lappend l [list map $a]
		}
	    }
	    if {$nested} {
		break
	    }
	}
	return [list $l $s]
    }

    proc unpack {snm {nested 0}} {
	if {$nested} {
	    upvar $snm s
	} else {
	    set s $snm
	}
	set l {}
	while {[string length $s]} {
	    binary scan $s c c
	    set tc [expr {$c & 0xFF}]
	    set s [string range $s 1 end]
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
		    lappend a {*}[unpack s 1]
		    lappend a {*}[unpack s 1]
		}
		lappend l [list map $a]
	    } elseif {$tc >= 0x90 && $tc <= 0x9F} {
		# FixArray
		set n [expr {$tc & 0xF}]
		set a {}
		for {set i 0} {$i < $n} {incr i} {
		    lappend a {*}[unpack s 1]
		}
		lappend l [list array $a]
	    } elseif {$tc >= 0xA0 && $tc <= 0xBF} {
		# FixRaw
		set n [expr {$tc & 0xF}]
		binary scan $s a$n c
		lappend l [list raw $c]
		set s [string range $s $n end]
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
		    binary scan $s R c
		    set s [string range $s 4 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCB} {
		    # double
		    binary scan $s Q c
		    set s [string range $s 8 end]
		    lappend l [list double $c]
		} elseif {$tc == 0xCC} {
		    # uint8
		    binary scan $s c c
		    set s [string range $s 1 end]
		    lappend l [list integer [expr {$c & 0xFF}]]
		} elseif {$tc == 0xCD} {
		    # uint16
		    binary scan $s S c
		    set s [string range $s 2 end]
		    lappend l [list integer [expr {$c & 0xFFFF}]]
		} elseif {$tc == 0xCE} {
		    # uint32
		    binary scan $s I c
		    set s [string range $s 4 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFF}]]
		} elseif {$tc == 0xCF} {
		    # uint64
		    binary scan $s W c
		    set s [string range $s 8 end]
		    lappend l [list integer [expr {$c & 0xFFFFFFFFFFFFFFFF}]]
		} elseif {$tc == 0xD0} {
		    # int8
		    binary scan $s c c
		    set s [string range $s 1 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD1} {
		    # int16
		    binary scan $s S c
		    set s [string range $s 2 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD2} {
		    # int32
		    binary scan $s I c
		    set s [string range $s 4 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xD3} {
		    # int64
		    binary scan $s W c
		    set s [string range $s 8 end]
		    lappend l [list integer $c]
		} elseif {$tc == 0xDA} {
		    # raw 16
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    binary scan $s a$n c
		    lappend l [list raw $c]
		    set s [string range $s $n end]
		} elseif {$tc == 0xDB} {
		    # raw 32
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    binary scan $s a$n c
		    lappend l [list raw $c]
		    set s [string range $s $n end]
		} elseif {$tc == 0xDC} {
		    # array 16
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack s 1]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDD} {
		    # array 32
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack s 1]
		    }
		    lappend l [list array $a]
		} elseif {$tc == 0xDE} {
		    # map 16
		    binary scan $s S n
		    set n [expr {$n & 0xFFFF}]
		    set s [string range $s 2 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack s 1]
			lappend a {*}[unpack s 1]
		    }
		    lappend l [list map $a]
		} elseif {$tc == 0xDF} {
		    # map 32
		    binary scan $s I n
		    set n [expr {$n & 0xFFFFFFFF}]
		    set s [string range $s 4 end]
		    set a {}
		    for {set i 0} {$i < $n} {incr i} {
			lappend a {*}[unpack s 1]
			lappend a {*}[unpack s 1]
		    }
		    lappend l [list map $a]
		}
	    }
	    if {$nested} {
		break
	    }
	}
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

unset ::tclmsgpacknamespace
