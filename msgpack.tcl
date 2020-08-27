package require Tcl 8.6
package provide msgpack 2.0.0

namespace eval msgpack {}

oo::class create msgpack::packer {

    variable data

    constructor {} {
        set data ""
    }

    destructor {}

    method data {} { return $data }

    method reset {} { set data "" }

    method pack {type {value 0} {value1 ""} {value2 ""}} {
        switch -exact -- $type {
            short { append data [my pack int16 $value] }
            int { append data [my pack int32 $value] }
            long { append data [my pack int32 $value] }
            long_long { append data [my pack int64 $value] }
            unsigned_short { append data [my pack uint16 $value] }
            unsigned_int { append data [my pack uint32 $value] }
            unsigned_long { append data [my pack uint32 $value] }
            unsigned_long_long { append data [my pack uint64 $value] }
            fixnumpos { append data [binary format c [expr {$value & 0x7F}]] }
            fixnumneg { append data [binary format c [expr {($value & 0x1F) | 0xE0}]] }
            int8 {
                if {$value < -32} {
                    append data [my pack fix_int8 $value]
                } else {
                    if {$value < 0} {
                        append data [my pack fixnumneg $value]
                    } else {
                        if {$value < 128} {
                            append data [my pack fixnumpos $value]
                        } else {
                            append data [my pack fix_int8 $value]
                        }
                    }
                }
            }
            int16 {
                if {$value < -128} {
                    append data [my pack fix_int16 $value]
                } elseif {$value < 128} {
                    append data [my pack int8 $value]
                } elseif {$value < 256} {
                    append data [my pack fix_uint8 $value]
                } else {
                    append data [my pack fix_uint16 $value]
                }
            }
            int32 {
                if {$value < -32768} {
                    append data [my pack fix_int32 $value]
                } elseif {$value < 65536} {
                    append data [my pack int16 $value]
                } else {
                    append data [my pack fix_uint32 $value]
                }
            }
            int64 {
                if {$value < -2147483648} {
                    append data [my pack fix_int64 $value]
                } elseif {$value < 4294967296} {
                    append data [my pack int32 $value]
                } else {
                    append data [my pack fix_uint64 $value]
                }
            }
            uint8 {
                set value [expr {$value & 0xFF}]
                if {$value < 128} {
                    append data [my pack fixnumpos $value]
                } else {
                    append data [my pack fix_uint8 $value]
                }
            }
            uint16 {
                set value [expr {$value & 0xFFFF}]
                if {$value < 256} {
                    append data [my pack uint8 $value]
                } else {
                    append data [my pack fix_uint16 $value]
                }
            }
            uint32 {
                set value [expr {$value & 0xFFFFFFFF}]
                if {$value < 65536} {
                    append data [my pack int16 $value]
                } else {
                    append data [my pack fix_uint32 $value]
                }
            }
            uint64 {
                set value [expr {$value & 0xFFFFFFFFFFFFFFFF}]
                if {$value < 4294967296} {
                    append data [my pack int32 $value]
                } else {
                    append data [my pack fix_uint64 $value]
                }
            }
            fix_ext1 {
                # $value is the extension type and $value1 is the byte array.
                append data [binary format cca1 0xD4 \
                    [expr {$value & 0xFF}] \
                    $value1
                ]
            }
            fix_ext2 {
                append data [binary format cca2 0xD5 \
                    [expr {$value & 0xFF}] \
                    $value1
                ]
            }
            fix_ext4 {
                append data [binary format cca4 0xD6 \
                    [expr {$value & 0xFF}] \
                    $value1
                ]
            }
            fix_ext8 {
                append data [binary format cca8 0xD7 \
                    [expr {$value & 0xFF}] \
                    $value1
                ]
            }
            fix_ext16 {
                append data [binary format cca16 0xD8 \
                    [expr {$value & 0xFF}] \
                    $value1
                ]
            }
            fix_int8 { append data [binary format cc 0xD0 [expr {$value & 0xFF}]] }
            fix_int16 { append data [binary format cS 0xD1 [expr {$value & 0xFFFF}]] }
            fix_int32 { append data [binary format cI 0xD2 [expr {$value & 0xFFFFFFFF}]] }
            fix_int64 { append data [binary format cW 0xD3 [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }
            fix_uint8 { append data [binary format cc 0xCC [expr {$value & 0xFF}]] }
            fix_uint16 { append data [binary format cS 0xCD [expr {$value & 0xFFFF}]] }
            fix_uint32 { append data [binary format cI 0xCE [expr {$value & 0xFFFFFFFF}]] }
            fix_uint64 { append data [binary format cW 0xCF [expr {$value & 0xFFFFFFFFFFFFFFFF}]] }
            float32 { append data [binary format cR 0xCA $value] }
            float64 { append data [binary format cQ 0xCB $value] }
            nil { append data [binary format c 0xC0] }
            true { append data [binary format c 0xC3] }
            false { append data [binary format c 0xC2] }
            array {
                if {$value < 16} {
                    append data [binary format c [expr {0x90 | $value}]]
                } elseif {$value < 65536} {
                    append data [binary format cS 0xDC $value]
                } else {
                    append data [binary format cI 0xDD $value]
                }
            }
            list {
                set r [my pack array [llength $value1]]
                foreach e $value1 {
                    append r [my pack $value $e]
                }
                append data $r
            }
            map {
                if {$value < 16} {
                    append data [binary format c [expr {0x80 | $value}]]
                } elseif {$value < 65536} {
                    append data [binary format cS 0xDE $value]
                } else {
                    append data [binary format cI 0xDF $value]
                }
            }
            tcl_array {
                upvar $value2 a
                set r [my pack map [array size a]]
                foreach k [lsort -dictionary [array names a]] {
                    append r [my pack $value $k]
                    append r [my pack $value1 $a($k)]
                }
                append data $r
            }
            dict {
                set r [my pack map [dict size $value2]]
                dict for {k v} $value2 {
                    append r [my pack $value $k]
                    append r [my pack $value1 $v]
                }
                append data $r
            }
            bin {
                set n [string length $value]
                if {$n < 256} {
                    append data [binary format cca* 0xC4 $n $value]
                } elseif {$n < 65536} {
                    append data [binary format cSa* 0xC5 $n $value]
                } else {
                    append data [binary format cIa* 0xC6 $n $value]
                }
            }
            str {
                set value [encoding convertto utf-8 $value]
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
            ext {
                # $value is the extension type and $value1 is the byte array.
                set n [string length $value1]
                if {$n < 256} {
                    append data [binary format ccca* 0xC7 $n $value $value1]
                } elseif {$n < 65536} {
                    append data [binary format cSca* 0xC8 $n $value $value1]
                } else {
                    append data [binary format cIca* 0xC9 $n $value $value1]
                }
            }
            timestamp32 {
                append data [my pack fix_ext4 -1 [binary format I [expr {
                    $value & 0xFFFFFFFF
                }]]]
            }
            timestamp64 {
                # $value is seconds (34 bits unsigned), $value1 is nanoseconds
                # (30 bits unsigned).
                if {$value1 > 999999999} { error {nanoseconds exceed 999999999} }

                append data [my pack fix_ext8 -1 [binary format W [expr {
                    (($value1 << 34) | $value) & 0xFFFFFFFFFFFFFFFF
                }]]]
            }
            timestamp96 {
                # $value is seconds (64 bits signed), $value1 is nanoseconds
                # (32 bits unsigned)
                if {$value1 > 999999999} { error {nanoseconds exceed 999999999} }

                append data [my pack ext -1 [binary format IW [expr {
                    $value1 & 0xFFFFFFFF
                }] $value]]
            }
            milliseconds {
                append data [my pack timestamp96 [expr {
                    round($value / 1000)
                }] [expr {
                    $value % 1000 * 1000000
                }]]
            }
            microseconds {
                append data [my pack timestamp96 [expr {
                    round($value / 1000000)
                }] [expr {
                    $value % 1000000 * 1000
                }]]
            }
            default {
                error [list unknown type: $type]
            }
        }
        return
    }
}

oo::class create msgpack::unpacker {

    variable data stream callback coro ext_unpackers

    constructor {} {
        set ext_unpackers {
            -1 {apply {{type data} {
                set len [string length $data]
                if {$len == 4} {
                    binary scan $data I s
                    set s [expr { $s & 0xFFFFFFFF }]

                    return [list timestamp $s 0]
                } elseif {$len == 8} {
                    binary scan $data W t
                    set t [expr { $t & 0xFFFFFFFFFFFFFFFF }]

                    set ns [expr { $t >> 34 }]
                    set s [expr { $t & 0X00000003FFFFFFFF }]

                    return [list timestamp $s $ns]
                } elseif {$len == 12} {
                    binary scan $data IW ns s

                    set ns [expr { $ns & 0xFFFFFFFFFFFFFFFF }]
                    return [list timestamp $s $ns]
                } else {
                    error [list can't decode timestamp of length $len]
                }
            }}}
        }
    }

    destructor {
        if {[info exists coro]} {
            rename $coro {}
        }
    }

    method set_ext_unpacker args {
        lassign $args type script
        switch [llength $args] {
            0 { return $ext_unpackers }
            1 { return [dict get $ext_unpackers $type] }
            2 {
                dict set ext_unpackers $type $script
                return {}
            }
            default {
                error "wrong # args: should be
                       \"set_ext_unpacker ?type? ?script?\""
            }
        }
    }

    method unpack_ext {type data} {
        if {[dict exists $ext_unpackers $type]
            && [dict get $ext_unpackers $type] ne {}} {
            return [{*}[dict get $ext_unpackers $type] $type $data]
        }

        return [list ext $type $data]
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
                # FixStr
                set n [expr {$tc & 0x1F}]
                my $need_proc $n
                binary scan $data a$n c
                lappend l [list str [encoding convertfrom utf-8 $c]]
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
                } elseif {$tc == 0xC7} {
                    # ext 8
                    my $need_proc 2
                    binary scan $data cc n ext_type
                    set n [expr {$n & 0xFF}]
                    set data [string range $data 2 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [my unpack_ext $ext_type $c]
                    set data [string range $data $n end]
                } elseif {$tc == 0xC8} {
                    # ext 16
                    my $need_proc 3
                    binary scan $data Sc n ext_type
                    set n [expr {$n & 0xFFFF}]
                    set data [string range $data 3 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [my unpack_ext $ext_type $c]
                    set data [string range $data $n end]
                } elseif {$tc == 0xC9} {
                    # ext 32
                    my $need_proc 5
                    binary scan $data Ic n ext_type
                    set n [expr {$n & 0xFFFFFFFF}]
                    set data [string range $data 5 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [my unpack_ext $ext_type $c]
                    set data [string range $data $n end]
                 } elseif {$tc == 0xCA} {
                    # float32
                    my $need_proc 4
                    binary scan $data R c
                    set data [string range $data 4 end]
                    lappend l [list float32 $c]
                } elseif {$tc == 0xCB} {
                    # float64
                    my $need_proc 8
                    binary scan $data Q c
                    set data [string range $data 8 end]
                    lappend l [list float64 $c]
                } elseif {$tc == 0xCC} {
                    # uint8
                    my $need_proc 1
                    binary scan $data c c
                    set data [string range $data 1 end]
                    lappend l [list integer [expr {$c & 0xFF}]]
                } elseif {$tc == 0xCD} {
                    # uint16
                    my $need_proc 2
                    binary scan $data S c
                    set data [string range $data 2 end]
                    lappend l [list integer [expr {$c & 0xFFFF}]]
                } elseif {$tc == 0xCE} {
                    # uint32
                    my $need_proc 4
                    binary scan $data I c
                    set data [string range $data 4 end]
                    lappend l [list integer [expr {$c & 0xFFFFFFFF}]]
                } elseif {$tc == 0xCF} {
                    # uint64
                    my $need_proc 8
                    binary scan $data W c
                    set data [string range $data 8 end]
                    lappend l [list integer [expr {$c & 0xFFFFFFFFFFFFFFFF}]]
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
                } elseif {$tc == 0xD4} {
                    # fixext 1
                    my $need_proc 2
                    binary scan $data ca1 ext_type c
                    set data [string range $data 2 end]
                    lappend l [list ext $ext_type $c]
                } elseif {$tc == 0xD5} {
                    # fixext 2
                    my $need_proc 3
                    binary scan $data ca2 ext_type c
                    set data [string range $data 3 end]
                    lappend l [my unpack_ext $ext_type $c]
                } elseif {$tc == 0xD6} {
                    # fixext 4
                    my $need_proc 5
                    binary scan $data ca4 ext_type c
                    set data [string range $data 5 end]
                    lappend l [my unpack_ext $ext_type $c]
                } elseif {$tc == 0xD7} {
                    # fixext 8
                    my $need_proc 9
                    binary scan $data ca8 ext_type c
                    set data [string range $data 9 end]
                    lappend l [my unpack_ext $ext_type $c]
                } elseif {$tc == 0xD8} {
                    # fixext 16
                    my $need_proc 17
                    binary scan $data ca16 ext_type c
                    set data [string range $data 17 end]
                    lappend l [my unpack_ext $ext_type $c]
                } elseif {$tc == 0xD9} {
                    # string 8
                    my $need_proc 1
                    binary scan $data c n
                    set n [expr {$n & 0xFF}]
                    set data [string range $data 1 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [list str [encoding convertfrom utf-8 $c]]
                    set data [string range $data $n end]
                } elseif {$tc == 0xDA} {
                    # string 16
                    my $need_proc 2
                    binary scan $data S n
                    set n [expr {$n & 0xFFFF}]
                    set data [string range $data 2 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [list str [encoding convertfrom utf-8 $c]]
                    set data [string range $data $n end]
                } elseif {$tc == 0xDB} {
                    # string 32
                    my $need_proc 4
                    binary scan $data I n
                    set n [expr {$n & 0xFFFFFFFF}]
                    set data [string range $data 4 end]
                    my $need_proc $n
                    binary scan $data a$n c
                    lappend l [list str [encoding convertfrom utf-8 $c]]
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
                } else {
                    error [list unknown type: $tc]
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

    proc pack {type {value 0} {value1 ""} {value2 ""}} {
        set o [msgpack::packer new]
        if {$type eq "tcl_array"} {
            upvar $value2 a
            $o pack $type $value $value1 a
        } else {
            $o pack $type $value $value1 $value2
        }
        set s [$o data]
        $o destroy
        return $s
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
