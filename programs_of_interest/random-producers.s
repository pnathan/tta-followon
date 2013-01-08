#########################################################
# XMOS Compiled Assembly File                           #
#########################################################
#       generated: Sat Aug 13 2011, 22:51               #
#         product: XS1-040                              #
#        compiler: XMOS 32-bit XC Compiler 11.2.2 (build 1981)#
#           input: ccSavdke.xi                          #
#          output: random-producers.s                   #
#########################################################
          .file     "random-producers.xc"
          .section .netinfo,       "", @netinfo
          .int      0x1eaba15c
          .text
          .align    2

.LDBG0:
          .section  .debug_line,   "",    @progbits
.LDBG1:
          .section  .debug_frame, "",     @progbits
          .align    4
.LDBG2:
          .int      .LDBG4-.LDBG3
.LDBG3:
          .int      0xffffffff
          .byte     0x03
          .byte     0x00
          .uleb128  0x2
          .sleb128  0xfffffffc
          .uleb128  0xf
          .byte     0x0c
          .uleb128  0xe
          .uleb128  0x0
          .byte     0x07
          .uleb128  0x0
          .byte     0x07
          .uleb128  0x1
          .byte     0x07
          .uleb128  0x2
          .byte     0x07
          .uleb128  0x3
          .byte     0x08
          .uleb128  0x4
          .byte     0x08
          .uleb128  0x5
          .byte     0x08
          .uleb128  0x6
          .byte     0x08
          .uleb128  0x7
          .byte     0x08
          .uleb128  0x8
          .byte     0x08
          .uleb128  0x9
          .byte     0x08
          .uleb128  0xa
          .byte     0x07
          .uleb128  0xb
          .byte     0x08
          .uleb128  0xc
          .byte     0x08
          .uleb128  0xd
          .byte     0x08
          .uleb128  0xe
          .byte     0x08
          .uleb128  0xf
          .align    4, 0
.LDBG4:
.extern abort, "f{0}(0)"
.extern abs, "f{si}(si)"
.extern atoi, "f{si}(&(a(:c:uc)))"
.extern atol, "f{sl}(&(a(:c:uc)))"
.extern div, "f{s(){m(quot){si},m(rem){si}}}(si,si)"
.extern exit, "f{0}(si)"
.extern labs, "f{sl}(sl)"
.extern ldiv, "f{s(){m(quot){sl},m(rem){sl}}}(sl,sl)"
.extern rand, "f{si}(0)"
.extern srand, "f{0}(ui)"
.extern system, "f{si}(&(a(:c:uc)))"
.extern _Exit, "f{0}(si)"
.extern jrand48, "f{sl}(&(a(3:us)))"
.extern lcong48, "f{0}(&(a(7:us)))"
.extern lrand48, "f{sl}(0)"
.extern mrand48, "f{sl}(0)"
.extern nrand48, "f{sl}(&(a(3:us)))"
.extern srand48, "f{0}(sl)"
          .section .netinfo,       "", @netinfo
main.parinfo.debugstring0:
.asciiz "# 30 \"random-producers.xc\""
main.parinfo.debugstring1:
.asciiz "# 26 \"random-producers.xc\""
.cc_top main.parinfo.cc, main.parinfo
.globl main.parinfo, "pi"
.type  main.parinfo, @object
main.parinfo:
          .int      0x00000004
          .long __main_default_network
          .long main.parinfo.debugstring0
          .long main.parinfo.debugstring1
          .int      0x00000003
          .int      0x00000000
          .int      $N __main_xm_0
          .long stdcore
          .int      0x00000001
          .int      0x00000000
          .int      $N __main_xm_1
          .long stdcore + 4
          .int      0x00000001
          .int      0x00000001
          .int      $N __main_xm_2
          .long stdcore + 8
          .int      0x00000002
          .int      0x00000001
          .int      0x00000000
.cc_bottom main.parinfo.cc
          .text
.cc_top __main_xm_2.function,__main_xm_2
          .align    4
.LDBG5:
.globl __main_xm_2, "f{0}(chd,chd)"
.globl __main_xm_2.nstackwords
.globl __main_xm_2.maxthreads
.globl __main_xm_2.maxtimers
.globl __main_xm_2.maxchanends
.globl __main_xm_2.maxsync
.type  __main_xm_2, @function
.linkset __main_xm_2.locnoside, 0
.linkset __main_xm_2.locnochandec, 1
.linkset __main_xm_2.nstackwords, 3
.LDBG40:
.LDBG8:

################
# Entry to function `__main_xm_2' (Implied thread 0)
################
# STACK
# -------|-workspace------
# -----2-| packet
# -----1-| queue1
# -----0-| queue2
# ------------------------
# r0 = [t:7, t:8, t:9, t:10, t:11, t:12, t:13, t:14, t:15, t:16, t:17, t:18, t:19, t:20, t:21, t:22, t:23, t:24, t:25, t:26, t:27, t:28, t:29, t:30]
# r1 = [f:queue2TS, f:queue1TS]
# r4 = [t:0]
# r5 = [t:1]
# r6 = [t:2]
# r7 = [t:3]
# r8 = [t:4]
# r9 = [t:5]
# r10 = [t:6]
# Code
__main_xm_2:
          entsp     0x3              # extend sp, save lr (mem[sp]=r15)
.LDBG41:
          stw       r0, sp[0x0]      # cross-thread move to stack location
          stw       r1, sp[0x1]      # cross-thread move to stack location
          .file     1 "random-producers.xc"
          .loc      1 26 0


# random-producers.xc, line 26:
.LDBG6:
          .loc      1 64 0


# random-producers.xc, line 64:
          clre      
          ldw       r0, sp[0x1] 
          ldap      r11, .L4_Select
          setv      res[r0], r11
          ldw       r0, sp[0x1] 
          eeu       res[r0]
          ldw       r0, sp[0x0] 
          ldap      r11, .L15_Select
          setv      res[r0], r11
          ldw       r0, sp[0x0] 
          eeu       res[r0]
.xtabranch .L15_Select,.L4_Select
          waiteu    
.LDBG9:
.LDBG10:
.L4_Select:
          .loc      1 65 0


# random-producers.xc, line 65:
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          mkmsk     r1, 0x1
.L10_inlinedfunc:
.L13_bb_begin:
          .loc      1 65 0


# random-producers.xc, line 65:
          bf        r1, .L11_lastOut 
.LDBG11:
.LDBG12:
.L12_notLastOut:
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
.LDBG13:
.LDBG14:
.L11_lastOut:
          ldc       r1, 0x0
          ldw       r0, sp[0x1] 
.L27_EndPoint:
          in        r0, res[r0]      # input on `_t0_r0'
          stw       r0, sp[0x2]      # cross-thread move to stack location
          ldw       r0, sp[0x2] 
          stw       r0, sp[0x2]      # cross-thread move to stack location
          bf        r1, .L8_notLastOut 
.LDBG15:
.LDBG16:
.L7_lastOut:
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          bu        .L9_transactionDone 
.LDBG17:
.LDBG18:
.L8_notLastOut:
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
.LDBG19:
.LDBG20:
.L9_transactionDone:
          .loc      1 66 0


# random-producers.xc, line 66:
          bu        .L3_SelectNext 
.LDBG21:
.LDBG22:
.L15_Select:
          .loc      1 67 0


# random-producers.xc, line 67:
          ldw       r0, sp[0x0] 
          chkct     res[r0], 0x1 
          mkmsk     r1, 0x1
.L21_inlinedfunc:
.L24_bb_begin:
          .loc      1 67 0


# random-producers.xc, line 67:
          bf        r1, .L22_lastOut 
.LDBG23:
.LDBG24:
.L23_notLastOut:
          ldw       r0, sp[0x0] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
.LDBG25:
.LDBG26:
.L22_lastOut:
          ldc       r1, 0x0
          ldw       r0, sp[0x0] 
.L28_EndPoint:
          in        r0, res[r0]      # input on `_t0_r0'
          stw       r0, sp[0x2]      # cross-thread move to stack location
          ldw       r0, sp[0x2] 
          stw       r0, sp[0x2]      # cross-thread move to stack location
          bf        r1, .L19_notLastOut 
.LDBG27:
.LDBG28:
.L18_lastOut:
          ldw       r0, sp[0x0] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x0] 
          chkct     res[r0], 0x1 
          bu        .L20_transactionDone 
.LDBG29:
.LDBG30:
.L19_notLastOut:
          ldw       r0, sp[0x0] 
          chkct     res[r0], 0x1 
          ldw       r0, sp[0x0] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
.LDBG31:
.LDBG32:
.L20_transactionDone:
          .loc      1 68 0


# random-producers.xc, line 68:
          bu        .L3_SelectNext 
.LDBG33:
.LDBG34:
.L1_ProcBranch:
.LDBG35:
.LDBG36:
.LDBG37:
.LDBG38:
.L3_SelectNext:
.LDBG7:
          .loc      1 74 0


# random-producers.xc, line 74:
.LDBG42:
.L26_bb_begin:
          retsp     0x3              # return: dealloc and link (pc=r15=mem[sp])
.LDBG39:
.LDBG43:
.LDBG44:
.size __main_xm_2, .-__main_xm_2
.cc_bottom __main_xm_2.function
          .section  .debug_frame, "",     @progbits
.cc_top __main_xm_2.function,__main_xm_2
          .align    4
          .int      .LDBG46-.LDBG45
.LDBG45:
          .long     .LDBG2           # offset in .debug_frame
          .int      .LDBG40
          .int      .LDBG44-.LDBG40
          .byte     0x01
          .int      .LDBG41
          .byte     0x0e
          .uleb128  0xc
          .byte     0x14
          .uleb128  0xe
          .uleb128  0x0
          .byte     0x01
          .int      .LDBG42
          .byte     0x0a
          .byte     0x01
          .int      .LDBG43
          .byte     0x0b
          .align    4, 0
.LDBG46:
.cc_bottom __main_xm_2.function

# PROFILE
.linkset __main_xm_2.maxchanends, 0
.linkset __main_xm_2.maxtimers, 0
.linkset __main_xm_2.maxthreads, 1
###############

          .text
.LDBG47:
.cc_top __main_xm_1.function,__main_xm_1
          .align    4
.LDBG48:
.call __main_xm_1, srand
.globl __main_xm_1, "f{0}(chd)"
.globl __main_xm_1.nstackwords
.globl __main_xm_1.maxthreads
.globl __main_xm_1.maxtimers
.globl __main_xm_1.maxchanends
.globl __main_xm_1.maxsync
.type  __main_xm_1, @function
.linkset __main_xm_1.locnoside, 0
.linkset __main_xm_1.locnochandec, 1
.linkset .LLNK1, srand.nstackwords $M srand.nstackwords
.linkset .LLNK0, .LLNK1 + 4
.linkset __main_xm_1.nstackwords, .LLNK0
.LDBG65:
.LDBG53:

################
# Entry to function `__main_xm_1' (Implied thread 0)
################
# STACK
# -------|-workspace------
# -----3-| i
# -----2-| outpacket
# -----1-| queue2
# -----0-| rsvd
# ------------------------
# r0 = [t:7, t:8, t:9, t:10, t:11, t:12, t:14, t:15, t:16, t:17, t:19, t:20, t:21, t:22, t:23]
# r1 = [t:13, t:18]
# r4 = [t:0]
# r5 = [t:1]
# r6 = [t:2]
# r7 = [t:3]
# r8 = [t:4]
# r9 = [t:5]
# r10 = [t:6]
# Code
__main_xm_1:
          entsp     0x4              # extend sp, save lr (mem[sp]=r15)
.LDBG66:
          stw       r0, sp[0x1]      # cross-thread move to stack location
          .loc      1 26 0


# random-producers.xc, line 26:
.LDBG51:
          .loc      1 51 0


# random-producers.xc, line 51:
.L30_bb_begin:
          ldc       r0, 0xf0
          stw       r0, sp[0x2]      # cross-thread move to stack location
.L31_inlinedfunc:
          .loc      1 52 0


# random-producers.xc, line 52:
.L33_bb_begin:
          .loc      1 52 0


# random-producers.xc, line 52:
          ldc       r0, 0x0
          .loc      1 52 0


# random-producers.xc, line 52:
.L48_Call:
          bl        srand 
.L32_post_fncall:
.LDBG49:
          .loc      1 53 0


# random-producers.xc, line 53:
.L35_bb_begin:
          ldc       r0, 0x0
          stw       r0, sp[0x3]      # cross-thread move to stack location
.LDBG54:
.LDBG55:
.L37_WhileBegin:
.L44_bb_begin:
          .loc      1 53 0


# random-producers.xc, line 53:
          ldw       r1, sp[0x3] 
          mkmsk     r0, 0x4
          lss       r0, r1, r0
          bt        r0, .L38_WhileTrue 
.LDBG56:
.LDBG57:
          bu        .L36_WhileNext 
.LDBG58:
.LDBG59:
.L38_WhileTrue:
.L41_inlinedfunc:
.L42_bb_begin:
          .loc      1 55 0


# random-producers.xc, line 55:
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          ldw       r1, sp[0x1] 
          ldw       r0, sp[0x2] 
.L49_EndPoint:
          out       res[r1], r0      # output on `_t0_r1'
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          .loc      1 56 0


# random-producers.xc, line 56:
.L43_bb_begin:
          ldw       r0, sp[0x2] 
          add       r0, r0, 0x1
          stw       r0, sp[0x2]      # cross-thread move to stack location
.L39_Continue:
          .loc      1 53 0


# random-producers.xc, line 53:
.L45_bb_begin:
          ldw       r0, sp[0x3] 
          add       r0, r0, 0x1
          stw       r0, sp[0x3]      # cross-thread move to stack location
          bu        .L37_WhileBegin 
.LDBG60:
.LDBG61:
.LDBG52:
.LDBG62:
.LDBG63:
.L36_WhileNext:
.LDBG50:
          .loc      1 74 0


# random-producers.xc, line 74:
.LDBG67:
.L47_bb_begin:
          retsp     0x4              # return: dealloc and link (pc=r15=mem[sp])
.LDBG64:
.LDBG68:
.LDBG69:
.size __main_xm_1, .-__main_xm_1
.cc_bottom __main_xm_1.function
          .section  .debug_frame, "",     @progbits
.cc_top __main_xm_1.function,__main_xm_1
          .align    4
          .int      .LDBG71-.LDBG70
.LDBG70:
          .long     .LDBG2           # offset in .debug_frame
          .int      .LDBG65
          .int      .LDBG69-.LDBG65
          .byte     0x01
          .int      .LDBG66
          .byte     0x0e
          .uleb128  0x10
          .byte     0x14
          .uleb128  0xe
          .uleb128  0x0
          .byte     0x8f
          .uleb128  0x0
          .byte     0x01
          .int      .LDBG67
          .byte     0x0a
          .byte     0x01
          .int      .LDBG68
          .byte     0x0b
          .align    4, 0
.LDBG71:
.cc_bottom __main_xm_1.function

# PROFILE
.linkset __main_xm_1.maxchanends, srand.maxchanends
.linkset __main_xm_1.maxtimers, srand.maxtimers
.linkset .LLNK4, srand.maxthreads - 1
.linkset .LLNK3, 1 + .LLNK4
.linkset .LLNK2, 1 $M .LLNK3
.linkset __main_xm_1.maxthreads, .LLNK2
###############

          .text
.LDBG72:
.cc_top __main_xm_0.function,__main_xm_0
          .align    4
.LDBG73:
.call __main_xm_0, srand
.globl __main_xm_0, "f{0}(chd)"
.globl __main_xm_0.nstackwords
.globl __main_xm_0.maxthreads
.globl __main_xm_0.maxtimers
.globl __main_xm_0.maxchanends
.globl __main_xm_0.maxsync
.type  __main_xm_0, @function
.linkset __main_xm_0.locnoside, 0
.linkset __main_xm_0.locnochandec, 1
.linkset .LLNK6, srand.nstackwords $M srand.nstackwords
.linkset .LLNK5, .LLNK6 + 4
.linkset __main_xm_0.nstackwords, .LLNK5
.LDBG90:
.LDBG78:

################
# Entry to function `__main_xm_0' (Implied thread 0)
################
# STACK
# -------|-workspace------
# -----3-| i
# -----2-| outpacket
# -----1-| queue1
# -----0-| rsvd
# ------------------------
# r0 = [t:7, t:8, t:9, t:10, t:11, t:12, t:14, t:15, t:16, t:17, t:19, t:20, t:21, t:22, t:23]
# r1 = [t:13, t:18]
# r4 = [t:0]
# r5 = [t:1]
# r6 = [t:2]
# r7 = [t:3]
# r8 = [t:4]
# r9 = [t:5]
# r10 = [t:6]
# Code
__main_xm_0:
          entsp     0x4              # extend sp, save lr (mem[sp]=r15)
.LDBG91:
          stw       r0, sp[0x1]      # cross-thread move to stack location
          .loc      1 26 0


# random-producers.xc, line 26:
.LDBG76:
          .loc      1 39 0


# random-producers.xc, line 39:
.L51_bb_begin:
          ldc       r0, 0x0
          stw       r0, sp[0x2]      # cross-thread move to stack location
.L52_inlinedfunc:
          .loc      1 40 0


# random-producers.xc, line 40:
.L54_bb_begin:
          .loc      1 40 0


# random-producers.xc, line 40:
          ldc       r0, 0x0
          .loc      1 40 0


# random-producers.xc, line 40:
.L69_Call:
          bl        srand 
.L53_post_fncall:
.LDBG74:
          .loc      1 41 0


# random-producers.xc, line 41:
.L56_bb_begin:
          ldc       r0, 0x0
          stw       r0, sp[0x3]      # cross-thread move to stack location
.LDBG79:
.LDBG80:
.L58_WhileBegin:
.L65_bb_begin:
          .loc      1 41 0


# random-producers.xc, line 41:
          ldw       r1, sp[0x3] 
          mkmsk     r0, 0x4
          lss       r0, r1, r0
          bt        r0, .L59_WhileTrue 
.LDBG81:
.LDBG82:
          bu        .L57_WhileNext 
.LDBG83:
.LDBG84:
.L59_WhileTrue:
.L62_inlinedfunc:
.L63_bb_begin:
          .loc      1 43 0


# random-producers.xc, line 43:
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          ldw       r1, sp[0x1] 
          ldw       r0, sp[0x2] 
.L70_EndPoint:
          out       res[r1], r0      # output on `_t0_r1'
          ldw       r0, sp[0x1] 
          outct     res[r0], 0x1     # output ctrl token on `_t0_r0'
          ldw       r0, sp[0x1] 
          chkct     res[r0], 0x1 
          .loc      1 44 0


# random-producers.xc, line 44:
.L64_bb_begin:
          ldw       r0, sp[0x2] 
          add       r0, r0, 0x1
          stw       r0, sp[0x2]      # cross-thread move to stack location
.L60_Continue:
          .loc      1 41 0


# random-producers.xc, line 41:
.L66_bb_begin:
          ldw       r0, sp[0x3] 
          add       r0, r0, 0x1
          stw       r0, sp[0x3]      # cross-thread move to stack location
          bu        .L58_WhileBegin 
.LDBG85:
.LDBG86:
.LDBG77:
.LDBG87:
.LDBG88:
.L57_WhileNext:
.LDBG75:
          .loc      1 74 0


# random-producers.xc, line 74:
.LDBG92:
.L68_bb_begin:
          retsp     0x4              # return: dealloc and link (pc=r15=mem[sp])
.LDBG89:
.LDBG93:
.LDBG94:
.size __main_xm_0, .-__main_xm_0
.cc_bottom __main_xm_0.function
          .section  .debug_frame, "",     @progbits
.cc_top __main_xm_0.function,__main_xm_0
          .align    4
          .int      .LDBG96-.LDBG95
.LDBG95:
          .long     .LDBG2           # offset in .debug_frame
          .int      .LDBG90
          .int      .LDBG94-.LDBG90
          .byte     0x01
          .int      .LDBG91
          .byte     0x0e
          .uleb128  0x10
          .byte     0x14
          .uleb128  0xe
          .uleb128  0x0
          .byte     0x8f
          .uleb128  0x0
          .byte     0x01
          .int      .LDBG92
          .byte     0x0a
          .byte     0x01
          .int      .LDBG93
          .byte     0x0b
          .align    4, 0
.LDBG96:
.cc_bottom __main_xm_0.function

# PROFILE
.linkset __main_xm_0.maxchanends, srand.maxchanends
.linkset __main_xm_0.maxtimers, srand.maxtimers
.linkset .LLNK9, srand.maxthreads - 1
.linkset .LLNK8, 1 + .LLNK9
.linkset .LLNK7, 1 $M .LLNK8
.linkset __main_xm_0.maxthreads, .LLNK7
###############

          .text
.LDBG97:
.par srand, srand, "random-producers.xc:30: error: use of `%s' violates parallel usage rules"
# Thread names for recovering thread graph in linker
.set thread.anon.0, 0  #unreal
.set thread.anon.1, 0  #unreal
.set thread.anon.2, 0  #unreal
.set thread.anon.3, 0  #unreal
.set thread.anon.4, 0  #unreal
.set thread.anon.5, 0  #unreal
.set thread.anon.6, 0  #unreal
.LDBG98:
.extern __mb_cur_max, "si"
.extern stdcore, "a(*:cr)"
          .section  .debug_info,   "",    @progbits
.LDBG100:
          .int      .LDBG102-.LDBG101
.LDBG101:
          .short    0x0003
          .long     .LDBG99          # offset in .debug_abbrev
          .byte     0x04
          .uleb128  0x1
          .long     .LDBG0           # low address
          .long     .LDBG98          # high address
          .asciiz   "random-producers.xc"
          .asciiz   "/Users/vlion/Documents/masters/programs_of_interest"
          .short    0xc000
          .asciiz   "XMOS Dwarf Symbolic Debug Generator"
          .long     .LDBG1           # offset in .debug_lineprog
.LDBG103:
          .uleb128  0x2
          .asciiz   "long"
          .byte     0x05
          .byte     0x04
.LDBG104:
          .uleb128  0x2
          .asciiz   "unsigned long"
          .byte     0x07
          .byte     0x04
.LDBG105:
          .uleb128  0x2
          .asciiz   "int"
          .byte     0x05
          .byte     0x04
.LDBG106:
          .uleb128  0x2
          .asciiz   "unsigned int"
          .byte     0x07
          .byte     0x04
.LDBG107:
          .uleb128  0x2
          .asciiz   "short"
          .byte     0x05
          .byte     0x02
.LDBG108:
          .uleb128  0x2
          .asciiz   "unsigned short"
          .byte     0x07
          .byte     0x02
.LDBG109:
          .uleb128  0x2
          .asciiz   "char"
          .byte     0x06
          .byte     0x01
.LDBG110:
          .uleb128  0x2
          .asciiz   "unsigned char"
          .byte     0x08
          .byte     0x01
.LDBG111:
          .uleb128  0x2
          .asciiz   "chanend"
          .byte     0x07
          .byte     0x04
.LDBG112:
          .uleb128  0x2
          .asciiz   "timer"
          .byte     0x07
          .byte     0x04
.LDBG113:
          .uleb128  0x2
          .asciiz   "clock"
          .byte     0x07
          .byte     0x04
.LDBG114:
          .uleb128  0x2
          .asciiz   "port"
          .byte     0x07
          .byte     0x04
.LDBG115:
          .uleb128  0x2
          .asciiz   "buffered port:1"
          .byte     0x07
          .byte     0x04
.LDBG116:
          .uleb128  0x2
          .asciiz   "buffered port:4"
          .byte     0x07
          .byte     0x04
.LDBG117:
          .uleb128  0x2
          .asciiz   "buffered port:8"
          .byte     0x07
          .byte     0x04
.LDBG118:
          .uleb128  0x2
          .asciiz   "buffered port:16"
          .byte     0x07
          .byte     0x04
.LDBG119:
          .uleb128  0x2
          .asciiz   "buffered port:32"
          .byte     0x07
          .byte     0x04
.cc_top __main_xm_2.function,__main_xm_2
.LDBG120:
          .uleb128  0x3
          .asciiz   "__main_xm_2"
          .byte     0x01
          .byte     0x1a
          .byte     0x01
          .byte     0x01
          .long     .LDBG5           # low address
          .long     .LDBG47          # high address
          .uleb128  0x4
          .asciiz   "queue2"
          .byte     0x01
          .short    0x001e
          .int      .LDBG111-.LDBG100
          .int      .LDBG121
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_2.function,__main_xm_2
.LDBG121:
          .int      .LDBG5-.LDBG0
          .int      .LDBG47-.LDBG0
          .short    .LDBG123-.LDBG122
.LDBG122:
          .byte     0x7e
          .sleb128  0x0
.LDBG123:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_2.function
          .section  .debug_info,   "",    @progbits
          .uleb128  0x4
          .asciiz   "queue1"
          .byte     0x01
          .short    0x001e
          .int      .LDBG111-.LDBG100
          .int      .LDBG124
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_2.function,__main_xm_2
.LDBG124:
          .int      .LDBG5-.LDBG0
          .int      .LDBG47-.LDBG0
          .short    .LDBG126-.LDBG125
.LDBG125:
          .byte     0x7e
          .sleb128  0x4
.LDBG126:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_2.function
          .section  .debug_info,   "",    @progbits
.LDBG127:
          .uleb128  0x5
          .long     .LDBG6           # low address
          .long     .LDBG7           # high address
          .uleb128  0x6
          .asciiz   "packet"
          .byte     0x01
          .short    0x003f
          .short    .LDBG128-.LDBG127
          .int      .LDBG105-.LDBG100
          .int      .LDBG129
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_2.function,__main_xm_2
.LDBG129:
          .int      .LDBG6-.LDBG0
          .int      .LDBG7-.LDBG0
          .short    .LDBG131-.LDBG130
.LDBG130:
          .byte     0x7e
          .sleb128  0x8
.LDBG131:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_2.function
          .section  .debug_info,   "",    @progbits
.LDBG128:
          .byte     0x00
          .byte     0x00
.cc_bottom __main_xm_2.function
.cc_top __main_xm_1.function,__main_xm_1
.LDBG132:
          .uleb128  0x3
          .asciiz   "__main_xm_1"
          .byte     0x01
          .byte     0x1a
          .byte     0x01
          .byte     0x01
          .long     .LDBG48          # low address
          .long     .LDBG72          # high address
          .uleb128  0x4
          .asciiz   "queue2"
          .byte     0x01
          .short    0x001e
          .int      .LDBG111-.LDBG100
          .int      .LDBG133
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_1.function,__main_xm_1
.LDBG133:
          .int      .LDBG48-.LDBG0
          .int      .LDBG72-.LDBG0
          .short    .LDBG135-.LDBG134
.LDBG134:
          .byte     0x7e
          .sleb128  0x4
.LDBG135:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_1.function
          .section  .debug_info,   "",    @progbits
.LDBG136:
          .uleb128  0x5
          .long     .LDBG51          # low address
          .long     .LDBG52          # high address
          .uleb128  0x6
          .asciiz   "outpacket"
          .byte     0x01
          .short    0x0033
          .short    .LDBG137-.LDBG136
          .int      .LDBG106-.LDBG100
          .int      .LDBG138
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_1.function,__main_xm_1
.LDBG138:
          .int      .LDBG51-.LDBG0
          .int      .LDBG52-.LDBG0
          .short    .LDBG140-.LDBG139
.LDBG139:
          .byte     0x7e
          .sleb128  0x8
.LDBG140:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_1.function
          .section  .debug_info,   "",    @progbits
.LDBG137:
.LDBG141:
          .uleb128  0x5
          .long     .LDBG49          # low address
          .long     .LDBG50          # high address
          .uleb128  0x6
          .asciiz   "i"
          .byte     0x01
          .short    0x0035
          .short    .LDBG142-.LDBG141
          .int      .LDBG105-.LDBG100
          .int      .LDBG143
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_1.function,__main_xm_1
.LDBG143:
          .int      .LDBG49-.LDBG0
          .int      .LDBG50-.LDBG0
          .short    .LDBG145-.LDBG144
.LDBG144:
          .byte     0x7e
          .sleb128  0xc
.LDBG145:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_1.function
          .section  .debug_info,   "",    @progbits
.LDBG142:
          .byte     0x00
          .byte     0x00
          .byte     0x00
.cc_bottom __main_xm_1.function
.cc_top __main_xm_0.function,__main_xm_0
.LDBG146:
          .uleb128  0x3
          .asciiz   "__main_xm_0"
          .byte     0x01
          .byte     0x1a
          .byte     0x01
          .byte     0x01
          .long     .LDBG73          # low address
          .long     .LDBG97          # high address
          .uleb128  0x4
          .asciiz   "queue1"
          .byte     0x01
          .short    0x001e
          .int      .LDBG111-.LDBG100
          .int      .LDBG147
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_0.function,__main_xm_0
.LDBG147:
          .int      .LDBG73-.LDBG0
          .int      .LDBG97-.LDBG0
          .short    .LDBG149-.LDBG148
.LDBG148:
          .byte     0x7e
          .sleb128  0x4
.LDBG149:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_0.function
          .section  .debug_info,   "",    @progbits
.LDBG150:
          .uleb128  0x5
          .long     .LDBG76          # low address
          .long     .LDBG77          # high address
          .uleb128  0x6
          .asciiz   "outpacket"
          .byte     0x01
          .short    0x0027
          .short    .LDBG151-.LDBG150
          .int      .LDBG105-.LDBG100
          .int      .LDBG152
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_0.function,__main_xm_0
.LDBG152:
          .int      .LDBG76-.LDBG0
          .int      .LDBG77-.LDBG0
          .short    .LDBG154-.LDBG153
.LDBG153:
          .byte     0x7e
          .sleb128  0x8
.LDBG154:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_0.function
          .section  .debug_info,   "",    @progbits
.LDBG151:
.LDBG155:
          .uleb128  0x5
          .long     .LDBG74          # low address
          .long     .LDBG75          # high address
          .uleb128  0x6
          .asciiz   "i"
          .byte     0x01
          .short    0x0029
          .short    .LDBG156-.LDBG155
          .int      .LDBG105-.LDBG100
          .int      .LDBG157
          .section  .debug_loc,    "",    @progbits
.cc_top __main_xm_0.function,__main_xm_0
.LDBG157:
          .int      .LDBG74-.LDBG0
          .int      .LDBG75-.LDBG0
          .short    .LDBG159-.LDBG158
.LDBG158:
          .byte     0x7e
          .sleb128  0xc
.LDBG159:
          .int      0x00000000
          .int      0x00000000
.cc_bottom __main_xm_0.function
          .section  .debug_info,   "",    @progbits
.LDBG156:
          .byte     0x00
          .byte     0x00
          .byte     0x00
.cc_bottom __main_xm_0.function
          .byte     0x00
.LDBG102:
          .section  .debug_pubnames, "",  @progbits
          .int      .LDBG161-.LDBG160
.LDBG160:
          .short    0x0002
          .long     .LDBG100         # offset in .debug_info
          .int      .LDBG102-.LDBG100
.cc_top __main_xm_2.function,__main_xm_2
          .int      .LDBG120-.LDBG100
          .asciiz   "__main_xm_2"
.cc_bottom __main_xm_2.function
.cc_top __main_xm_1.function,__main_xm_1
          .int      .LDBG132-.LDBG100
          .asciiz   "__main_xm_1"
.cc_bottom __main_xm_1.function
.cc_top __main_xm_0.function,__main_xm_0
          .int      .LDBG146-.LDBG100
          .asciiz   "__main_xm_0"
.cc_bottom __main_xm_0.function
          .int      0x00000000
.LDBG161:
          .section  .debug_abbrev, "",    @progbits
.LDBG99:
          .uleb128  0x1
          .byte     0x11
          .byte     0x01
          .byte     0x11
          .byte     0x01
          .byte     0x12
          .byte     0x01
          .byte     0x03
          .byte     0x08
          .byte     0x1b
          .byte     0x08
          .byte     0x13
          .byte     0x05
          .byte     0x25
          .byte     0x08
          .byte     0x10
          .byte     0x06
          .byte     0x00
          .byte     0x00
          .uleb128  0x2
          .byte     0x24
          .byte     0x00
          .byte     0x03
          .byte     0x08
          .byte     0x3e
          .byte     0x0b
          .byte     0x0b
          .byte     0x0b
          .byte     0x00
          .byte     0x00
          .uleb128  0x3
          .byte     0x2e
          .byte     0x01
          .byte     0x03
          .byte     0x08
          .byte     0x3a
          .byte     0x0b
          .byte     0x3b
          .byte     0x0b
          .byte     0x3f
          .byte     0x0c
          .byte     0x27
          .byte     0x0c
          .byte     0x11
          .byte     0x01
          .byte     0x12
          .byte     0x01
          .byte     0x00
          .byte     0x00
          .uleb128  0x4
          .byte     0x05
          .byte     0x00
          .byte     0x03
          .byte     0x08
          .byte     0x3a
          .byte     0x0b
          .byte     0x3b
          .byte     0x05
          .byte     0x49
          .byte     0x13
          .byte     0x02
          .byte     0x06
          .byte     0x00
          .byte     0x00
          .uleb128  0x6
          .byte     0x34
          .byte     0x00
          .byte     0x03
          .byte     0x08
          .byte     0x3a
          .byte     0x0b
          .byte     0x3b
          .byte     0x05
          .byte     0x2c
          .byte     0x05
          .byte     0x49
          .byte     0x13
          .byte     0x02
          .byte     0x06
          .byte     0x00
          .byte     0x00
          .uleb128  0x5
          .byte     0x0b
          .byte     0x01
          .byte     0x11
          .byte     0x01
          .byte     0x12
          .byte     0x01
          .byte     0x00
          .byte     0x00

          .byte     0x00
          .section .xtaendpointtable,       "", @progbits
.L71_xta_begin:
          .int      .L72_xta_end-.L71_xta_begin
          .int      0x00000000
          .asciiz   "/Users/vlion/Documents/masters/programs_of_interest"
.cc_top __main_xm_0.function, __main_xm_0
          .asciiz  "random-producers.xc"
          .int      0x0000002b
          .long    .L70_EndPoint
.cc_bottom __main_xm_0.function
.cc_top __main_xm_1.function, __main_xm_1
          .asciiz  "random-producers.xc"
          .int      0x00000037
          .long    .L49_EndPoint
.cc_bottom __main_xm_1.function
.cc_top __main_xm_2.function, __main_xm_2
          .asciiz  "random-producers.xc"
          .int      0x00000043
          .long    .L28_EndPoint
.cc_bottom __main_xm_2.function
.cc_top __main_xm_2.function, __main_xm_2
          .asciiz  "random-producers.xc"
          .int      0x00000041
          .long    .L27_EndPoint
.cc_bottom __main_xm_2.function
.L72_xta_end:
          .section .xtalabeltable,       "", @progbits
.L73_xta_begin:
          .int      .L74_xta_end-.L73_xta_begin
          .int      0x00000000
          .asciiz   "/Users/vlion/Documents/masters/programs_of_interest"
.cc_top __main_xm_0.function, __main_xm_0
          .asciiz  "random-producers.xc"
          .int      0x0000004a
          .int      0x0000004a
# line info for line 74 
          .long    .L68_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000029
          .int      0x00000029
# line info for line 41 
          .long    .L66_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x0000002c
          .int      0x0000002c
# line info for line 44 
          .long    .L64_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x0000002b
          .int      0x0000002b
# line info for line 43 
          .long    .L63_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000029
          .int      0x00000029
# line info for line 41 
          .long    .L65_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000029
          .int      0x00000029
# line info for line 41 
          .long    .L56_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000028
          .int      0x00000028
# line info for line 40 
          .long    .L54_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000027
          .int      0x00000027
# line info for line 39 
          .long    .L51_bb_begin
.cc_bottom __main_xm_0.function
.cc_top __main_xm_1.function, __main_xm_1
          .asciiz  "random-producers.xc"
          .int      0x0000004a
          .int      0x0000004a
# line info for line 74 
          .long    .L47_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000035
          .int      0x00000035
# line info for line 53 
          .long    .L45_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000038
          .int      0x00000038
# line info for line 56 
          .long    .L43_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000037
          .int      0x00000037
# line info for line 55 
          .long    .L42_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000035
          .int      0x00000035
# line info for line 53 
          .long    .L44_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000035
          .int      0x00000035
# line info for line 53 
          .long    .L35_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000034
          .int      0x00000034
# line info for line 52 
          .long    .L33_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000033
          .int      0x00000033
# line info for line 51 
          .long    .L30_bb_begin
.cc_bottom __main_xm_1.function
.cc_top __main_xm_2.function, __main_xm_2
          .asciiz  "random-producers.xc"
          .int      0x0000004a
          .int      0x0000004a
# line info for line 74 
          .long    .L26_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000043
          .int      0x00000043
# line info for line 67 
          .long    .L24_bb_begin
          .asciiz  "random-producers.xc"
          .int      0x00000041
          .int      0x00000041
# line info for line 65 
          .long    .L13_bb_begin
.cc_bottom __main_xm_2.function
.L74_xta_end:
          .section .dp.data,       "adw", @progbits
.align 4
          .align    4
          .section .dp.bss,        "adw", @nobits
.align 4
          .ident    "XMOS 32-bit XC Compiler 11.2.2 (build 1981)"
          .core     "XS1"
          .corerev  "REVB"

# memory access instructions: 60
# total instructions: 126
########################################
