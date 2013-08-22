%%
%% Definitions of MIB constants 
%% 
-ifndef(__SYSCTL_HRL__).
-define(__SYSCTL_HRL__, true).

%% Level 0
-define(CTL_KERN,	1).
-define(CTL_VM,		2).
-define(CTL_VFS,	3).
-define(CTL_NET,	4).
-define(CTL_DEBUG,	5).
-define(CTL_HW,		6).
-define(CTL_MACHDEP,	7).
-define(CTL_USER,	8).

%% Level1: CTL_KERN identifiers
-define(KERN_OSTYPE,	 	 1).	%% string: system version 
-define(KERN_OSRELEASE,	 	 2).	%% string: system release 
-define(KERN_OSREV,	 	 3).	%% int: system revision 
-define(KERN_VERSION,	 	 4).	%% string: compile time info 
-define(KERN_MAXVNODES,	 	 5).	%% int: max vnodes 
-define(KERN_MAXPROC,	 	 6).	%% int: max processes 
-define(KERN_MAXFILES,	 	 7).	%% int: max open files 
-define(KERN_ARGMAX,	 	 8).	%% int: max arguments to exec 
-define(KERN_SECURELVL,	 	 9).	%% int: system security level 
-define(KERN_HOSTNAME,		10).	%% string: hostname 
-define(KERN_HOSTID,		11).	%% int: host identifier 
-define(KERN_CLOCKRATE,		12).	%% struct: struct clockrate 
-define(KERN_VNODE,		13).	%% struct: vnode structures 
-define(KERN_PROC,		14).	%% struct: process entries 
-define(KERN_FILE,		15).	%% struct: file entries 
-define(KERN_PROF,		16).	%% node: kernel profiling info 
-define(KERN_POSIX1,		17).	%% int: POSIX.1 version 
-define(KERN_NGROUPS,		18).	%% int: # of supplemental group ids 
-define(KERN_JOB_CONTROL,	19).	%% int: is job control available 
-define(KERN_SAVED_IDS,		20).	%% int: saved set-user/group-ID 
-define(KERN_BOOTTIME,		21).	%% struct: time kernel was booted 
-define(KERN_NISDOMAINNAME,	22).	%% string: YP domain name 
-define(KERN_DOMAINNAME,	?KERN_NISDOMAINNAME).
-define(KERN_MAXPARTITIONS,	23).	%% int: number of partitions/disk 
-define(KERN_KDEBUG,		24).	%% int: kernel trace points 
-define(KERN_UPDATEINTERVAL,	25).	%% int: update process sleep time 
-define(KERN_OSRELDATE,		26).	%% int: OS release date 
-define(KERN_NTP_PLL,		27).	%% node: NTP PLL control 
-define(KERN_BOOTFILE,		28).	%% string: name of booted kernel 
-define(KERN_MAXFILESPERPROC,	29).	%% int: max open files per proc 
-define(KERN_MAXPROCPERUID, 	30).	%% int: max processes per uid 
-define(KERN_DUMPDEV,		31).	%% dev_t: device to dump on 
-define(KERN_IPC,		32).	%% node: anything related to IPC 
-define(KERN_DUMMY33,		33).	%% unused 
-define(KERN_PS_STRINGS,        34).	%% int: address of PS_STRINGS 
-define(KERN_USRSTACK32,        35).	%% int: address of USRSTACK 
-define(KERN_LOGSIGEXIT,        36).	%% int: do we log sigexit procs? 
-define(KERN_SYMFILE,		37).	%% string: kernel symbol filename 
-define(KERN_PROCARGS,		38).
                             %% 39 was KERN_PCSAMPLES... now deprecated 
-define(KERN_NETBOOT,		40).	%% int: are we netbooted? 1=yes,0=no 
-define(KERN_PANICINFO,		41).	%% node: panic UI information 
-define(KERN_SYSV,		42).	%% node: System V IPC information 
-define(KERN_AFFINITY,		43).	%% xxx 
-define(KERN_TRANSLATE,	   	44).	%% xxx 
-define(KERN_CLASSIC,	   	?KERN_TRANSLATE).  %% XXX backwards compat 
-define(KERN_EXEC,		45).	%% xxx 
-define(KERN_CLASSICHANDLER,	?KERN_EXEC). %% XXX backwards compatibility 
-define(KERN_AIOMAX,		46).	%% int: max aio requests 
-define(KERN_AIOPROCMAX,	47).	%% int: max aio requests per process 
-define(KERN_AIOTHREADS,	48).	%% int: max aio worker threads 
-ifdef(__APPLE_API_UNSTABLE).
-define(KERN_PROCARGS2,		49).
-endif.
-define(KERN_COREFILE,		50).	%% string: corefile format string 
-define(KERN_COREDUMP,		51).	%% int: whether to coredump at all 
-define(KERN_SUGID_COREDUMP,	52).	%% int: whether to dump SUGID cores 
-define(KERN_PROCDELAYTERM,	53).	%% int: set/reset current proc for delayed termination during shutdown 
-define(KERN_SHREG_PRIVATIZABLE,54).	%% int: can shared regions be privatized ? 
                             %% 55 was KERN_PROC_LOW_PRI_IO... now deprecated 
-define(KERN_LOW_PRI_WINDOW,	56).	%% int: set/reset throttle window - milliseconds 
-define(KERN_LOW_PRI_DELAY,	57).	%% int: set/reset throttle delay - milliseconds 
-define(KERN_POSIX,		58).	%% node: posix tunables 
-define(KERN_USRSTACK64,	59).	%% LP64 user stack query 
-define(KERN_NX_PROTECTION,	60).	%% int: whether no-execute protection is enabled 
-define(KERN_TFP,		61).	%% Task for pid settings 
-define(KERN_PROCNAME, 		62).	%% setup process program  name(2*MAXCOMLEN) 
-define(KERN_THALTSTACK,	63).	%% for compat with older x86 and does nothing 
-define(KERN_SPECULATIVE_READS,	64).	%% int: whether speculative reads are disabled 
-define(KERN_OSVERSION,		65).	%% for build number i.e. 9A127 
-define(KERN_SAFEBOOT,		66).	%% are we booted safe? 
-define(KERN_LCTX,		67).	%% node: login context 
-define(KERN_RAGEVNODE,		68).
-define(KERN_TTY,		69).	%% node: tty settings 
-define(KERN_CHECKOPENEVT,      70).    %% spi: check the VOPENEVT flag on vnodes at open time 
-define(KERN_THREADNAME,	71).	%% set/get thread name 

-ifdef(__LP64__).
-define(KERN_USRSTACK, ?KERN_USRSTACK64).
-else.
-define(KERN_USRSTACK, ?KERN_USRSTACK32).
-endif.

%% KERN_RAGEVNODE types 
-define(KERN_RAGE_PROC,		1).
-define(KERN_RAGE_THREAD,	2).
-define(KERN_UNRAGE_PROC,	3).
-define(KERN_UNRAGE_THREAD,	4).

%% KERN_OPENEVT types 
-define(KERN_OPENEVT_PROC,     1).
-define(KERN_UNOPENEVT_PROC,   2).

%% KERN_TFP types 
-define(KERN_TFP_POLICY,		1).

%% KERN_TFP_POLICY values . All policies allow task port for self 
-define(KERN_TFP_POLICY_DENY, 		0). 	%% Deny Mode: None allowed except privileged 
-define(KERN_TFP_POLICY_DEFAULT, 	2).	%% Default  Mode: related ones allowed and upcall authentication 

%% KERN_KDEBUG types 
-define(KERN_KDEFLAGS,		1).
-define(KERN_KDDFLAGS,		2).
-define(KERN_KDENABLE,		3).
-define(KERN_KDSETBUF,		4).
-define(KERN_KDGETBUF,		5).
-define(KERN_KDSETUP,		6).
-define(KERN_KDREMOVE,		7).
-define(KERN_KDSETREG,		8).
-define(KERN_KDGETREG,		9).
-define(KERN_KDREADTR,		10).
-define(KERN_KDPIDTR,           11).
-define(KERN_KDTHRMAP,          12).
%% Don't use 13 as it is overloaded with KERN_VNODE 
-define(KERN_KDPIDEX,            14).
-define(KERN_KDSETRTCDEC,        15).
-define(KERN_KDGETENTROPY,       16).
-define(KERN_KDWRITETR,		17).
-define(KERN_KDWRITEMAP,		18).
-define(KERN_KDENABLE_BG_TRACE,	19).
-define(KERN_KDDISABLE_BG_TRACE,	20).
-define(KERN_KDSET_TYPEFILTER,   22).

%% KERN_PANICINFO types 
-define(KERN_PANICINFO_MAXSIZE,	1).	%% quad: panic UI image size limit 
-define(KERN_PANICINFO_IMAGE,	2).	%% panic UI in 8-bit kraw format 
-define(KERN_PANICINFO_TEST, 	4).	%% Allow the panic UI to be tested by root without causing a panic 
-define(KERN_PANICINFO_NOROOT_TEST, 5).	%% Allow the noroot UI to be tested by root 

%% 
%% KERN_PROC subtypes
%% 
-define(KERN_PROC_ALL,		0).	%% everything 
-define(KERN_PROC_PID,		1).	%% by process id 
-define(KERN_PROC_PGRP,		2).	%% by process group id 
-define(KERN_PROC_SESSION,	3).	%% by session of pid 
-define(KERN_PROC_TTY,		4).	%% by controlling tty 
-define(KERN_PROC_UID,		5).	%% by effective uid 
-define(KERN_PROC_RUID,		6).	%% by real uid 
-define(KERN_PROC_LCID,		7).	%% by login context id 

%%
%% KERN_LCTX subtypes
 
-define(KERN_LCTX_ALL,		0).	%% everything 
-define(KERN_LCTX_LCID,		1).	%% by login context id 

%% CTL_HW identifiers

-define(HW_MACHINE,	 1).		%% string: machine class 
-define(HW_MODEL,	 2).		%% string: specific machine model 
-define(HW_NCPU,	 3).		%% int: number of cpus 
-define(HW_BYTEORDER,	 4).		%% int: machine byte order 
-define(HW_PHYSMEM,	 5).		%% int: total memory 
-define(HW_USERMEM,	 6).		%% int: non-kernel memory 
-define(HW_PAGESIZE,	 7).		%% int: software page size 
-define(HW_DISKNAMES,	 8).		%% strings: disk drive names 
-define(HW_DISKSTATS,	 9).		%% struct: diskstats[] 
-define(HW_EPOCH,  	10).		%% int: 0 for Legacy, else NewWorld 
-define(HW_FLOATINGPT,	11).		%% int: has HW floating point? 
-define(HW_MACHINE_ARCH,12).		%% string: machine architecture 
-define(HW_VECTORUNIT,	13).		%% int: has HW vector unit? 
-define(HW_BUS_FREQ,	14).		%% int: Bus Frequency 
-define(HW_CPU_FREQ,	15).		%% int: CPU Frequency 
-define(HW_CACHELINE,	16).		%% int: Cache Line Size in Bytes 
-define(HW_L1ICACHESIZE,17).		%% int: L1 I Cache Size in Bytes 
-define(HW_L1DCACHESIZE,18).		%% int: L1 D Cache Size in Bytes 
-define(HW_L2SETTINGS,	19).		%% int: L2 Cache Settings 
-define(HW_L2CACHESIZE,	20).		%% int: L2 Cache Size in Bytes 
-define(HW_L3SETTINGS,	21).		%% int: L3 Cache Settings 
-define(HW_L3CACHESIZE,	22).		%% int: L3 Cache Size in Bytes 
-define(HW_TB_FREQ,	23).		%% int: Bus Frequency 
-define(HW_MEMSIZE,	24).		%% uint64_t: physical ram size 
-define(HW_AVAILCPU,	25).		%% int: number of available CPUs 

-endif.
