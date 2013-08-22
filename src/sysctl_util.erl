-module(sysctl_util).

-export([nametomib/1]).

-include("sysctl.hrl").
-include("sysctl_net.hrl").

-record(mibnode,
	{
	  num = 0,
	  type = void,
	  comment = ""
	}).

-define(MIB2(N,T), #mibnode { num=(N), type=(T) }).
-define(MIB3(N,T,C), #mibnode { num=(N), type=T,comment=(C) }).

nametomib(Name) ->
    nametomib(root, node, string:tokens(Name, "."), []).

nametomib(Node,_Type,[N|Ns],Acc) ->
    case d(Node,N) of
	#mibnode { num=I,type={node,Node1} } ->
	    nametomib(Node1,node,Ns,[I|Acc]);
	#mibnode { num=I,type=node } ->
	    nametomib(any,node,Ns,[I|Acc]);
	#mibnode { num=0,type={nodelist,List} } ->
	    {lists:reverse(Acc), List};
	#mibnode { num=I,type=Type1 } ->
	    nametomib(none,Type1,Ns,[I|Acc])
    end;
nametomib(_Node,Type,[],Acc) ->
    {lists:reverse(Acc),Type}.


%% ROOT:
d(root, "kern") ->    ?MIB3(1, {node,kern}, "\"high kernel\": proc, limits");
d(root, "vm") ->      ?MIB3(2, {node,vm}, "virtual memory");
d(root, "vfs") ->     ?MIB3(3, {node,vfs}, "file system, mount type is next");
d(root, "net") ->     ?MIB3(4, {node,net}, "network, see socket.h");
d(root, "debug") ->   ?MIB3(5, {node,debug}, "debugging parameters");
d(root, "hw") ->      ?MIB3(6, {node,hw}, "generic cpu/io");
d(root, "machdep") -> ?MIB3(7, {node,machdep}, "machine dependent");
d(root, "user") ->    ?MIB3(8, {node,user}, "user-level");
d(root, "*") ->
    ?MIB2(0, {nodelist,["kern","vm","vfs","net","debug",
			"hw","machdep","user"]});
%% KERNEL:
d(kern, "ostype")      -> ?MIB2(1, string);
d(kern, "osrelease")   -> ?MIB2(2, string);
d(kern, "osrevision")  -> ?MIB2(3, int32);
d(kern, "version")     -> ?MIB2(4, string);
d(kern, "maxvnodes")   -> ?MIB2(5, int32);
d(kern, "maxproc")     -> ?MIB2(6, int32);
d(kern, "maxfiles")    -> ?MIB2(7, int32);
d(kern, "argmax")      -> ?MIB2(8, int32);
d(kern, "securelevel") -> ?MIB2(9, int32);
d(kern, "hostname")    -> ?MIB2(10, string);
d(kern, "hostid")      -> ?MIB2(11, int32);
d(kern, "clockrate")   -> ?MIB2(12, {struct,clockrat});
d(kern, "vnode")       -> ?MIB2(13, {struct, vnode});
d(kern, "proc")        -> ?MIB2(14, {struct, proc});
d(kern, "file")        -> ?MIB2(15, {struct, file});
d(kern, "profiling")   -> ?MIB2(16, node );
d(kern, "posix1version") -> ?MIB2(17, int32);
d(kern, "ngroups")     -> ?MIB2(18, int32);
d(kern, "job_control") -> ?MIB2(19, int32);
d(kern, "saved_ids")   -> ?MIB2(20, int32);
d(kern, "boottime")    -> ?MIB2(21, {struct, time});
d(kern, "nisdomainname") -> ?MIB2(22, string);
d(kern, "maxpartitions") -> ?MIB2(23, int32);
d(kern, "kdebug")      -> ?MIB2(24, int32);
d(kern, "update")      -> ?MIB2(25, int32);
d(kern, "osreldate")   -> ?MIB2(26, int32);
d(kern, "ntp_pll")     -> ?MIB2(27, node);
d(kern, "bootfile")    -> ?MIB2(28, string);
d(kern, "maxfilesperproc") -> ?MIB2(29, int32);
d(kern, "maxprocperuid") -> ?MIB2(30, int32);
d(kern, "dumpdev")     -> ?MIB2(31, binary);
d(kern, "ipc")         -> ?MIB2(32, node);
d(kern, "_dummy33")    -> ?MIB2(33, int32);
d(kern, "ps_strings")  -> ?MIB2(34, int32);
d(kern, "usrstack32")  -> ?MIB2(35, int32);
d(kern, "logsigexit")  -> ?MIB2(36, int32);
d(kern, "symfile")     -> ?MIB2(37, string);
d(kern, "procargs")    -> ?MIB2(38, binary);
d(kern, "_dummy39")    -> ?MIB2(39, int32); %% deprecated pcsamples 
d(kern, "netboot")     -> ?MIB2(40, int32);
d(kern, "panicinfo")   -> ?MIB2(41, node);
d(kern, "sysv")        -> ?MIB2(42, node);
d(kern, "affinity")    -> ?MIB2(43, binary);  %% unknown
d(kern, "translate")   -> ?MIB2(44, binary);  %% unknown
d(kern, "exec")        -> ?MIB2(45, node);
d(kern, "aiomax")      -> ?MIB2(46, int32);
d(kern, "aioprocmax")  -> ?MIB2(47, int32);
d(kern, "aiothreads")  -> ?MIB2(48, int32);
d(kern, "procargs2")   -> ?MIB2(49, {struct,procarg});
d(kern, "corefile")    -> ?MIB2(50, string);
d(kern, "coredump")    -> ?MIB2(51, int32);
d(kern, "sugid_coredump") -> ?MIB2(52, int32);
d(kern, "delayterm")   -> ?MIB2(53, int32);
d(kern, "shreg_private") -> ?MIB2(54, int32);
d(kern, "proc_low_pri_io") -> ?MIB2(55, int32);
d(kern, "low_pri_window") -> ?MIB2(56, int32);
d(kern, "low_pri_delay") -> ?MIB2(57, int32);
d(kern, "posix")       -> ?MIB2(58, node);
d(kern, "usrstack64")  -> ?MIB2(59, int64);
d(kern, "nx")          -> ?MIB2(60, int32);
d(kern, "tfp")         -> ?MIB2(61, node);
d(kern, "procname")    -> ?MIB2(62, string);
d(kern, "threadsigaltstack") -> ?MIB2(63, int32);
d(kern, "speculative_reads_disabled") -> ?MIB2(64, int32);
d(kern, "osversion")   -> ?MIB2(65, string);
d(kern, "safeboot")    -> ?MIB2(66, int32);
d(kern, "lctx")        -> ?MIB2(67, node);
d(kern, "rage_vnode")  -> ?MIB2(68, int32);
d(kern, "tty")         -> ?MIB2(69,node);
d(kern, "check_openevt") -> ?MIB2(70, int32);
d(kern, "thread_name") -> ?MIB2(71, string);
d(kern, "*") ->
    ?MIB2(0, {nodelist,["ostype","osrelease","osrevision","version",
			"maxvnodes","maxproc","maxfiles","argmax",
			"securelevel","hostname","hostid","clockrate",
			"vnode","proc","file","profiling",
			"posix1version","ngroups","job_control",
			"saved_ids","boottime","nisdomainname",
			"maxpartitions","kdebug","update","osreldate",
			"ntp_pll","bootfile","maxfilesperproc","maxprocperuid",
			"dumpdev","ipc","ps_strings","usrstack32","logsigexit",
			"symfile","procargs","netboot","panicinfo","sysv",
			"affinity","translate","exec","aiomax","aioprocmax",
			"aiothreads","procargs2","corefile","coredump",
			"sugid_coredump","delayterm","shreg_private",
			"proc_low_pri_io","low_pri_window","low_pri_delay",
			"posix","usrstack64","nx","tfp","procname",
			"threadsigaltstack","speculative_reads_disabled",
			"osversion","safeboot","lctx","rage_vnode",
			"tty","check_openevt","thread_name"]});
%% VFS
d(vfs, "vfsconf")     -> ?MIB2(1, {struct, vfsconf});
d(vfs, "*") -> ?MIB2(0, {nodelist,["vfsconf"]});

d(hw, "machine")      -> ?MIB2(1, string);
d(hw, "model")        -> ?MIB2(2, string);
d(hw, "ncpu")         -> ?MIB2(3, int32);
d(hw, "byteorder")    -> ?MIB2(4, int32);
d(hw, "physmem")      -> ?MIB2(5, int32);
d(hw, "usermem")      -> ?MIB2(6, int32);
d(hw, "pagesize")     -> ?MIB2(7, int32);
d(hw, "disknames")    -> ?MIB2(8, {struct,disknames});
d(hw, "diskstats")    -> ?MIB2(9, {struct,diskstats});
d(hw, "epoch")        -> ?MIB2(10, int32);
d(hw, "floatingpoint") -> ?MIB2(11, int32);
d(hw, "machinearch")  -> ?MIB2(12, string);
d(hw, "vectorunit")   -> ?MIB2(13, int32);
d(hw, "busfrequency") -> ?MIB2(14, int32);
d(hw, "cpufrequency") -> ?MIB2(15, int32);
d(hw, "cachelinesize") -> ?MIB2(16, int32);
d(hw, "l1icachesize") -> ?MIB2(17, int32);
d(hw, "l1dcachesize") -> ?MIB2(18, int32);
d(hw, "l2settings")   -> ?MIB2(19, int32);
d(hw, "l2cachesize")  -> ?MIB2(20, int32);
d(hw, "l3settings")   -> ?MIB2(21, int32);
d(hw, "l3cachesize")  -> ?MIB2(22, int32);
d(hw, "tbfrequency")  -> ?MIB2(23, int32);
d(hw, "memsize")      -> ?MIB2(24, int64);
d(hw, "availcpu")     -> ?MIB2(25, int32);
d(hw, "*") ->
    ?MIB2(0,{nodelist,
	     ["machine","model","ncpu","byteorder","physmem","usermem",
	      "pagesize","disknames","diskstats","epoch","floatingpoint",
	      "machinearch","vectorunit","busfrequency","cpufrequency",
	      "cachelinesize","l1icachesize","l1dcachesize","l2settings",
	      "l2cachesize","l3settings","l3cachesize","tbfrequency",
	      "memsize","availcpu"]});

d(user,	"cs_path") -> ?MIB2(1, string);
d(user,	"bc_base_max") -> ?MIB2(2,int32);
d(user,	"bc_dim_max") -> ?MIB2(3,int32);
d(user,	"bc_scale_max") -> ?MIB2(4,int32);
d(user,	"bc_string_max") -> ?MIB2(5,int32);
d(user,	"coll_weights_max") -> ?MIB2(6,int32);
d(user,	"expr_nest_max") -> ?MIB2(7,int32);
d(user,	"line_max") -> ?MIB2(8,int32);
d(user,	"re_dup_max") -> ?MIB2(9,int32);
d(user,	"posix2_version") -> ?MIB2(10,int32);
d(user,	"posix2_c_bind") -> ?MIB2(11,int32);
d(user,	"posix2_c_dev") -> ?MIB2(12,int32);
d(user,	"posix2_char_term") -> ?MIB2(13,int32);
d(user,	"posix2_fort_dev") -> ?MIB2(14,int32);
d(user,	"posix2_fort_run") -> ?MIB2(15,int32);
d(user,	"posix2_localedef") -> ?MIB2(16,int32);
d(user,	"posix2_sw_dev") -> ?MIB2(17,int32);
d(user,	"posix2_upe") -> ?MIB2(18,int32);
d(user,	"stream_max") -> ?MIB2(19,int32);
d(user,	"tzname_max") -> ?MIB2(20,int32);
d(user, "*") ->
    ?MIB2(0,{nodelist,["cs_path","bc_base_max","bc_dim_max","bc_scale_max","bc_string_max",
		       "coll_weights_max","expr_nest_max","line_max","re_dup_max","posix2_version",
		       "posix2_c_bind","posix2_c_dev","posix2_char_term","posix2_fort_dev", "posix2_fort_run", 
		       "posix2_localedef","posix2_sw_dev","posix2_upe","stream_max","tzname_max"]});

d(net, "unspec") -> ?MIB2(?PF_UNSPEC, node);
d(net, "unix") -> ?MIB2(?PF_UNIX, node);
d(net, "inet") -> ?MIB2(?PF_INET, node);
d(net, "implink") -> ?MIB2(?PF_IMPLINK, node);
d(net, "pup") -> ?MIB2(?PF_PUP, node);
d(net, "chaos") -> ?MIB2(?PF_CHAOS, node);
d(net, "ns") -> ?MIB2(?PF_NS, node);
d(net, "iso") -> ?MIB2(?PF_ISO, node);
d(net, "osi") -> ?MIB2(?PF_OSI, node);
d(net, "ecma") -> ?MIB2(?PF_ECMA, node);
d(net, "datakit") -> ?MIB2(?PF_DATAKIT, node);
d(net, "ccitt") -> ?MIB2(?PF_CCITT, node);
d(net, "sna") -> ?MIB2(?PF_SNA, node);
d(net, "decnet") -> ?MIB2(?PF_DECnet, node);
d(net, "dli") -> ?MIB2(?PF_DLI, node);
d(net, "lat") -> ?MIB2(?PF_LAT, node);
d(net, "hylink") -> ?MIB2(?PF_HYLINK, node);
d(net, "appletalk") -> ?MIB2(?PF_APPLETALK, node);
d(net, "route") -> ?MIB2(?PF_ROUTE, node);
d(net, "link") -> ?MIB2(?PF_LINK, node);
d(net, "*") ->
    ?MIB2(0,{nodelist,
	     ["unspec","unix","inet","implink","pup","chaos","ns",
	      "iso", "osi", "ecma", "datakit", "ccitt", "sna",
	      "decnet", "dli", "lat", "hylink", "appletalk",
	      "route", "link"]});

d(_Node, N) ->
    case string:to_integer(N) of
	{I,[]} -> ?MIB2(I, node)
    end.



	    
