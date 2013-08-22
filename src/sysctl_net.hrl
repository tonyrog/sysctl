%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Definitions of net structs
%%% @end
%%% Created : 15 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-define(AF_UNSPEC,	0).		%% unspecified 
-define(AF_UNIX,	1).		%% local to host (pipes) 
-define(AF_INET,	2).		%% internetwork: UDP, TCP, etc. 
-define(AF_IMPLINK,	3).		%% arpanet imp addresses 
-define(AF_PUP,		4).		%% pup protocols: e.g. BSP 
-define(AF_CHAOS,	5).		%% mit CHAOS protocols 
-define(AF_NS,		6).		%% XEROX NS protocols 
-define(AF_ISO,		7).		%% ISO protocols 
-define(AF_OSI,		?AF_ISO).
-define(AF_ECMA,	8).		%% European computer manufacturers 
-define(AF_DATAKIT,	9).		%% datakit protocols 
-define(AF_CCITT,	10).		%% CCITT protocols, X.25 etc 
-define(AF_SNA,		11).		%% IBM SNA 
-define(AF_DECnet,	12).		%% DECnet 
-define(AF_DLI,		13).		%% DEC Direct data link interface 
-define(AF_LAT,		14).		%% LAT 
-define(AF_HYLINK,	15).		%% NSC Hyperchannel 
-define(AF_APPLETALK,	16).		%% Apple Talk 
-define(AF_ROUTE,	17).		%% Internal Routing Protocol 
-define(AF_LINK,	18).		%% Link layer interface 

-define(PF_UNSPEC,	?AF_UNSPEC).
-define(PF_UNIX,	?AF_UNIX).
-define(PF_INET,	?AF_INET).
-define(PF_IMPLINK,	?AF_IMPLINK).
-define(PF_PUP,		?AF_PUP).
-define(PF_CHAOS,	?AF_CHAOS).
-define(PF_NS,		?AF_NS).
-define(PF_ISO,		?AF_ISO).
-define(PF_OSI,		?AF_ISO).
-define(PF_ECMA,	?AF_ECMA).
-define(PF_DATAKIT,	?AF_DATAKIT).
-define(PF_CCITT,	?AF_CCITT).
-define(PF_SNA,		?AF_SNA).
-define(PF_DECnet,	?AF_DECnet).
-define(PF_DLI,		?AF_DLI).
-define(PF_LAT,		?AF_LAT).
-define(PF_HYLINK,	?AF_HYLINK).
-define(PF_APPLETALK,	?AF_APPLETALK).
-define(PF_ROUTE,	?AF_ROUTE).
-define(PF_LINK,       	?AF_LINK).
%%
%% Routing table
%%  [net,<pf_route>,0,<af>,<net_rt_x>,<flags> | <arg>?
%%
%% Three additional levels are defined:
%%	Fourth: <af> address family, 0 is wildcard
%%	Fifth:  <net_rt_x> type of info, defined below
%%	Sixth:  flag(s) to mask with for NET_RT_FLAGS
%%
-define(NET_RT_DUMP,		1).	%% dump; may limit to a.f. 
-define(NET_RT_FLAGS,		2).	%% by flags, e.g. RESOLVING 
-define(NET_RT_IFLIST,		3).	%% survey interface list 
-define(NET_RT_STAT,		4).	%% routing statistics 
-define(NET_RT_TRASH,		5).	%% routes not in table but not freed 
-define(NET_RT_IFLIST2,		6).	%% interface list with addresses 
-define(NET_RT_DUMP2,		7).	%% dump; may limit to a.f.

-define(RTM_NEWADDR,   12).
-define(RTM_DELADDR,   13).
-define(RTM_IFINFO,    14).
-define(RTM_IFINFO2,   18).
-define(RTM_NEWMADDR2, 19).

%% time structure
-record(timeval,
	{
	  tv_sec,
	  tv_usec
	}).

%%
%% Structure describing information about an interface
%% which may be of interest to management entities.
%%
-record(if_data64, {
	  ifi_type,		%% ethernet, tokenring, etc 
	  ifi_typelen,		%% Length of frame type id 
	  ifi_physical,		%% e.g., AUI, Thinnet, 10base-T, etc 
	  ifi_addrlen,		%% media address length 
	  ifi_hdrlen,		%% media header length 
	  ifi_recvquota,		%% polling quota for receive intrs 
	  ifi_xmitquota,		%% polling quota for xmit intrs 
	  ifi_unused1,		%% for future use 
	  ifi_mtu,		%% maximum transmission unit 
	  ifi_metric,		%% routing metric (external only) 
	  ifi_baudrate,		%% linespeed 
	  %% volatile statistics 
	  ifi_ipackets,		%% packets received on interface 
	  ifi_ierrors,		%% input errors on interface 
	  ifi_opackets,		%% packets sent on interface 
	  ifi_oerrors,		%% output errors on interface 
	  ifi_collisions,	%% collisions on csma interfaces 
	  ifi_ibytes,		%% total number of octets received 
	  ifi_obytes,		%% total number of octets sent 
	  ifi_imcasts,		%% packets received via multicast 
	  ifi_omcasts,		%% packets sent via multicast 
	  ifi_iqdrops,		%% dropped on input, this interface 
	  ifi_noproto,		%% destined for unsupported protocol 
	  ifi_recvtiming,	%% usec spent receiving when timing 
	  ifi_xmittiming,	%% usec spent xmitting when timing 
	  ifi_lastchange 	%% time of last administrative change 
	 }).

-record(if_msghdr2,
	{
	  ifm_msglen,      %% to skip over non-understood messages 
	  ifm_version,	   %% future binary compatability 
	  ifm_type,	   %% message type 
	  ifm_addrs,	   %% like rtm_addrs 
	  ifm_flags,	   %% value of if_flags 
	  ifm_index,	   %% index for associated ifp 
	  ifm_unused1,     %% padding for above field
	  ifm_snd_len,	   %% instantaneous length of send queue 
	  ifm_snd_maxlen,  %% maximum length of send queue 
	  ifm_snd_drops,   %% number of drops in send queue 
	  ifm_timer        %% time until if_watchdog called 
	}).
