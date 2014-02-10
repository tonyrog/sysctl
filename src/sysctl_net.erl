%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    sysctl net utils
%%% @end
%%% Created : 15 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-module(sysctl_net).

-export([get_statistics/0,get_statistics/1]).
-export([get_value/1]).
%% data decoding
-export([timeval/0, if_msghdr2/0, if_data64/0]).

-compile(export_all).

-include("sysctl_net.hrl").

get_statistics() ->
    get_statistics(0).

get_statistics(I) when is_integer(I) ->
    {Prefix,_} = sysctl_util:nametomib("net.route"),
    Data = sysctl:get_mib(Prefix++[0,0,?NET_RT_IFLIST2,I]),
    decode_msghdr2(Data, []);
get_statistics(Name) when is_list(Name) ->
    case interface_index(Name) of
	0 -> {error, enoent};
	I -> get_statistics(I)
    end;
get_statistics(Name) when is_atom(Name) ->
    get_statistics(atom_to_list(Name)).

get_value() ->
    get_value("*").

get_value("") ->
    get_value("*");
get_value(Counter) ->
    case string:tokens(Counter, ".") of
	[]         -> get_all_counters();
	["*"]      -> get_all_counters();
	["*","*"]    -> get_all_counters();
	[Name,"*"] -> get_all_counters(Name);
	[Name]     -> get_all_counters(Name);
	["*",Var] ->
	    Field = list_to_atom("ifi_"++Var),
	    Fi = index(Field, record_info(fields, if_data64)),
	    if Fi =:= 0 ->
		    [];
	       true ->
		    {ok,Ifs} = inet:getifaddrs(), %% find index
		    select_counters_(1, Ifs, Fi+1, Var, [])
	    end;
	[Name,Var] ->
	    Field = list_to_atom("ifi_"++Var),
	    [{_Index,S}] = get_statistics(Name),
	    %% dynamic version of #if_data64.<field>
	    case index(Field, record_info(fields, if_data64)) of
		0 -> [];
		Fi -> [{Counter,element(Fi+1, S)}]
	    end
    end.

get_all_counters() ->
    {ok,Ifs} = inet:getifaddrs(), %% find index
    get_all_counters_(1,Ifs,[]).

get_all_counters(Name) ->
    [{_Index,S}] = get_statistics(Name),
    Acc = get_counters_(Name, 2, record_info(fields, if_data64),S,[]),
    lists:reverse(Acc).

select_counters_(I,[{Name,_Addr}|As],Fi,Var,Acc) ->
    [{I,S}] = get_statistics(I),
    Value = element(Fi,S),
    select_counters_(I+1,As,Fi,Var,[{Name++"."++Var,Value}|Acc]);
select_counters_(_I, [], _Fi, _Var, Acc) ->
    lists:reverse(Acc).

get_all_counters_(I,[{Name,_Addr}|As],Acc) ->
    [{I,S}] = get_statistics(I),
    Acc1 = get_counters_(Name, 2, record_info(fields, if_data64),S, Acc),    
    get_all_counters_(I+1,As,Acc1);
get_all_counters_(_I, [], Acc) ->
    lists:reverse(Acc).

get_counters_(Name, I, [F|Fs], S, Acc) ->
    "ifi_"++Var = atom_to_list(F),
    get_counters_(Name, I+1, Fs, S, [{Name++"."++Var, element(I,S)} |Acc]);
get_counters_(_Name, _I, [], _S, Acc) ->
    Acc.

%%
%%
%%
interface_index(Name) ->
    {ok,Ifs} = inet:getifaddrs(), %% find index
    keyindex(Name, 1, Ifs).
    
%%
%% Find first Key in Position Pos in the Tuple list 
%% return the position in the list or 0 if not found
keyindex(Key, N, List) ->
    keyindex_(1, Key, N, List).

keyindex_(I,Key,N,[Tuple|_List]) when element(N,Tuple) =:= Key ->
    I;
keyindex_(I,Key,N,[_|List]) ->
    keyindex_(I+1,Key,N,List);
keyindex_(_I,_Key,_N,[]) ->
    0.

%%
%% Find first Key in Position Pos in the List 
%% return the position in the list or 0 if not found
index(Value, List) ->
    index_(1, Value, List).

index_(I,Value,[Value|_List]) ->  I;
index_(I,Value,[_|List]) -> index_(I+1,Value,List);
index_(_I,_Value,[]) -> 0.


decode_msghdr2(<<>>, Acc) ->
    lists:reverse(Acc);
decode_msghdr2(Data, Acc) ->
    {I,Data1} = sysctl_codec:decode({struct,?MODULE,if_msghdr2}, Data),
    MsgHdrLen = byte_size(Data) - byte_size(Data1),
    Len = I#if_msghdr2.ifm_msglen,
    MsgDataLen = Len - MsgHdrLen,
    <<_:MsgHdrLen/binary, Payload:MsgDataLen/binary, Data2/binary>> = Data,
    if I#if_msghdr2.ifm_type =:= ?RTM_IFINFO2 ->
	    {S,_} = sysctl_codec:decode({struct,?MODULE,if_data64},Payload),
	    Index = I#if_msghdr2.ifm_index,
	    Stat = {Index,S},
	    %% dump(Index, S),
	    decode_msghdr2(Data2, [Stat|Acc]);
       true ->
	    %% io:format("skip type = ~w, payload=~p\n", [I#if_msghdr2.ifm_type,Payload]),
	    decode_msghdr2(Data2, Acc)
    end.

dump(Index,S) ->
    io:format("interface: ~w\n", [Index]),
    show(record_info(fields, if_data64), tl(tuple_to_list(S))).

show([F|Fs], [V|Vs]) ->
    io:format("~s: ~w\n", [F, V]),
    show(Fs, Vs);
show([], []) ->
    ok.

timeval() ->
    [int32, int32].

if_data64() ->
    [uint8,uint8,uint8,uint8,
     uint8,uint8,uint8,uint8,
     uint32,
     uint32,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint64,
     uint32,
     uint32,
     {struct,?MODULE,timeval}].

if_msghdr2() ->
    [uint16,uint8,uint8,
     int32,
     int32,
     uint16,
     uint16, %% pad
     int32,
     int32,
     int32,
     int32].


