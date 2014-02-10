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
%%%    sysctl api
%%% @end
%%% Created : 14 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-module(sysctl).

-export([get/1]).
-export([nametomib/1]).
-export([get_mib/1, set_mib/2]).

-on_load(init/0).

init() ->
    Nif = filename:join(code:priv_dir(sysctl), "sysctl_nif"),
    erlang:load_nif(Nif, 0).

get_mib(_Mib) ->
    erlang:error(nif_not_loaded).

set_mib(_Mib, _Value) ->
    erlang:error(nif_not_loaded).

get(Name) ->
    case sysctl_util:nametomib(Name) of
	{Mib,Type} ->
	    decode(Type, get_mib(Mib));
	error ->
	    error
    end.

nametomib(Name) ->
    sysctl_util:nametomib(Name).

decode(node, Bin) -> Bin;
decode(int32,  <<Int:32/little>>) -> Int;
decode(string, Bin) -> binary:bin_to_list(Bin, 0, byte_size(Bin)-1);
decode(int64, <<Int:64/little>>) -> Int;
decode(binary, Bin) -> Bin;
decode(struct, Bin) -> Bin;
decode({struct,_Struct},Bin) -> Bin.
