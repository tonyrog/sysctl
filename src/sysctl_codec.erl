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
%%%    Simple native data decode
%%% @end
%%% Created : 15 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-module(sysctl_codec).

-export([decode/2]).

decode(uint8, <<V:8,Cs/binary>>) -> {V,Cs};
decode(uint16, <<V:16/unsigned-native,Cs/binary>>) -> {V,Cs};
decode(uint32, <<V:32/unsigned-native,Cs/binary>>) -> {V,Cs};
decode(uint64, <<V:64/unsigned-native,Cs/binary>>) -> {V,Cs};
decode(int8, <<V:8/signed,Cs/binary>>) -> {V,Cs};
decode(int16, <<V:16/signed-native,Cs/binary>>) -> {V,Cs};
decode(int32, <<V:32/signed-native,Cs/binary>>) -> {V,Cs};
decode(int64, <<V:64/signed-native,Cs/binary>>) -> {V,Cs};
decode({struct,Mod,Rec}, Cs) ->
    Es1 = apply(Mod, Rec, []),
    {Fs,Cs1} = decode_list(Es1,Cs),
    {list_to_tuple([Rec|Fs]), Cs1}.

decode_list(Es,Cs) ->
    decode_list(Es,Cs,[]).

decode_list([E|Es],Cs,Vs) ->
    {V,Cs1} = decode(E, Cs),
    decode_list(Es,Cs1,[V|Vs]);
decode_list([],Cs,Vs) ->
    {lists:reverse(Vs), Cs}.
