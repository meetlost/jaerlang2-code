%% My Lib Misc

-module(my_lib_misc).

-compile(export_all).

reverse_bin(Bin) ->
	L = [ X || <<X:1/binary>> <= Bin ],
	list_to_binary(lists:reverse(L)).

reverse_bits(Bits) ->
    L = [ X || <<X:1>> <= Bits ],
    << <<X:1>> || X <- lists:reverse(L) >>.

term_to_packet(Term) ->
    Bin = term_to_binary(Term),
    N = byte_size(Bin),
    << N:32, Bin/binary >>.

packet_to_term(Packet) ->
    {_, Bin} = erlang:split_binary(Packet, 4),
    erlang:binary_to_term(Bin).

test() ->
    Term = {a, b, 1, 2},
    Term = packet_to_term(term_to_packet(Term)),

    Term1 = <<3, 4, 5>>,
    Term1 = packet_to_term(term_to_packet(Term1)),

    Bin = <<3, 4, 5>>,
    Bin = reverse_bin(reverse_bin(Bin)),

    Bits = <<3, 4, 5>>,
    Bits = reverse_bits(reverse_bits(Bits)),

    ok.

test1() ->
    L = [1,2,3,4,5],
    lists:map(fun double/1, L).

double(X) ->
    2 * X.

test2() ->
    F = fun b:x/0,
    F().

mod_has_most_funs() ->
    Mods = [ Mod || {Mod, _} <- code:all_loaded() ],
    Funs = lists:map(fun(X) -> {X, length(erlang:apply(X, module_info, [exports]))} end, Mods),
    [H | _T] = lists:sort(fun({_ModA, LenA}, {_ModB, LenB}) -> LenA > LenB end, Funs),
    H.

most_common_fun() ->
    L = all_funs(),
    [H|_] = lists:sort(fun({_F_A, N_A}, {_F_B, N_B}) -> N_A > N_B end, L),
    H.

unique_funs() ->
    L = all_funs(),
    L1 = lists:sort(fun({_F_A, N_A}, {_F_B, N_B}) -> N_A < N_B end, L),
    lists:filter(fun({_, N}) -> N =:= 1 end, L1).

all_funs() ->
    Mods = [ Mod || {Mod, _} <- code:all_loaded() ],
    Funs = [ erlang:apply(X, module_info, [exports]) || X <- Mods ],
    all_funs(lists:merge(Funs)).

all_funs(L) ->
    all_funs(L, #{}).

all_funs([H|T], M) ->
    case maps:find(H, M) of
        {ok, N} -> all_funs(T, M#{ H := N+1 });
        error -> all_funs(T, M#{ H => 1 })
    end;
all_funs([], M) ->
    maps:to_list(M).
