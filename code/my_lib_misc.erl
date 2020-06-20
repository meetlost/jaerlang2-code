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

-spec start(AnAtom, Fun) -> AnAtom when
    AnAtom :: atom(),
    Fun :: fun().

start(AnAtom, Fun) ->
    case whereis(AnAtom) of
        undefined -> register(AnAtom, spawn(Fun));
        _ -> error
    end.

-spec my_spawn(Mod, Func, Args) -> Pid when
    Mod :: module(),
    Func :: atom(),
    Args :: [term()],
    Pid :: pid().

my_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    statistics(wall_clock),
    lib_misc:on_exit(
        Pid,
        fun(Why) ->
            {_, Time} = statistics(wall_clock),
            io:format("~p died with:~p, lived ~p microseconds~n", [Pid, Why, Time * 1000])
        end
    ),
    Pid.

-spec my_spawn(Mod, Func, Args, Time) -> Pid when
    Mod :: module(),
    Func :: atom(),
    Args :: [term()],
    Time :: non_neg_integer(),
    Pid :: pid().

my_spawn(Mod, Func, Args, Time) ->
    Pid = spawn(Mod, Func, Args),
    lib_misc:on_exit(
        Pid,
        fun(Why) ->
            io:format("~p died with:~p~n", [Pid, Why])
        end
    ),
    spawn(fun() ->
        receive
        after
            Time * 1000 ->
                exit(Pid, kill)
        end
    end),
    Pid.

foo_boy() ->
    io:format("spawn foo boy~n"),
    register(foo_boy, spawn(fun() -> foo_boy_loop() end)).

foo_boy_loop() ->
    receive
    after
        5000 ->
            io:format("I'm still running~n")
    end,
    foo_boy_loop().

foo_boy_monitor() ->
    lib_misc:on_exit(whereis(foo_boy),
        fun(Why) ->
            io:format("foo_boy died with:~p~n", [Why]),
            foo_boy(),
            foo_boy_monitor()
        end
    ).

worker(Input) ->
    io:format("worker doing ~p~n~n", [Input]),
    receive
    after
        10000 ->
            worker(Input)
    end.

-spec manager_init(L, F, Tag) -> pid() when
    L :: [term()],
    F :: fun(),
    Tag :: atom().

manager_init(L, F, Tag) ->
    spawn(fun() ->
        L1 = [ {X, spawn_monitor(fun() -> F(X) end)} || X <- L ],
        manager_loop(L1, F, Tag)
    end).

-spec manager_loop(L, F, Tag) -> no_return() when
    L :: [{term(), pid()}],
    F :: fun(),
    Tag :: atom().

manager_loop(L, F, Tag) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p died with:~p~n", [{Pid, Ref}, Why]),
            case lists:keysearch({Pid, Ref}, 2, L) of
                {value, {X, {Pid, Ref}}} ->
                    case Tag of
                        one_for_one ->
                            {NewPid, NewRef} = spawn_monitor(fun() -> F(X) end),
                            L1 = lists:keyreplace({Pid, Ref}, 2, L, {X, {NewPid, NewRef}}),
                            manager_loop(L1, F, Tag);
                        one_for_all ->
                            L1 = [ {X1, spawn_monitor(fun() -> F(X1) end)} || {X1, _} <- L ],
                            [ exit(Pid1, kill) || {_, {Pid1, _}} <- L, Pid1 =/= Pid ],
                            manager_loop(L1, F, Tag)
                    end;
                false ->
                    manager_loop(L, F, Tag)
            end;
        {From, get_worker_list} ->
            From ! {self(), From, L},
            manager_loop(L, F, Tag);
        Any ->
            io:format("~p~n", [Any]),
            manager_loop(L, F, Tag)
    end.

-spec get_worker_list(M) -> L when
    M :: pid(),
    L :: [{pid(), term()}].

get_worker_list(M) ->
    Me = self(),
    M ! {Me, get_worker_list},
    receive
        {M, Me, L} ->
            L
    after
        5000 ->
            timeout
    end.

%% M = my_lib_misc:manager_init([1,2,3], fun my_lib_misc:worker/1, one_for_one).
%% my_lib_misc:get_worker_list(M).
%% exit(erlang:list_to_pid("<0.84.0>"), kill).

cpu_type() ->
    CPU_TYPE = os:cmd("uname -m"),
    CPU_TYPE -- "\n".
