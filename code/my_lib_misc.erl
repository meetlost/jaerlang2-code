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
