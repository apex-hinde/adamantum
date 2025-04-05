-module(encode).
-export([encode_message/2]).

-spec encode_text_component(string()) -> binary().

encode_message(Data, Packet_name) ->
    {Packet_name, Param_list} = data_packets:get_messages_clientbound(Packet_name),
    Data2 = encode_message_list(Data, Param_list,  <<>>),
    
    Packet_ID = data_packets:get_packet_number_clientbound(Packet_name),
    Data3 = <<Packet_ID:8, Data2/binary>>,

    Data3.
encode_message_list([], _, Acc) ->
    Acc;

encode_message_list([Data|T_data], [H|T],  Acc) ->
    Result = get_encode_value(Data, H),
    encode_message_list(T_data, T, <<Acc/binary, Result/binary>>).



get_encode_value(Data, Type) ->
    case Type of 
        bool ->
            encode_bool(Data);
        byte ->
            encode_byte(Data);
        ubyte ->
            encode_ubyte(Data);
        short ->
            encode_short(Data);
        ushort ->
            encode_ushort(Data);
        int ->
            encode_int(Data);
        long ->
            encode_long(Data);
        float -> 
            encode_float(Data);
        double ->
            encode_double(Data);
        string ->
            encode_string(Data);
        text_component ->
            encode_text_component(Data);
%        json_text_component ->
%           encode_json_text_component(Data);
        identifier ->
            encode_string(Data);
        varint ->
            encode_varint(Data);
%        varlong ->
%            encode_varlong(Data);
%        entity_metadata ->
%            encode_entity_metadata(Data);
%        slot ->
%            encode_slot(Data);
        nbt ->
            nbt:encode(Data);
        position ->
            encode_position(Data);
        angle ->
            encode_byte(Data);
        uuid ->
            encode_uuid(Data);
        bitset ->
            encode_bitset(Data);
        fixed_bitset ->
            encode_fixed_bitset(Data);
        prefixed_array ->
            encode_prefixed_array(Data);
%        in_set ->
%            encode_in_set(Data);
%        sound_event ->
%            encode_sound_event(Data);
%        teleport_flags ->
%            encode_teleport_flags(Data);
%        recipe_display ->
%            encode_recipe_display(Data);
%        slot_display ->
%            encode_slot_display(Data);
%        chunk_data ->
%            encode_chunk_data(Data);
%        light_data ->
%            encode_light_data(Data)
        prefixed_array_login ->
            encode_prefixed_array_login(Data);
        boss_bar ->
            encode_boss_bar(Data)

    end.

encode_bool(Data) ->
    case Data of
        true ->
            <<1>>;
        false ->
            <<0>>
    end.

encode_byte(Data) ->
    <<Data:8/signed>>.

encode_ubyte(Data) ->
    <<Data:8/unsigned>>.

encode_short(Data) ->
    <<Data:16/signed>>.

encode_ushort(Data) ->
    <<Data:16/unsigned>>.

encode_int(Data) ->
    <<Data:32/signed>>.

encode_long(Data) ->
    <<Data:64/signed>>.

encode_float(Data) ->
    <<Data:32/float>>.

encode_double(Data) ->
    <<Data:64/float>>.

encode_string(Data) ->
    Data2 =
        case is_binary(Data) of
            true ->
                Data;
            false ->
                list_to_binary(Data)        
        end,

    Length = varint:encode_varint(byte_size(Data2)),
    <<Length/binary, Data2/binary>>.
%% currently only implemented basic text, need to add colour and such later
encode_text_component(Data) ->
    iolist_to_binary(json:encode(#{text => Data})).

encode_varint(Data) ->
    varint:encode_varint(Data).

encode_position({X,Y,Z}) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer>>.

encode_uuid(Data) ->
    Data.

encode_bitset(Data) ->
    Length = length(Data),
    <<Length_of_string/integer>> = varint:encode_varint(Length),
    Length_of_string2 = Length_of_string/8,
    <<Length_of_string2/integer, Data/binary>>.
encode_fixed_bitset(Data) ->
    Length = length(Data),
    Length_of_string = varint:encode_varint(Length),
    <<Length_of_string/binary, Data/binary>>.

encode_prefixed_array(Data) ->
    Length = length(Data),
    Length_of_string = varint:encode_varint(Length),
    <<Length_of_string/binary, Data/binary>>.
encode_prefixed_array_login({_Name, _Value}) ->
%    io:format("why of why~p~n", [{Name, Value}]),
%    Length_of_name = varint:encode_varint(byte_size(Name)),
%    Length_of_value = varint:encode_varint(byte_size(Value)),
%    Data = <<Length_of_name/binary, Name/binary, Length_of_value/binary, Value/binary, 0:8>>,
%    Length = varint:encode_varint(byte_size(Data)),
%    <<Length/binary, Data/binary>>.
    <<0:8>>.
%% boss bar is a list containing action and data, data being another list
encode_boss_bar([Action, Data]) ->
    Result = 
        case Action of
            0 -> 
                [Title, Health, Color, Division, Flags] = Data,
                <<Result1/binary>> = encode_text_component(Title),
                <<Result2/binary>> = encode_float(Health),
                <<Result3/binary>> = encode_varint(Color),
                <<Result4/binary>> = encode_varint(Division),
                <<Result5/binary>> = encode_ubyte(Flags),
                <<Result1/binary, Result2/binary, Result3/binary, Result4/binary, Result5/binary>>;
            1 ->
                <<>>;
            2 ->
                [Health] = Data,
                encode_float(Health);
            3 ->
                [Title] = Data,
                encode_text_component(Title);
            4 ->
                [Color, Dividers] = Data,
                <<Result1/binary>> = encode_varint(Color),
                <<Result2/binary>> = encode_varint(Dividers),
                <<Result1/binary, Result2/binary>>;

            5 ->
                [Flags] = Data,
                encode_ubyte(Flags)
            end,
    <<Action:8, Result/binary>>.

    

