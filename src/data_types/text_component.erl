-module(text_component).

-export([
	 decode/1,
	 encode/1,
	 normalize/1,
	 to_snbt/1,
	 from_snbt/1
	]).


-spec decode(term()) -> map().
decode(Data) when is_binary(Data); is_list(Data) ->
    case is_string_or_binary(Data) of
        true ->
            Tags = snbt:decode(Data),
            normalize(Tags);
        false ->
            normalize(Data)
    end;
decode(Tags) ->
    normalize(Tags).

-spec encode(term()) -> binary().
encode(Data) ->
    to_snbt(Data).

-spec from_snbt(iodata()) -> map().
from_snbt(SNBT) ->
    Tags = snbt:decode(SNBT),
    normalize(Tags).

-spec to_snbt(term()) -> binary().
to_snbt(Map) when is_map(Map) ->
    NbtTags = to_nbt_tags(Map),
    snbt:encode(NbtTags);
to_snbt(Tags) when is_list(Tags) ->
    snbt:encode(Tags);
to_snbt(Str) when is_binary(Str) ->
    Str.

-spec normalize(term()) -> map().
normalize([{tag_string, _Name, Str}]) ->
    normalize_string_shorthand(Str);
normalize([{tag_list, _Name, Items}]) ->
    normalize_list_shorthand(Items);
normalize([{tag_compound, _Name, Fields}]) ->
    normalize_compound_fields(Fields);
normalize({tag_string, _Name, Str}) ->
    normalize_string_shorthand(Str);
normalize({tag_string, Str}) ->
    normalize_string_shorthand(Str);
normalize({tag_compound, _Name, Fields}) ->
    normalize_compound_fields(Fields);
normalize({tag_compound, Fields}) ->
    normalize_compound_fields(Fields);
normalize(Str) when is_binary(Str) ->
    normalize_string_shorthand(Str);
normalize(List) when is_list(List) ->
    case is_nbt_field_list(List) of
        true ->
            normalize_compound_fields(List);
        false ->
            case is_string_or_binary(List) of
                true  -> normalize_string_shorthand(List);
                false -> normalize_list_shorthand(List)
            end
    end;
normalize(Map) when is_map(Map) ->
    normalize_map(Map);
normalize(Term) ->
    #{type => <<"text">>, text => to_binary(Term)}.

is_nbt_field_list([{_Key, {TagType, _Val}} | _]) when is_atom(TagType) ->
    is_nbt_tag_type(TagType);
is_nbt_field_list([{TagType, _Key, _Val} | _]) when is_atom(TagType) ->
    is_nbt_tag_type(TagType);
is_nbt_field_list(_) ->
    false.

is_nbt_tag_type(tag_byte) -> true;
is_nbt_tag_type(tag_short) -> true;
is_nbt_tag_type(tag_int) -> true;
is_nbt_tag_type(tag_long) -> true;
is_nbt_tag_type(tag_float) -> true;
is_nbt_tag_type(tag_double) -> true;
is_nbt_tag_type(tag_byte_array) -> true;
is_nbt_tag_type(tag_string) -> true;
is_nbt_tag_type(tag_list) -> true;
is_nbt_tag_type(tag_compound) -> true;
is_nbt_tag_type(tag_int_array) -> true;
is_nbt_tag_type(tag_long_array) -> true;
is_nbt_tag_type(_) -> false.

normalize_string_shorthand(Str) ->
    #{type => <<"text">>, text => to_binary(Str)}.

normalize_list_shorthand([]) ->
    #{type => <<"text">>, text => <<"">>};
normalize_list_shorthand([First | Rest]) ->
    Base = normalize(First),
    case Rest of
        [] ->
            Base;
        _ ->
            Extra = [normalize(Item) || Item <- Rest],
            ExistingExtra = maps:get(extra, Base, []),
            Base#{extra => ExistingExtra ++ Extra}
    end.

normalize_compound_fields(Fields) ->
    RawMap = lists:foldl(
	       fun
		   ({Key, {tag_string, Val}}, Acc) when is_list(Key); is_binary(Key); is_atom(Key) ->
				Acc#{to_atom(Key) => to_binary(Val)};
		   ({Key, {_TagType, Val}}, Acc) when is_list(Key); is_binary(Key); is_atom(Key) ->
				Acc#{to_atom(Key) => Val};
		   ({tag_string, Key, Val}, Acc) ->
				Acc#{to_atom(Key) => to_binary(Val)};
		   ({_TagType, Key, Val}, Acc) when is_list(Key); is_binary(Key); is_atom(Key) ->
				Acc#{to_atom(Key) => Val};
		   ({TagType, Val}, Acc) ->
				Acc#{to_atom(TagType) => Val}
			end,
	       #{},
	       Fields
	      ),
    normalize_map(RawMap).

normalize_map(Map) ->
    AtomMap = maps:fold(fun(K, V, Acc) -> Acc#{to_atom(K) => V} end, #{}, Map),

    Type = determine_type(AtomMap),
    Map1 = AtomMap#{type => Type},

    Map2 = process_content_fields(Type, Map1),

    Map3 = process_formatting_fields(Map2),

    Map4 = process_interactivity(Map3),

    process_extra(Map4).


determine_type(#{type := Type}) ->
    BinType = to_binary(Type),
    case is_valid_type(BinType) of
        true  -> BinType;
        false -> infer_type(Type)
    end;
determine_type(Map) ->
    infer_type(Map).

is_valid_type(<<"text">>) -> true;
is_valid_type(<<"translatable">>) -> true;
is_valid_type(<<"score">>) -> true;
is_valid_type(<<"selector">>) -> true;
is_valid_type(<<"keybind">>) -> true;
is_valid_type(<<"nbt">>) -> true;
is_valid_type(<<"object">>) -> true;
is_valid_type(_) -> false.

infer_type(Map) when is_map(Map) ->
    if
        is_map_key(text, Map) -> <<"text">>;
        is_map_key(translate, Map) -> <<"translatable">>;
        is_map_key(score, Map) -> <<"score">>;
        is_map_key(selector, Map) -> <<"selector">>;
        is_map_key(keybind, Map) -> <<"keybind">>;
        is_map_key(nbt, Map) -> <<"nbt">>;
        is_map_key(object, Map) orelse is_map_key(sprite, Map) orelse is_map_key(player, Map) -> <<"object">>;
        true -> <<"text">>
    end;
infer_type(_) ->
    <<"text">>.


process_content_fields(<<"text">>, Map) ->
    Text = maps:get(text, Map, <<"">>),
    Map#{text => to_binary(Text)};
process_content_fields(<<"translatable">>, Map) ->
    Translate = to_binary(maps:get(translate, Map, <<"">>)),
    M1 = Map#{translate => Translate},
    M2 = case M1 of
	     #{fallback := FB} ->
		 M1#{fallback => to_binary(FB)};
	     #{} ->
		 M1
	 end,
    case M2 of
        #{with := WithList} when is_list(WithList) ->
            M2#{with => [normalize(Item) || Item <- WithList]};
        #{} ->
            M2
    end;
process_content_fields(<<"score">>, Map) ->
    Score = maps:get(score, Map, #{}),
    ScoreMap = case is_map(Score) of
		   true -> Score;
		   false -> normalize_compound_fields(Score)
	       end,
    Name = to_binary(maps:get(name, ScoreMap, <<"">>)),
    Obj = to_binary(maps:get(objective, ScoreMap, <<"">>)),
    Map#{score => #{name => Name, objective => Obj}};
process_content_fields(<<"selector">>, Map) ->
    Selector = to_binary(maps:get(selector, Map, <<"">>)),
    M1 = Map#{selector => Selector},
    case M1 of
        #{separator := Sep} ->
            M1#{separator => normalize(Sep)};
        #{} ->
            M1
    end;
process_content_fields(<<"keybind">>, Map) ->
    Keybind = to_binary(maps:get(keybind, Map, <<"">>)),
    Map#{keybind => Keybind};
process_content_fields(<<"nbt">>, Map) ->
    NBTPath = to_binary(maps:get(nbt, Map, <<"">>)),
    M1 = Map#{nbt => NBTPath},
    M2 = case M1 of
	     #{source := Src} ->
		 M1#{source => to_binary(Src)};
	     #{} ->
		 M1
	 end,
    M3 = M2#{
	     interpret => to_bool(maps:get(interpret, M2, false)),
	     plain     => to_bool(maps:get(plain, M2, false))
	    },
    M4 = case M3 of
	     #{separator := Sep} ->
		 M3#{separator => normalize(Sep)};
	     #{} ->
		 M3
	 end,
    M5 = case M4 of
	     #{block := B} ->
		 M4#{block => to_binary(B)};
	     #{} ->
		 M4
	 end,
    M6 = case M5 of
	     #{entity := E} ->
		 M5#{entity => to_binary(E)};
	     #{} ->
		 M5
	 end,
    case M6 of
        #{storage := S} ->
            M6#{storage => to_binary(S)};
        #{} ->
            M6
    end;
process_content_fields(<<"object">>, Map) ->
    ObjType = to_binary(maps:get(object, Map, <<"atlas">>)),
    M1 = Map#{object => ObjType},
    case ObjType of
        <<"atlas">> ->
            Atlas = to_binary(maps:get(atlas, M1, <<"minecraft:blocks">>)),
            Sprite = to_binary(maps:get(sprite, M1, <<"">>)),
            M1#{atlas => Atlas, sprite => Sprite};
        <<"player">> ->
            Player = maps:get(player, M1, <<"">>),
            Hat = to_bool(maps:get(hat, M1, true)),
            M1#{player => normalize_player_profile(Player), hat => Hat};
        _ ->
            M1
    end;
process_content_fields(_, Map) ->
    Map.

normalize_player_profile(P) when is_binary(P); is_list(P) ->
    to_binary(P);
normalize_player_profile(P) when is_map(P) ->
    P;
normalize_player_profile(Fields) when is_list(Fields) ->
    normalize_compound_fields(Fields).


process_formatting_fields(Map) ->
    M1 = case Map of
	     #{color := C} ->
		 Map#{color => to_binary(C)};
	     #{} ->
		 Map
	 end,
    M2 = case M1 of
	     #{font := F} ->
		 M1#{font => to_binary(F)};
	     #{} ->
		 M1
	 end,
    M3 = process_bool_formatting(bold, M2),
    M4 = process_bool_formatting(italic, M3),
    M5 = process_bool_formatting(underlined, M4),
    M6 = process_bool_formatting(strikethrough, M5),
    M7 = process_bool_formatting(obfuscated, M6),
    process_shadow_color(M7).

process_bool_formatting(Key, Map) ->
    case Map of
        #{Key := Val} ->
            Map#{Key => to_bool(Val)};
        #{} ->
            Map
    end.

process_shadow_color(Map) ->
    case maps:find(shadow_color, Map) of
        {ok, FloatList} when is_list(FloatList), length(FloatList) =:= 4 ->
            Floats = [to_float_val(X) || X <- FloatList],
            [R, G, B, A] = Floats,
            IntColor = float_rgba_to_argb_int(R, G, B, A),
            Map#{shadow_color => IntColor};
        {ok, IntVal} when is_integer(IntVal) ->
            Map#{shadow_color => IntVal};
        error ->
            Map
    end.

to_float_val({tag_float, F}) -> float(F);
to_float_val({tag_double, F}) -> float(F);
to_float_val({tag_int, I}) -> float(I);
to_float_val(F) when is_float(F) -> F;
to_float_val(I) when is_integer(I) -> float(I).

float_rgba_to_argb_int(R, G, B, A) ->
    RInt = trunc(R * 255) band 16#FF,
    GInt = trunc(G * 255) band 16#FF,
    BInt = trunc(B * 255) band 16#FF,
    AInt = trunc(A * 255) band 16#FF,
    (AInt bsl 24) + (RInt bsl 16) + (GInt bsl 8) + BInt.

process_interactivity(Map) ->
    M1 = case Map of
	     #{insertion := Ins} ->
		 Map#{insertion => to_binary(Ins)};
	     #{} ->
		 Map
	 end,
    M2 = case M1 of
	     #{click_event := CE} ->
		 M1#{click_event => process_click_event(CE)};
	     #{} ->
		 M1
	 end,
    case M2 of
        #{hover_event := HE} ->
            M2#{hover_event => process_hover_event(HE)};
        #{} ->
            M2
    end.

process_click_event(CE) when is_list(CE) ->
    process_click_event(normalize_compound_fields(CE));
process_click_event(CE) when is_map(CE) ->
    Action = to_binary(maps:get(action, CE, <<"">>)),
    M = #{action => Action},
    case Action of
        <<"open_url">> -> M#{url => to_binary(maps:get(url, CE, <<"">>))};
        <<"open_file">> -> M#{path => to_binary(maps:get(path, CE, <<"">>))};
        <<"run_command">> -> M#{command => to_binary(maps:get(command, CE, <<"">>))};
        <<"suggest_command">> -> M#{command => to_binary(maps:get(command, CE, <<"">>))};
        <<"change_page">> -> M#{page => to_integer(maps:get(page, CE, 1))};
        <<"copy_to_clipboard">> -> M#{value => to_binary(maps:get(value, CE, <<"">>))};
        <<"show_dialog">> -> M#{dialog => maps:get(dialog, CE, <<"">>)};
        <<"custom">> ->
            M1 = M#{id => to_binary(maps:get(id, CE, <<"">>))},
            case CE of
		#{payload := P} ->
		    M1#{payload => to_binary(P)};
		#{} ->
		    M1
	    end;
        _ -> CE
    end.

process_hover_event(HE) when is_list(HE) ->
    process_hover_event(normalize_compound_fields(HE));
process_hover_event(HE) when is_map(HE) ->
    Action = to_binary(maps:get(action, HE, <<"">>)),
    M = #{action => Action},
    case Action of
        <<"show_text">> ->
            Val = maps:get(value, HE, <<"">>),
            M#{value => normalize(Val)};
        <<"show_item">> ->
            M1 = M#{id => to_binary(maps:get(id, HE, <<"minecraft:air">>))},
            M2 = case HE of
		     #{count := C} ->
			 M1#{count => to_integer(C)};
		     #{} ->
			 M1
		 end,
            case HE of
                #{components := Comp} ->
                    M2#{components => Comp};
                #{} ->
                    M2
            end;
        <<"show_entity">> ->
            M1 = M#{id => to_binary(maps:get(id, HE, <<"">>))},
            M2 = case HE of
		     #{name := NameComp} ->
			 M1#{name => normalize(NameComp)};
		     #{} ->
			 M1
		 end,
            case HE of
                #{uuid := UUID} ->
                    M2#{uuid => UUID};
                #{} ->
                    M2
            end;
        _ -> HE
    end.


process_extra(Map) ->
    case Map of
        #{extra := ExtraList} when is_list(ExtraList) ->
            Map#{extra => [normalize(Child) || Child <- ExtraList]};
        #{} ->
            Map
    end.


-spec to_nbt_tags(map()) -> [tuple()].
to_nbt_tags(Map) ->
    Fields = map_to_nbt_fields(Map),
    [{tag_compound, "", Fields}].

map_to_nbt_fields(Map) ->
    maps:fold(
      fun(K, V, Acc) ->
	      case encode_nbt_field(to_string(K), V) of
		  none -> Acc;
		  Field -> [Field | Acc]
	      end
      end,
      [],
      Map
     ).

encode_nbt_field(Key, Val) when is_binary(Val) ->
    {tag_string, Key, binary_to_list(Val)};
encode_nbt_field(Key, Val) when is_list(Val), is_integer(hd(Val)) ->
    {tag_string, Key, Val};
encode_nbt_field(Key, true) ->
    {tag_byte, Key, 1};
encode_nbt_field(Key, false) ->
    {tag_byte, Key, 0};
encode_nbt_field(Key, Val) when is_integer(Val) ->
    {tag_int, Key, Val};
encode_nbt_field(Key, Val) when is_float(Val) ->
    {tag_double, Key, Val};
encode_nbt_field(Key, List) when is_list(List) ->
    NbtItems = [map_to_nbt_item(Item) || Item <- List],
    {tag_list, Key, NbtItems};
encode_nbt_field(Key, Map) when is_map(Map) ->
    Fields = map_to_nbt_fields(Map),
    {tag_compound, Key, Fields};
encode_nbt_field(_Key, undefined) ->
    none.

map_to_nbt_item(Map) when is_map(Map) ->
    Fields = map_to_nbt_fields(Map),
    {tag_compound, Fields};
map_to_nbt_item(Str) when is_binary(Str); is_list(Str) ->
    {tag_string, binary_to_list(to_binary(Str))};
map_to_nbt_item(Item) ->
    {tag_string, binary_to_list(to_binary(Item))}.


is_string_or_binary(Data) when is_binary(Data) -> true;
is_string_or_binary(Data) when is_list(Data) ->
    case Data of
        [] -> true;
        [H | _] when is_integer(H) -> true;
        _ -> false
    end;
is_string_or_binary(_) -> false.

to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg -> erlang:apply(erlang, binary_to_atom, [B, utf8])
    end;
to_atom(L) when is_list(L) ->
    try list_to_existing_atom(L)
    catch error:badarg -> erlang:apply(erlang, list_to_atom, [L])
    end.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) ->
    case is_string_or_binary(L) of
        true  -> list_to_binary(L);
        false -> <<"">>
    end;
to_binary({tag_string, _Name, Val}) -> to_binary(Val);
to_binary({tag_string, Val}) -> to_binary(Val);
to_binary({tag_float, Val}) -> to_binary(Val);
to_binary({tag_double, Val}) -> to_binary(Val);
to_binary({tag_int, Val}) -> to_binary(Val);
to_binary({tag_byte, Val}) -> to_binary(Val);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F) -> float_to_binary(F, [{decimals, 4}, compact]).

to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(L) when is_list(L) -> L.

to_bool(true)  -> true;
to_bool(false) -> false;
to_bool(1)     -> true;
to_bool(0)     -> false;
to_bool(B) when is_binary(B) -> B =:= <<"true">>;
to_bool(_)     -> false.

to_integer(I) when is_integer(I) -> I;
to_integer(B) when is_binary(B) -> binary_to_integer(B);
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(_) -> 0.
