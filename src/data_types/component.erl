-module(component).

-export([
	 encode_component/2,
	 decode_component/2,
	 encode_dye_color/1,
	 decode_dye_color/1,
	 dye_colors/0,
	 encode_painting_variant/1,
	 decode_painting_variant/1
	]).

encode_component(TypeId, Data) when is_integer(TypeId) ->
    Name = component_type_registry:id_to_name(TypeId),
    encode(Name, Data);
encode_component(Name, Data) when is_atom(Name) ->
    encode(Name, Data).

encode(TypeId, Data) ->
    case TypeId of
	'minecraft:dye' ->
	    encode_dye_color(Data);
	'minecraft:wolf/collar' ->
	    encode_dye_color(Data);
	'minecraft:cat/collar' ->
	    encode_dye_color(Data);
	'minecraft:sheep/color' ->
	    encode_dye_color(Data);
	'minecraft:shulker/color' ->
	    encode_dye_color(Data);
	'minecraft:tropical_fish/base_color' ->
	    encode_dye_color(Data);
	'minecraft:tropical_fish/pattern_color' ->
	    encode_dye_color(Data);
	'minecraft:painting/variant' ->
	    encode_painting_variant(Data);
	_ ->
	    error({unimplemented_component_encoder, TypeId})
    end.




decode_component(TypeId, BinData) when is_integer(TypeId) ->
    Name = component_type_registry:id_to_name(TypeId),
    decode(Name, BinData);
decode_component(Name, BinData) when is_atom(Name) ->
    decode(Name, BinData).
decode(TypeId, Data) ->
    case TypeId of
        'minecraft:dye' ->
	    decode_dye_color(Data);
	'minecraft:wolf/collar' ->
	    decode_dye_color(Data);
	'minecraft:cat/collar' ->
	    decode_dye_color(Data);
	'minecraft:sheep/color' ->
	    decode_dye_color(Data);
	'minecraft:shulker/color' ->
	    decode_dye_color(Data);
	'minecraft:tropical_fish/base_color' ->
	    decode_dye_color(Data);
	'minecraft:tropical_fish/pattern_color' ->
	    decode_dye_color(Data);
	'minecraft:painting/variant' ->
	    decode_painting_variant(Data);
	_ ->
	    error({unimplemented_component_decoder, TypeId})
    end.




dye_colors() ->
    [white, orange, magenta, light_blue, yellow, lime, pink, gray,
     light_gray, cyan, purple, blue, brown, green, red, black].

encode_dye_color(Color) ->
    encode:encode_type(Color, {enum, varint, dye_colors()}).

decode_dye_color(BinData) ->
    decode:decode_type(BinData, {enum, varint, dye_colors()}).


encode_painting_variant(Id) when is_integer(Id) ->
    encode_painting_variant({id, Id});
encode_painting_variant({id, Id}) ->
    TagBin = encode:encode_type(0, varint),
    IdBin = encode:encode_type(Id, varint),
    <<TagBin/binary, IdBin/binary>>;
encode_painting_variant({inline, Map}) when is_map(Map) ->
    encode_painting_variant(Map);
encode_painting_variant(#{asset_id := AssetId, width := Width, height := Height, title := Title, author := Author}) ->
    TagBin = encode:encode_type(1, varint),
    AssetBin = encode:encode_type(AssetId, string),
    WidthBin = encode:encode_type(Width, varint),
    HeightBin = encode:encode_type(Height, varint),
    TitleBin = encode:encode_type(Title, string),
    AuthorBin = encode:encode_type(Author, string),
    <<TagBin/binary, AssetBin/binary, WidthBin/binary, HeightBin/binary, TitleBin/binary, AuthorBin/binary>>.

decode_painting_variant(Data) ->
    {Rest1, Tag} = decode:decode_type(Data, varint),
    case Tag of
        0 ->
            {Rest2, Id} = decode:decode_type(Rest1, varint),
            {Rest2, {id, Id}};
        1 ->
            {Rest2, AssetId} = decode:decode_type(Rest1, string),
            {Rest3, Width} = decode:decode_type(Rest2, varint),
            {Rest4, Height} = decode:decode_type(Rest3, varint),
            {Rest5, Title} = decode:decode_type(Rest4, string),
            {Rest6, Author} = decode:decode_type(Rest5, string),

            VariantMap = #{
			   asset_id => AssetId,
			   width => Width,
			   height => Height,
			   title => Title,
			   author => Author
			  },
            {Rest6, {inline, VariantMap}}
    end.


