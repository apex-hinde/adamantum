-include("src/data_types/chunk_records.hrl").


-record(db_chunks, {
    coordinates :: tuple(),
    chunk_column :: #chunk_column{}
}).