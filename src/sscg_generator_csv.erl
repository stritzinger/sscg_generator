-module(sscg_generator_csv).
% @doc A module for reading CSV files, including support for multiline rows.
% This module handles CSV entries where fields span multiple lines due to
% quoted content.

% API
-export([read/1, read/2]).

%--- Types ---------------------------------------------------------------------
-type file_path() :: binary(). 

%--- API -----------------------------------------------------------------------
-spec read(file_path()) -> [map()].
read(FilePath) ->
    read(FilePath, #{}).

% @doc
% Reads a CSV file with specified options and returns a list of maps. 
% Options can include: separator, the character used to separate fields 
% (default is comma). Malformed rows, such as those with missing or extra
% fields, are automatically skipped.
-spec read(binary(), map()) -> {ok, [map()]} | {error, term()}.
read(FilePath, Opts) ->
    case file:open(FilePath, [read]) of
        {ok, File} ->
            try
                {ok, parse_csv(File, Opts)}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            after
                file:close(File)
            end;
        {error, Reason} ->
            {error, {file_open_failed, Reason}}
    end.

%--- Internal Functions --------------------------------------------------------

% Reads the header line from an open CSV file and parses it into columns.
-spec parse_csv(FileHandle, Opts) -> Result
    when FileHandle :: file:io_device(),
         Opts       :: #{} | #{separator => $,},
         Result     :: [map()] 
                       | {error, end_of_file} 
                       | {error, {header_read_failed, binary()}}.
parse_csv(FileHandle, Options) ->
    case file:read_line(FileHandle) of
        {ok, HeaderLineWithNewline} ->
            HeaderLine = string:trim(HeaderLineWithNewline),
            FieldSeparator = maps:get(separator, Options, $,),
            % Check if the header line contains the separator
            case string:tokens(HeaderLine, [FieldSeparator]) of
                [SingleColumn] when SingleColumn =:= HeaderLine ->
                    ColumnHeaders = [SingleColumn]; % Only one column, no separator needed
                Headers ->
                    ColumnHeaders = Headers
            end,
            parse_rows(FileHandle, ColumnHeaders, FieldSeparator, []);
        eof ->
            {error, end_of_file};
        {error, Reason} ->
            {error, {header_read_failed, Reason}}
    end.

% Parses the rows of the CSV file, supporting multiline fields.
% Reads each line from the file and handles it appropriately.
parse_rows(FileHandle, ColumnHeaders, Separator, AccumulatedRows) ->
    case io:get_line(FileHandle, '') of
        eof -> 
            lists:reverse(AccumulatedRows);
        {error, Reason} -> 
            {error, {line_read_failed, Reason}};
        Line -> parse_and_handle_line(FileHandle, 
                                      Line, 
                                      ColumnHeaders, 
                                      Separator, 
                                      AccumulatedRows)
    end.

% Processes a CSV line, handling multiline fields if needed. Trims the line and
% checks for unclosed quotes. If quotes are unclosed, calls `process_multiline/5`
% to gather the complete row. Otherwise, splits and parses the line.
parse_and_handle_line(FileHandle, Line, Headers, Separator, Acc) ->
    TrimmedLine = string:trim(Line),
    case has_unclosed_quote(TrimmedLine) of
        true -> 
            process_multiline(FileHandle, TrimmedLine, Headers, Separator, Acc);
        false -> 
            Row = split_row_preserving_quotes(TrimmedLine, Separator),
            NewAcc = handle_row(Headers, Row, Acc),
            parse_rows(FileHandle, Headers, Separator, NewAcc)
    end.

% Processes a multiline entry until it is complete.
process_multiline(File, CurrentLine, Headers, Separator, Acc) ->
    case io:get_line(File, '') of
        eof -> 
            handle_row(Headers, split_row_preserving_quotes(CurrentLine, Separator), Acc);
        NextLine ->
            CombinedLine = CurrentLine ++ "\n" ++ NextLine,
            case has_unclosed_quote(CombinedLine) of
                true -> process_multiline(File, CombinedLine, Headers, Separator, Acc);
                false ->
                    Row = split_row_preserving_quotes(CombinedLine, Separator),
                    NewAcc = handle_row(Headers, Row, Acc),
                    parse_rows(File, Headers, Separator, NewAcc)
            end
    end.


% Splits a line into fields, preserving quoted fields to handle complex CSV structures.
split_row_preserving_quotes(Line, Separator) ->
    split_row_preserving_quotes(Line, Separator, [], [], false).

split_row_preserving_quotes([], _Separator, CurrentField, Acc, _InQuotes) ->
    lists:reverse([lists:reverse(CurrentField) | Acc]);

split_row_preserving_quotes([Char | Rest], Separator, CurrentField, Acc, InQuotes) ->
    case {Char, InQuotes} of
        % Opening quote: start a quoted section.
        {$", false} ->
            split_row_preserving_quotes(Rest, Separator, [Char | CurrentField], Acc, true);
        % Closing quote: end a quoted section.
        {$", true} ->
            split_row_preserving_quotes(Rest, Separator, [Char | CurrentField], Acc, false);
        % Separator outside quotes: finish current field and move to the next.
        {Separator, false} ->
            split_row_preserving_quotes(Rest, Separator, [], [lists:reverse(CurrentField) | Acc], false);
        % Default case: continue building the current field.
        _ ->
            split_row_preserving_quotes(Rest, Separator, [Char | CurrentField], Acc, InQuotes)
    end.

% Determines if a line has unclosed quotes by counting the number of quote
% characters. Returns `true` if the number of quotes is odd, indicating
% an unclosed quote.
has_unclosed_quote(Line) ->
    QuoteCount = lists:foldl(fun(Char, Count) ->
        if
            Char =:= $\" -> Count + 1;
            true -> Count
        end
    end, 0, Line),
    (QuoteCount rem 2) =/= 0.

% Handles a row by mapping it to headers and cleaning the values.
handle_row(Headers, Row, Acc) ->
    case length(Headers) =:= length(Row) of
        true ->
            RowMap = lists:zip(Headers, Row),
            CleanedRowMap = clean_map(maps:from_list(RowMap)),
            [CleanedRowMap | Acc];
        false ->
            Acc
    end.

% Cleans a map by trimming and casting values to integers where possible.
clean_map(Map) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        CleanValue = string:trim(Value),
        FinalValue = case catch list_to_integer(CleanValue) of
            Int when is_integer(Int) -> Int;
            _ -> CleanValue
        end,
        maps:put(Key, FinalValue, Acc)
    end, #{}, maps:to_list(Map)).