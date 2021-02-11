defmodule Patternr.Common do
  @moduledoc """
  Common utilities for several matchers,
  like pieces for parsers.
  """

  import NimbleParsec

  def whitespace(combinator \\ empty()) do
    ignore(combinator, utf8_string([?\s], min: 1))
  end

  def owhitespace(combinator \\ empty()) do
    optional(combinator, whitespace())
  end

  # copied from the docs
  def string_with_quotes(quote_char) do
    ignore(ascii_char([quote_char]))
    |> repeat(
      lookahead_not(ascii_char([quote_char]))
      |> choice([
        ~S(\") |> string() |> replace(?"),
        utf8_char([])
      ])
    )
    |> ignore(ascii_char([quote_char]))
    |> reduce({List, :to_string, []})
  end
end
