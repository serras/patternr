defmodule Patternr.Elixir.Parser do
  @moduledoc """
  Parser for Elixir patterns and values.
  The actual parser is defined in the
  module `Patternr.Elixir`.
  """

  import NimbleParsec
  import Patternr.Common

  def generic_name(combinator \\ empty(), initial_ranges, name) do
    combinator
    |> utf8_string(initial_ranges, 1)
    |> utf8_string([?A..?Z, ?a..?z, ?0..?9, ?_..?_], min: 0)
    |> reduce({Enum, :join, []})
    |> label(name)
  end

  def wildcard() do
    generic_name([?_..?_], "wildcard")
    |> unwrap_and_tag(:wildcard)
    |> label("wildcard")
  end

  def build_var([v]), do: {v, nil}
  def build_var([v, e]), do: {v, e}

  def variable(combinator \\ empty()) do
    combinator
    |> generic_name([?A..?Z, ?a..?z], "variable name")
    |> optional(ignore(owhitespace() |> string("=") |> owhitespace()) |> parsec(:element))
    |> reduce(:build_var)
    |> unwrap_and_tag(:variable)
    |> label("variable")
  end

  def parens(combinator \\ empty()) do
    combinator
    |> ignore(string("(") |> owhitespace())
    |> parsec(:element)
    |> lookahead_not(owhitespace() |> string(","))
    |> ignore(owhitespace() |> string(")"))
  end

  def tuple(combinator \\ empty()) do
    combinator
    |> ignore(string("{") |> owhitespace())
    |> choice([
      # empty list
      ignore(string("}")) |> replace({:list, []}),
      # at least one element
      parsec(:element)
      |> repeat(
        ignore(owhitespace() |> string(",") |> owhitespace())
        |> parsec(:element)
      )
      |> owhitespace()
      |> ignore(string("}"))
      |> tag(:tuple)
    ])
    |> label("tuple")
  end

  def element() do
    choice([
      parens(),
      tuple(),
      # list(),
      string_with_quotes(?") |> unwrap_and_tag(:string),
      string_with_quotes(?') |> unwrap_and_tag(:charlist),
      integer(min: 1) |> unwrap_and_tag(:integer),
      variable(),
      wildcard()
    ])
  end
end

defmodule Patternr.Elixir do
  @moduledoc """
  Pattern matcher for Elixir.
  """

  @behaviour Patternr

  import NimbleParsec
  import Patternr.Elixir.Parser

  @type element ::
          {:wildcard, String.t()}
          | {:variable, {String.t(), element | nil}}
          | {:tuple, list(element)}
          | {:string, String.t()}
          | {:charlist, String.t()}
          | {:integer, integer}
  @type field :: {String.t(), element}

  @spec vars(element) :: list(String.t())
  def vars({:wildcard, _}), do: []
  def vars({:variable, {v, nil}}), do: [v]
  def vars({:variable, {v, t}}), do: [v | vars(t)]
  def vars({:tuple, elts}), do: Enum.flat_map(elts, &vars/1)
  def vars(_), do: []

  ##  PARSER
  ##  ======
  defparsec(:element, element())

  @type pattern :: element
  @type value :: element

  @impl Patternr
  @spec pattern(String.t()) :: {:ok, pattern} | {:error, String.t()}
  def pattern(str) do
    case Patternr.Elixir.element(str) do
      {:ok, [p], "", _, _, _} ->
        {:ok, p}

      {:ok, _, _, _, _, _} ->
        {:error, "unfinished string"}

      {:error, e, _, _, _, _} ->
        {:error, e}
    end
  end

  @impl Patternr
  @spec value(String.t()) :: {:ok, value} | {:error, String.t()}
  def value(str) do
    case Patternr.Elixir.element(str) do
      {:ok, [p], "", _, _, _} ->
        vars = vars(p)

        if Enum.empty?(vars(p)) do
          {:ok, p}
        else
          {:error, "found variables: #{Enum.join(vars, ", ")}"}
        end

      {:ok, _, _, _, _, _} ->
        {:error, "unfinished string"}

      {:error, e, _, _, _, _} ->
        {:error, e}
    end
  end

  ##  PRINTER
  ## =======
  @impl Patternr
  @spec intro_text() :: {String.t(), String.t()}
  def intro_text(), do: {"case", "do"}

  @impl Patternr
  @spec show(value) :: String.t()
  def show({:wildcard, v}), do: v
  def show({:variable, {v, nil}}), do: v
  def show({:variable, {v, p}}), do: v <> " = " <> show(p)
  def show({:tuple, lst}), do: show_several("{", "}", lst)
  def show({:string, s}), do: "\"#{String.replace(s, "\"", "\\\"")}\""
  def show({:charlist, c}), do: "'#{c}'"
  def show({:integer, c}), do: "#{c}"
  # for record fields
  def show({k, v}), do: "#{k} = #{show(v)}"

  defp show_several(startc, endc, ps) do
    startc <> Enum.map_join(ps, ", ", &show/1) <> endc
  end

  ##  MATCHER
  ##  =======
  @impl Patternr
  @spec match(value, pattern) ::
          {:match, Patternr.assignment()} | {:non_match, list({value, pattern})}
  # yay! in one go!
  def match(x, x), do: {:match, %{}}
  def match(v, {:variable, {x, nil}}), do: {:match, %{x => v}}

  def match(v, {:variable, {x, inner}}),
    do: join_match({:match, %{x => v}}, match(v, inner))

  def match({:tuple, velts}, {:tuple, pelts})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  def match(v, p), do: {:non_match, [{v, p}]}

  def match_many(vs, ps) when length(vs) == length(ps) do
    Enum.zip(vs, ps)
    |> Enum.map(fn {v, p} -> match(v, p) end)
    |> join_matches()
  end

  def join_matches(matches) do
    Enum.reduce(matches, {:match, %{}}, &join_match/2)
  end

  def join_match({:match, x}, {:match, y}) do
    {in_x_and_y_from_x, _only_in_x} = Map.split(x, Map.keys(y))
    {in_x_and_y_from_y, _only_in_y} = Map.split(y, Map.keys(x))

    if Map.equal?(in_x_and_y_from_x, in_x_and_y_from_y) do
      {:match, Map.merge(x, y)}
    else
      {:non_match, []}
    end
  end

  def join_match({:match, _}, {:non_match, y}), do: {:non_match, y}
  def join_match({:non_match, x}, {:match, _}), do: {:non_match, x}
  def join_match({:non_match, x}, {:non_match, y}), do: {:non_match, x ++ y}

  def is_equal_or_nil(_, nil), do: true
  def is_equal_or_nil(x, x), do: true
  def is_equal_or_nil(_, _), do: false
end
