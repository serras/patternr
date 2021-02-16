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

  def variable(combinator \\ empty()) do
    combinator
    |> generic_name([?a..?z], "variable name")
    |> unwrap_and_tag(:variable)
    |> label("variable")
  end

  def build_both([v, e]), do: {v, e}

  def both(combinator \\ empty()) do
    combinator
    |> parsec(:element_)
    |> ignore(owhitespace() |> string("=") |> owhitespace())
    |> parsec(:element)
    |> reduce(:build_both)
    |> unwrap_and_tag(:both)
    |> label("both")
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
      ignore(string("}")) |> replace({:tuple, []}),
      # at least one element
      parsec(:element)
      |> repeat(
        ignore(owhitespace() |> string(",") |> owhitespace())
        |> parsec(:element)
      )
      |> ignore(owhitespace() |> string("}"))
      |> tag(:tuple)
    ])
    |> label("tuple")
  end

  def split_last(lst) do
    {all_but_last, [last]} = Enum.split(lst, -1)
    {all_but_last, last}
  end

  def list(combinator \\ empty()) do
    combinator
    |> ignore(string("[") |> owhitespace())
    |> choice([
      # empty list
      ignore(string("]")) |> replace({:list, {[], nil}}),
      # at least one element
      parsec(:element)
      |> repeat(
        ignore(owhitespace() |> string(",") |> owhitespace())
        |> parsec(:element)
      )
      |> choice([
        ignore(owhitespace() |> string("]"))
        |> replace(nil),
        ignore(owhitespace() |> string("|") |> owhitespace())
        |> parsec(:element)
        |> ignore(owhitespace() |> string("]"))
      ])
      |> reduce(:split_last)
      |> unwrap_and_tag(:list)
    ])
    |> label("list")
  end

  def field(combinator \\ empty()) do
    combinator
    |> generic_name([?a..?z], "field name")
    |> ignore(owhitespace() |> string(":") |> owhitespace())
    |> parsec(:element)
    |> reduce(:build_both)
  end

  def fields(combinator \\ empty()) do
    choice(combinator, [
      field()
      |> repeat(
        ignore(owhitespace() |> string(",") |> owhitespace())
        |> field()
      ),
      owhitespace() |> replace([])
    ])
  end

  def separate([cstr, []]), do: {cstr, []}
  def separate([cstr | args]), do: {cstr, args}

  def map(combinator \\ empty()) do
    combinator
    |> ignore(string("%"))
    |> choice([
      generic_name([?A..?Z], "struct name")
      |> ignore(string("{")),
      ignore(string("{"))
      |> replace(nil)
    ])
    |> ignore(owhitespace())
    |> fields()
    |> ignore(owhitespace() |> string("}"))
    |> reduce(:separate)
    |> unwrap_and_tag(:map)
    |> label("map")
  end

  def atom(combinator \\ empty()) do
    combinator
    |> ignore(string(":"))
    |> generic_name([?A..?Z, ?a..?z], "atom name")
    |> unwrap_and_tag(:atom)
    |> label("atom")
  end

  def char(combinator \\ empty()) do
    combinator
    |> ignore(string("?"))
    |> utf8_char([])
    |> unwrap_and_tag(:integer)
    |> label("character")
  end

  def wrap_charlist([str]) do
    chrlst =
      str
      |> String.to_charlist()
      |> Enum.map(&{:integer, &1})

    {:list, {chrlst, nil}}
  end

  def charlist(_combinator \\ empty()) do
    string_with_quotes(?')
    |> reduce(:wrap_charlist)
  end

  def build_range([this, that]) do
    {"Range", [{"first", this}, {"last", that}]}
  end

  def range(combinator \\ empty()) do
    combinator
    |> parsec(:element_)
    |> ignore(owhitespace() |> string("..") |> owhitespace())
    |> parsec(:element_)
    |> reduce(:build_range)
    |> unwrap_and_tag(:map)
    |> label("range")
  end

  def element() do
    choice([
      both(),
      range(),
      element_()
    ])
  end

  def element_() do
    choice([
      parens(),
      tuple(),
      list(),
      map(),
      string_with_quotes(?") |> unwrap_and_tag(:string),
      charlist(),
      integer(min: 1) |> unwrap_and_tag(:integer),
      atom(),
      char(),
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
          | {:variable, String.t()}
          | {:both, {element, element}}
          | {:string, String.t()}
          | {:integer, integer}
          | {:atom, String.t()}
          | {:tuple, list(element)}
          | {:list, {list(element), element | nil}}
          | {:map, {String.t() | nil, list(field)}}
  @type field :: {String.t(), element}

  @spec vars(element) :: list(String.t())
  def vars({:wildcard, _}), do: []
  def vars({:variable, v}), do: [v]
  def vars({:both, {x, y}}), do: vars(x) ++ vars(y)
  def vars({:tuple, elts}), do: Enum.flat_map(elts, &vars/1)
  def vars({:list, {elts, nil}}), do: Enum.flat_map(elts, &vars/1)
  def vars({:list, {elts, xs}}), do: Enum.flat_map(elts, &vars/1) ++ vars(xs)
  def vars({:map, {_, elts}}), do: Enum.flat_map(elts, fn {_, v} -> vars(v) end)
  def vars(_), do: []

  ##  PARSER
  ##  ======
  defparsec(:element, element())
  defparsec(:element_, element_())

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
  @spec end_text() :: String.t()
  def end_text(), do: "end"

  @impl Patternr
  @spec help() :: list({String.t(), String.t()})
  def help() do
    [
      {"_", "Wilcard, matches everything leaving no trace"},
      {"x", "Variable, matches and remembers the value"},
      {"<pattern> = <pattern>", "Match both patterns, usually one is a variable"},
      {"1", "(Integer) number"},
      {"?a", "Character (actually an integer)"},
      {":hello", "Atom"},
      {"\"hello\"", "String, also known as binary"},
      {"'hello'", "Character (or integer) list"},
      {"%{name: <pattern>, ..}", "Map, may match only a subset of fields"},
      {"%Person{name: <pattern>, ..}", "Struct, may match only a subset of fields"},
      {"{<pattern>, ..}", "Tuple, matches the exact amount of elements"},
      {"[<pattern>, .. | <pattern>]",
       "List, matches some elements in the head and (optionally) the tail of the list"},
      {"<first> .. <last>", "Range, matches the bounds"}
    ]
  end

  @impl Patternr
  @spec show(value) :: String.t()
  def show({:wildcard, v}), do: v
  def show({:variable, v}), do: v
  def show({:both, {x, y}}), do: "#{show(x)} = #{show(y)}"
  def show({:tuple, lst}), do: show_several("{", "}", lst)

  def show({:list, {lst, nil}}) do
    if is_list_of_integers(lst) do
      intlist = Enum.map(lst, fn {:integer, x} -> x end)
      "'#{intlist}'"
    else
      show_several("[", "]", lst)
    end
  end

  def show({:list, {lst, xs}}), do: show_several("[", " | #{show(xs)}]", lst)
  def show({:string, s}), do: "\"#{String.replace(s, "\"", "\\\"")}\""
  def show({:integer, c}), do: "#{c}"
  def show({:atom, name}), do: ":#{name}"
  def show({:map, {nil, elts}}), do: "%#{show_several("{", "}", elts)}"

  def show({:map, {"Range", [{"first", this}, {"last", that}]}}),
    do: "#{show(this)}..#{show(that)}"

  def show({:map, {struct, elts}}), do: "%#{struct}#{show_several("{", "}", elts)}"
  # for record fields
  def show({k, v}), do: "#{k}: #{show(v)}"

  defp show_several(startc, endc, ps) do
    startc <> Enum.map_join(ps, ", ", &show/1) <> endc
  end

  defp is_list_of_integers(xs) do
    Enum.all?(xs, fn
      {:integer, _} -> true
      _ -> false
    end)
  end

  ##  MATCHER
  ##  =======
  @impl Patternr
  @spec match(value, pattern) ::
          {:match, Patternr.assignment()} | {:non_match, list({value, pattern})}
  # yay! in one go!
  def match(x, x), do: {:match, %{}}
  def match(v, {:variable, x}), do: {:match, %{x => v}}

  def match(v, {:both, {x, y}}),
    do: join_match(match(v, x), match(v, y))

  def match({:tuple, velts}, {:tuple, pelts})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  def match({:list, {velts, nil}}, {:list, {pelts, nil}})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  #  not the same length, cannot match
  def match(v = {:list, _}, p = {:list, {_, nil}}),
    do: {:non_match, [{v, p}]}

  def match(ts = {:list, _}, {:list, {[], xs}}),
    do: match(ts, xs)

  def match({:list, {[v | vs], vss}}, {:list, {[p | ps], pss}}),
    do: join_match(match(v, p), match({:list, {vs, vss}}, {:list, {ps, pss}}))

  def match(v = {:map, {_, _}}, p = {:map, {nil, _}}),
    do: match_map_args(v, p)

  def match(v = {:map, {s, _}}, p = {:map, {s, _}}),
    do: match_map_args(v, p)

  def match(v, p), do: {:non_match, [{v, p}]}

  def match_many(vs, ps) when length(vs) == length(ps) do
    Enum.zip(vs, ps)
    |> Enum.map(fn {v, p} -> match(v, p) end)
    |> join_matches()
  end

  def match_map_args(vrec = {:map, {_, vargs}}, prec = {:map, {_, pargs}}) do
    result =
      for {key, pat} <- pargs, reduce: {:match, %{}} do
        nil ->
          nil

        acc ->
          case Enum.find(vargs, fn {k, _} -> k == key end) do
            nil ->
              nil

            {_, val} ->
              this_match = match(val, pat)
              join_match(acc, this_match)
          end
      end

    if result == nil, do: {:non_match, [{vrec, prec}]}, else: result
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
