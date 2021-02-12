defmodule Patternr.Haskell.Parser do
  @moduledoc """
  Parser for Haskell patterns and values.
  The actual parser is defined in the
  module `Patternr.Haskell`.
  """

  import NimbleParsec
  import Patternr.Common

  def generic_name(combinator \\ empty(), initial_range, name) do
    combinator
    |> utf8_string([initial_range], 1)
    |> utf8_string([?A..?Z, ?a..?z, ?0..?9, ?_..?_], min: 0)
    |> reduce({Enum, :join, []})
    |> label(name)
  end

  def wildcard() do
    generic_name(?_..?_, "wildcard")
    |> unwrap_and_tag(:wildcard)
    |> label("wildcard")
  end

  def build_var([v]), do: {v, nil}
  def build_var([v, e]), do: {v, e}

  def variable(combinator \\ empty()) do
    combinator
    |> generic_name(?a..?z, "variable name")
    |> optional(ignore(string("@")) |> parsec(:element_))
    |> reduce(:build_var)
    |> unwrap_and_tag(:variable)
    |> label("variable")
  end

  def field(combinator \\ empty()) do
    combinator
    |> generic_name(?a..?z, "field name")
    |> ignore(owhitespace() |> string("=") |> owhitespace())
    |> parsec(:element)
    |> reduce(:build_var)
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

  def record(combinator \\ empty()) do
    combinator
    |> generic_name(?A..?Z, "constructor name")
    |> ignore(owhitespace() |> string("{") |> owhitespace())
    |> fields()
    |> ignore(owhitespace() |> string("}"))
    |> reduce(:separate)
    |> unwrap_and_tag(:record)
    |> label("constructor")
  end

  def separate([cstr | args]), do: {cstr, args}

  def single_constructor(combinator \\ empty()) do
    combinator
    |> generic_name(?A..?Z, "constructor name")
    |> reduce(:separate)
    |> unwrap_and_tag(:constructor)
    |> label("constructor without arguments")
  end

  def constructor(combinator \\ empty()) do
    combinator
    |> generic_name(?A..?Z, "constructor name")
    |> repeat(whitespace() |> parsec(:element_))
    |> reduce(:separate)
    |> unwrap_and_tag(:constructor)
    |> label("constructor")
  end

  def parens(combinator \\ empty()) do
    combinator
    |> ignore(string("(") |> owhitespace())
    |> parsec(:element)
    |> lookahead_not(owhitespace() |> string(","))
    |> ignore(owhitespace() |> string(")"))
  end

  def list(combinator \\ empty()) do
    combinator
    |> ignore(string("[") |> owhitespace())
    |> choice([
      # empty list
      ignore(string("]")) |> replace({:list, []}),
      # at least one element
      parsec(:element)
      |> repeat(
        ignore(owhitespace() |> string(",") |> owhitespace())
        |> parsec(:element)
      )
      |> ignore(owhitespace() |> string("]"))
      |> tag(:list)
    ])
    |> label("list literal")
  end

  def two_to_tuple([a, b]), do: {a, b}

  def cons(combinator \\ empty()) do
    combinator
    |> parsec(:element_)
    |> ignore(owhitespace() |> string(":") |> owhitespace())
    |> parsec(:element)
    |> reduce(:two_to_tuple)
    |> unwrap_and_tag(:cons)
    |> label("list cons")
  end

  def element_() do
    choice([
      parens(),
      #  tuple(),
      list(),
      single_constructor(),
      string_with_quotes(?") |> unwrap_and_tag(:string),
      string_with_quotes(?') |> unwrap_and_tag(:char),
      integer(min: 1) |> unwrap_and_tag(:integer),
      variable(),
      wildcard()
    ])
  end

  def element() do
    choice([
      cons(),
      record(),
      constructor(),
      element_()
    ])
  end
end

defmodule Patternr.Haskell do
  @moduledoc """
  Pattern matcher for Haskell.
  """

  @behaviour Patternr

  import NimbleParsec
  import Patternr.Haskell.Parser

  @type element ::
          {:wildcard, String.t()}
          | {:variable, {String.t(), element | nil}}
          | {:constructor, {String.t(), list(element)}}
          | {:tuple, list(element)}
          | {:list, list(element)}
          | {:cons, {element, element}}
          | {:record, {String.t(), list(field)}}
          | {:string, String.t()}
          | {:char, String.t()}
          | {:integer, integer}
  @type field :: {String.t(), element}

  @spec vars(element) :: list(String.t())
  def vars({:wildcard, _}), do: []
  def vars({:variable, {v, nil}}), do: [v]
  def vars({:variable, {v, t}}), do: [v | vars(t)]
  def vars({:cons, {x, xs}}), do: vars(x) ++ vars(xs)
  def vars({:constructor, {_c, elts}}), do: Enum.flat_map(elts, &vars/1)
  def vars({:tuple, elts}), do: Enum.flat_map(elts, &vars/1)
  def vars({:list, elts}), do: Enum.flat_map(elts, &vars/1)
  def vars({:record, {_, elts}}), do: Enum.flat_map(elts, fn {_, v} -> vars(v) end)
  def vars(_), do: []

  ##  PARSER
  ##  ======
  defparsec(:element_, element_())
  defparsec(:element, element())

  @type pattern :: element
  @type value :: element

  @impl Patternr
  @spec pattern(String.t()) :: {:ok, pattern} | {:error, String.t()}
  def pattern(str) do
    case Patternr.Haskell.element(str) do
      {:ok, [p], "", _, _, _} ->
        vars = vars(p)
        uniques = Enum.uniq(vars)

        if length(vars) == length(uniques) do
          {:ok, p}
        else
          {:error, "duplicate variables"}
        end

      {:ok, _, _, _, _, _} ->
        {:error, "unfinished string"}

      {:error, e, _, _, _, _} ->
        {:error, e}
    end
  end

  @impl Patternr
  @spec value(String.t()) :: {:ok, value} | {:error, String.t()}
  def value(str) do
    case Patternr.Haskell.element(str) do
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
  def intro_text(), do: {"case", "of"}

  @impl Patternr
  @spec show(value) :: String.t()
  def show({:wildcard, v}), do: v
  def show({:variable, {v, nil}}), do: v
  def show({:variable, {v, p}}), do: v <> "@" <> show_parens(p)
  def show({:constructor, {c, []}}), do: c
  def show({:constructor, {c, args}}), do: show_several_spaces(c <> " ", "", args)
  def show({:tuple, lst}), do: show_several("(", ")", lst)
  def show({:list, lst}), do: show_several("[", "]", lst)
  def show({:char, c}), do: "'#{c}'"
  def show({:integer, c}), do: "#{c}"
  def show({:string, s}), do: "\"#{String.replace(s, "\"", "\\\"")}\""
  def show({:cons, {x, xs}}), do: "#{show_parens(x)} : #{show(xs)}"
  def show({:record, {c, elts}}), do: "#{c} #{show_several("{ ", " }", elts)}"
  # for record fields
  def show({k, v}), do: "#{k} = #{show(v)}"

  defp show_parens(p) do
    r = show(p)

    has_spaces = String.contains?(r, " ")
    starts_with_bracket = String.starts_with?(r, ["[", "(", "\"", "'"])

    if has_spaces and not starts_with_bracket do
      "(" <> r <> ")"
    else
      r
    end
  end

  defp show_several(startc, endc, ps) do
    startc <> Enum.map_join(ps, ", ", &show/1) <> endc
  end

  defp show_several_spaces(startc, endc, ps) do
    startc <> Enum.map_join(ps, " ", &show_parens/1) <> endc
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

  def match({:constructor, {c, vargs}}, {:constructor, {c, pargs}})
      when length(vargs) == length(pargs),
      do: match_many(vargs, pargs)

  # Special case: Foo {} matches constructor Foo with any args
  def match({:constructor, {c, _}}, {:record, {c, []}}), do: {:match, %{}}

  def match(vrec = {:record, {c, vargs}}, prec = {:record, {c, pargs}}) do
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

  def match({:tuple, velts}, {:tuple, pelts})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  # list, cons, string, oh my!
  def match({:list, velts}, {:list, pelts})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  def match({:cons, {v, vs}}, {:cons, {p, ps}}),
    do: join_match(match(v, p), match(vs, ps))

  def match({:list, [v | vs]}, {:cons, {p, ps}}),
    do: join_match(match(v, p), match({:list, vs}, ps))

  def match({:cons, {v, vs}}, {:list, [p | ps]}),
    do: join_match(match(v, p), match(vs, {:list, ps}))

  def match({:string, ""}, {:list, []}), do: {:match, %{}}
  def match({:list, []}, {:string, ""}), do: {:match, %{}}

  def match(v = {:string, str}, p = {:list, [px | pxs]}) do
    if String.length(str) > 0 do
      {s, ss} = String.split_at(str, 1)
      join_match(match({:char, s}, px), match({:string, ss}, {:list, pxs}))
    else
      {:non_match, [{v, p}]}
    end
  end

  def match(v = {:string, str}, p = {:cons, {px, pxs}}) do
    if String.length(str) > 0 do
      {s, ss} = String.split_at(str, 1)
      join_match(match({:char, s}, px), match({:string, ss}, pxs))
    else
      {:non_match, [{v, p}]}
    end
  end

  def match(v = {:list, [vx | vxs]}, p = {:string, str}) do
    if String.length(str) > 0 do
      {s, ss} = String.split_at(str, 1)
      join_match(match(vx, {:char, s}), match({:list, vxs}, {:string, ss}))
    else
      {:non_match, [{v, p}]}
    end
  end

  def match(v = {:cons, {vx, vxs}}, p = {:string, str}) do
    if String.length(str) > 0 do
      {s, ss} = String.split_at(str, 1)
      join_match(match(vx, {:char, s}), match(vxs, {:string, ss}))
    else
      {:non_match, [{v, p}]}
    end
  end

  def match(v, p), do: {:non_match, [{v, p}]}

  def match_many(vs, ps) when length(vs) == length(ps) do
    Enum.zip(vs, ps)
    |> Enum.map(fn {v, p} -> match(v, p) end)
    |> join_matches()
  end

  def join_matches(matches) do
    Enum.reduce(matches, {:match, %{}}, &join_match/2)
  end

  def join_match({:match, x}, {:match, y}), do: {:match, Map.merge(x, y)}
  def join_match({:match, _}, {:non_match, y}), do: {:non_match, y}
  def join_match({:non_match, x}, {:match, _}), do: {:non_match, x}
  def join_match({:non_match, x}, {:non_match, y}), do: {:non_match, x ++ y}
end
