defmodule Patternr.Haskell.Parser do
  @moduledoc """
  Parser for Haskell patterns and values.
  The actual parser is defined in the
  module `Patternr.Haskell`.
  """

  import NimbleParsec

  def whitespace(combinator \\ empty()) do
    ignore(combinator, utf8_string([?\s], min: 1))
  end

  def generic_name(initial_range, name) do
    utf8_string([initial_range], 1)
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

  def variable() do
    generic_name(?a..?z, "variable name")
    |> optional(ignore(string("@")) |> parsec(:element_))
    |> reduce(:build_var)
    |> unwrap_and_tag(:variable)
    |> label("variable")
  end

  def separate([cstr | args]), do: {cstr, args}

  def single_constructor() do
    generic_name(?A..?Z, "constructor name")
    |> reduce(:separate)
    |> unwrap_and_tag(:constructor)
    |> label("constructor without arguments")
  end

  def constructor() do
    generic_name(?A..?Z, "constructor name")
    |> repeat(whitespace() |> parsec(:element))
    |> reduce(:separate)
    |> unwrap_and_tag(:constructor)
    |> label("constructor")
  end

  def parens() do
    ignore(string("(") |> optional(whitespace()))
    |> parsec(:element)
    |> lookahead_not(optional(whitespace()) |> string(","))
    |> ignore(optional(whitespace()) |> string(")"))
  end

  def list() do
    ignore(string("[") |> optional(whitespace()))
    |> choice([
      # empty list
      ignore(string("]")) |> replace({:list, []}),
      # at least one element
      parsec(:element)
      |> repeat(
        ignore(optional(whitespace()) |> string(",") |> optional(whitespace()))
        |> parsec(:element)
      )
      |> optional(whitespace())
      |> ignore(string("]"))
      |> tag(:list)
    ])
    |> label("list literal")
  end

  def two_to_tuple([a, b]), do: {a, b}

  def cons() do
    parsec(:element_)
    |> ignore(optional(whitespace()) |> string(":") |> optional(whitespace()))
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
      Patternr.Common.string_with_quotes(?") |> unwrap_and_tag(:string),
      Patternr.Common.string_with_quotes(?') |> unwrap_and_tag(:char),
      integer(min: 1) |> unwrap_and_tag(:integer),
      variable(),
      wildcard()
    ])
  end

  def element() do
    choice([
      cons(),
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
          | {:record, {String.t(), list(field), boolean}}
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

  def match({:tuple, velts}, {:tuple, pelts})
      when length(velts) == length(pelts),
      do: match_many(velts, pelts)

  # list, cons, string, oh my!
  def match({:list, velts}, {:list, pelts}),
    do: match_many(velts, pelts)

  def match({:cons, {v, vs}}, {:cons, {p, ps}}),
    do: join_match(match(v, p), match(vs, ps))

  def match({:list, [v | vs]}, {:cons, {p, ps}}),
    do: join_match(match(v, p), match(vs, ps))

  def match({:cons, {v, vs}}, {:list, [p | ps]}),
    do: join_match(match(v, p), match(vs, ps))

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
