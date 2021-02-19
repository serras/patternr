defmodule Patternr do
  @moduledoc """
  Generic interface for a pattern matcher.
  """

  @type variable :: String.t()
  @type pattern :: any
  @type value :: any
  @type assignment :: %{variable => value}

  @callback pattern(String.t()) :: {:ok, pattern} | {:error, String.t()}
  @callback value(String.t()) :: {:ok, value} | {:error, String.t()}

  @callback show(value) :: String.t()
  @callback intro_text() :: {String.t(), String.t()}
  @callback end_text() :: String.t()
  @callback help() :: list({String.t(), String.t()})
  @callback examples() :: list({String.t(), String.t(), String.t(), list(String.t())})

  @callback match(value, pattern) ::
              {:match, assignment}
              | {:non_match, list({value, pattern})}
              | {:type_error, list({value, pattern})}

  @spec parse_match(atom, String.t(), String.t()) :: any
  def parse_match(module, val, pat) do
    with {:ok, v} <- module.value(val),
         {:ok, p} <- module.pattern(pat),
         m <- module.match(v, p) do
      case m do
        {:match, subst} ->
          showable_subst = Enum.map(subst, fn {var, info} -> {var, module.show(info)} end)
          {:match, showable_subst}

        {not_good, problems} ->
          showable_problems =
            Enum.map(problems, fn {one, other} -> {module.show(one), module.show(other)} end)

          {not_good, showable_problems}
      end
    end
  end
end
