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

  @callback match(value, pattern) ::
              {:match, assignment} | {:non_match, list({value, pattern})}

  @spec parse_match(atom, String.t(), String.t()) :: any
  def parse_match(module, val, pat) do
    with {:ok, v} <- module.value(val),
         {:ok, p} <- module.pattern(pat),
         {:match, m} <- module.match(v, p) do
      {:ok, Enum.map(m, fn {var, info} -> {var, module.show(info)} end)}
    end
  end
end
