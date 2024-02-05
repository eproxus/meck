defmodule MatchSpecTest do
  use ExUnit.Case
  doctest MatchSpec

  test "greets the world" do
    assert MatchSpec.hello() == :world
  end
end
