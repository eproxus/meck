defmodule Meck.Mixfile do
  use Mix.Project

  def project do
    [
      app: :meck,
      version: "0.8.4",
      description: description,
      package: package,
      deps: [],
    ]
  end

  defp description do
    """
    A mocking framework for Erlang
    """
  end

  defp package do
    [
      files: [
        "Makefile",
        "rebar.config",
        "test.config",
        "src",
        "test/*.erl",
        "test/cover_test_module.dontcompile",
        "test/include",
        "README.md",
        "LICENSE",
        "CHANGELOG",
      ],
      contributors: [
        "Adam Lindberg",
      ],
      licenses: [
        "Apache 2.0",
      ],
      links: %{
        "GitHub" => "https://github.com/eproxus/meck",
      },
    ]
  end
end
