defmodule Cucumberl.Mixfile do
  use Mix.Project

  def project do
    [app: :cucumberl,
     version: "0.0.7",
     description: description,
     package: package,
     deps: deps]
  end

  def application do
    []
  end

  defp deps do
    []
  end

  defp description do
  """
   A pure-erlang, open-source, implementation of Cucumber
   (http://cukes.info).  This provides a subset of the Cucumber feature
   definition language.
  """
  end

  defp package do
    [# These are the default files included in the package
      name: :cucumberl,
      files: ["src", "include", "rebar.config", "mix.exs", "README*", "COPYING*"],
      maintainers: ["Eric Merritt"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/ericbmerritt/cucumberl"}]
  end
end
