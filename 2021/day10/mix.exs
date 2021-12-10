defmodule Two.MixProject do
  use Mix.Project

  def project do
    [
      app: :"two.out",
      version: "0.1.0",
      escript: [main_module: Two],
      lib_path: __DIR__
    ]
  end

end
