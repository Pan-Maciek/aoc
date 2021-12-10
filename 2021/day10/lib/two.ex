defmodule Two do

  def validate([?) | xs], [?( | stack]), do: validate(xs, stack)
  def validate([?] | xs], [?[ | stack]), do: validate(xs, stack)
  def validate([?} | xs], [?{ | stack]), do: validate(xs, stack)
  def validate([?> | xs], [?< | stack]), do: validate(xs, stack)

  def validate([c | _], _) when c in [?),?],?},?>], do: {:corrupted, c}
  def validate([c | xs], stack) when c in [?(,?[,?{,?<], do: validate(xs, [c | stack])

  def validate([], []), do: {:ok, []}
  def validate([], stack), do: {:unbalanced, stack}

  def validate(input), do: validate(input, [])

  def score([], score), do: score
  def score([?( | xs], score), do: score(xs, score * 5 + 1)
  def score([?[ | xs], score), do: score(xs, score * 5 + 2)
  def score([?{ | xs], score), do: score(xs, score * 5 + 3)
  def score([?< | xs], score), do: score(xs, score * 5 + 4)
  def score(input), do: score(input, 0)

  def main([fileName]) do
    scores = File.stream!(fileName) 
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.to_charlist/1)
    |> Stream.map(&Two.validate/1)
    |> Stream.filter(fn {status, _} -> status == :unbalanced end)
    |> Stream.map(fn {_, stack} -> Two.score stack end)
    |> Enum.to_list
    |> Enum.sort

    middle_score = Enum.at(scores, floor((length scores)/ 2))
    IO.puts "#{middle_score}"
  end

  def main(_), do: IO.puts "Usage: ./two.out <input>"
end
