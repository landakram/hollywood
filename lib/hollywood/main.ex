defmodule Main do
  def random_network(count) do
    nodes = generate_nodes(count) 
    randomize_connections(nodes)
    nodes
  end

  def generate_nodes(count) do
    1..count |> Enum.map fn(_) -> 
      {:ok, pid} = :gen_server.start_link(Hollywood.Node, Hollywood.Node.State.new, [])
      pid
    end
  end

  def mood_breakdown(nodes) do
    size = Enum.count nodes
    Enum.map(nodes, &Hollywood.Node.mood(&1)) |> do_mood_breakdown(size)
  end
  defp do_mood_breakdown([], _) do 
  end
  defp do_mood_breakdown(moods, size) do
    mood = Enum.first moods
    {match, others} = moods |> Enum.partition fn(m) -> m == mood end
    IO.puts "#{Enum.count(match) / size * 100 |> round}% of actors are #{mood}."
    do_mood_breakdown(others, size)
  end

  def moods(nodes) do
    Enum.map nodes, fn(node) ->
      IO.puts "#{inspect node} is #{Hollywood.Node.mood(node)}."
    end
  end

  def randomize_connections(nodes) do
    pid = self

    # Parallel map to bind nodes together
    nodes |> Enum.map(fn(node) ->
      spawn_link fn -> 
        :random.seed(:erlang.now)
        number_of_friends = 10
        Enum.shuffle(nodes) |> Enum.take(number_of_friends) |> Enum.map fn(friend) -> 
          if node != friend do
            :gen_server.cast(node, {:add_friend, friend})
          end
        end

        pid <- :ok
      end
    end) |> Enum.map (fn(_pid) -> 
      receive do :ok -> :ok end
    end)
  end
end
