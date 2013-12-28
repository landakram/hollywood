defmodule Main do
  @number_of_friends 10

  @doc"""
  Generate a network of nodes where each node is connected 
  to 10 random friends.
  """
  def random_network(count) do
    nodes = generate_nodes(count) 
    randomize_connections(nodes)
    nodes
  end

  @doc"""
  Generate `count` nodes.
  """
  def generate_nodes(count) do
    1..count |> Enum.map fn(_) -> 
      {:ok, pid} = :gen_server.start_link(Hollywood.Node, Hollywood.Node.State.new, [])
      pid
    end
  end

  @doc"""
  Create 10 random connections for each node in parallel.
  """
  def randomize_connections(nodes) do
    pid = self
    # Parallel map to bind nodes together
    nodes |> Enum.map(fn(node) ->
      spawn_link fn -> 
        # This was a doozy! Not seeding here causes shuffle to return the 
        # *same* list every time, since it is being called in a new process
        # and seeded from the same value.
        :random.seed(:erlang.now) 
        Enum.shuffle(nodes) |> Enum.take(@number_of_friends) |> Enum.map fn(friend) -> 
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

  @doc"""
  Print a percentage breakdown of the "moods" of all nodes.
  """
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
end
