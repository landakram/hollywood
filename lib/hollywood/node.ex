defmodule Hollywood.Node do
  use GenServer.Behaviour

  @gossip_interval 500

  defrecord State, friends: [], mood: "happy", date: elem(:os.timestamp, 1)
  
  def start_link(state) do
    :gen_server.start_link({ :local, :node}, __MODULE__, state, [])
  end

  def init(state) do
    pid = self
    spawn_link fn -> 
      gossip_loop(pid)
    end
    {:ok, state}
  end

  defp gossip_loop(pid) do
    :timer.sleep @gossip_interval
    :gen_server.cast(pid, :gossip)
    gossip_loop pid
  end

  def handle_call(:mood, _from, state) do
    {:reply, {state.mood, state.date}, state}
  end

  def handle_cast({:add_friend, pid}, State[friends: friends, mood: mood]) do
    {:noreply, State[friends: [pid|friends], mood: mood]}
  end

  def handle_cast({:change_mood, mood, date}, state) do
    # Change mood and start gossipping friends
    if mood != state.mood and date > state.date do
      state = State[friends: state.friends, mood: mood, date: date]
      pid = self
    end
    {:noreply, state}
  end

  def handle_cast(:gossip, state) do
    case state.friends |> Enum.shuffle |> Enum.first do
      nil -> {:noreply, state}
      friend -> 
        :gen_server.cast(friend, {:change_mood, state.mood, state.date})
        {:noreply, state}
    end
  end

  def mood(node) do
    node |> :gen_server.call(:mood) |> elem(0)
  end

  def change_mood(node, mood) do
    node |> :gen_server.cast({:change_mood, mood, elem(:os.timestamp, 1)})
  end
end
