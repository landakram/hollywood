# Hollywood

[Actors](https://en.wikipedia.org/wiki/Actor_model),
[gossip](https://en.wikipedia.org/wiki/Gossip_protocol), and eventual
homogeneity, just like the real thing.

    ~/D/c/hollywood ❯❯❯ iex -S mix                                                                                                                                                                         
    iex(1)> actors = Main.random_network(10000)
    iex(2)> Main.mood_breakdown(actors)
    100% of actors are happy.
    nil
    iex(3)> Enum.first(actors) |> Hollywood.Node.change_mood("worried about getting old and no longer being relevant")
    :ok
    iex(4)> Main.mood_breakdown(actors)
    0% of actors are worried about getting old and no longer being relevant.
    100% of actors are happy.
    nil
    iex(5)> Main.mood_breakdown(actors)
    2% of actors are worried about getting old and no longer being relevant.
    98% of actors are happy.
    nil
    iex(6)> Main.mood_breakdown(actors)
    31% of actors are worried about getting old and no longer being relevant.
    69% of actors are happy.
    nil
    iex(7)> Main.mood_breakdown(actors)
    85% of actors are worried about getting old and no longer being relevant.
    15% of actors are happy.
    nil
    iex(8)> Main.mood_breakdown(actors)
    98% of actors are worried about getting old and no longer being relevant.
    2% of actors are happy.
    nil
    iex(9)> Main.mood_breakdown(actors)
    100% of actors are worried about getting old and no longer being relevant.
    0% of actors are happy.
    nil
    