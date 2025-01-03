% config_name(+GameConfig, +Player, -Name)
config_name(game_config(_, player_info(Name, _), _), white, Name).
config_name(game_config(_, _, player_info(Name, _)), black, Name).

% config_difficulty(+GameConfig, +Player, -Difficulty)
config_difficulty(game_config(_, player_info(_, Difficulty), _), white, Difficulty).
config_difficulty(game_config(_, _, player_info(_, Difficulty)), black, Difficulty).
